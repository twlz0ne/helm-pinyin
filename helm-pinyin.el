;;; helm-pinyin.el --- Pinyin support for helm -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/10/07
;; Version: 0.3.0
;; Last-Updated: 2023-05-30 17:50:48 +0800
;;           by: Gong Qijian
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/twlz0ne/helm-pinyin
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Pinyin support for ‘helm-find-files’ and ‘helm-mini’.

;; Usage:
;; ┌────
;; │ (require 'helm-pinyin)
;; │ (helm-pinyin-mode 1)
;; │
;; │ ;; Apply pinyin matching on `completing-read`.
;; │ (add-to-list 'completion-styles-alist
;; │              (cons 'helm-pinyin (cdr (assq 'helm completion-styles-alist))))
;; │ (setq helm-completion-style 'helm-pinyin)
;; │
;; │ ;; (define-key helm-map (kbd "C-c ,")
;; │ ;;             (lambda ()
;; │ ;;               "Toggle helm-pinyin while helm window is activated."
;; │ ;;               (interactive)
;; │ ;;               (if helm-pinyin-mode
;; │ ;;                   (helm-pinyin-mode -1)
;; │ ;;                 (helm-pinyin-mode 1))
;; │ ;;               (helm-refresh)))
;; │ ```
;; └────

;; See README.md for more information.

;;; Change Log:

;;  0.3.0  2023/05/23  Refactor.
;;  0.2.0  2021/10/07  Fix invalid-regexp issue.
;;  0.1.0  2020/10/07  Initial version.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'helm)
(require 'pinyinlib)

(defvar helm-pinyin-original-match-functions nil
  "A list of original match function of current source.")

(defvar helm-pinyin-matched-candidate-alist nil
  "A list of ((candidate1 . pinyin1) (candidate2 . pinyin2) ...).")

(defvar helm-pinyin-buffers-source-p nil
  "Non-nil means current is the source \"Buffers\".")

(defvar helm-pinyin-input-pinyin-p nil
  "Non-nil means current match pattern contains Chinese character.")

(defalias 'helm-pinyin-text-properties
  (if (fboundp 'object-intervals)
      'object-intervals
    (lambda (object)
      (let ((props (cdar (read-from-string (substring (format "%S" object) 1))))
            intvls)
        (while props
          (push (list (pop props) (pop props) (pop props)) intvls))
        intvls)))
  "Return text properties of OBJECT.")

(defun helm-pinyin-convert-to-pinyin (candidate)
  "Convert chinese characters in CANDIDATE to pinyin."
  (let ((part (helm-basename candidate))
        (pynum 0))
    (cons
     (concat (helm-basedir candidate)
             (string-join
              (cl-loop for idx from 0 to (1- (length part))
                       collect
                         (let ((chr (char-to-string (aref part idx))))
                           (if (string-match "\\cc" chr)
                               (cl-loop for asc from ?a to ?z
                                        for hzs in pinyinlib--simplified-char-table
                                        if (string-match chr hzs)
                                        return (progn (cl-incf pynum) (char-to-string asc)))
                             chr)))))
     pynum)))

;; Borrowed from `undo-fu'.
(defmacro helm-pinyin--with-advice (fn-orig where fn-advice &rest body)
  "Execute BODY with advice added.

WHERE using FN-ADVICE temporarily added to FN-ORIG."
  (declare (indent 1))
  `(let ((fn-advice-var ,fn-advice))
     (unwind-protect
         (progn
           (advice-add ,fn-orig ,where fn-advice-var)
           ,@body)
       (advice-remove ,fn-orig fn-advice-var))))

(defun helm-pinyin-call-original (adviced-fn &rest args)
  "Call the original function of ADVICED-FN with ARGS."
  (apply (advice--cdr (symbol-function adviced-fn)) args))

(defun helm-pinyin--advice-set-default-prompt-display (fn source &optional _)
  "Advice to prepend pinyin indicator to header line of helm window."
   (helm-pinyin--with-advice 'force-mode-line-update
     :around (lambda (&rest args)
               (setq header-line-format
                     (concat helm-pinyin-header-line-indicator
                             header-line-format))
               (apply args))
     (funcall fn source 'force)))

(defun helm-pinyin--advice-fuzzy-default-highlight-match (orig-fn candidate &rest rest)
  "Advice to highlight chinese characters matched by pinyin."
  (if-let ((input-pinyin-p helm-pinyin-input-pinyin-p)
           (candtmp (if (consp candidate) (car candidate) candidate))
           (candpy (assoc-default (if helm-pinyin-buffers-source-p
                                      (string-trim-right candtmp)
                                    candtmp)
                                  helm-pinyin-matched-candidate-alist)))
      (progn
        (setq candtmp (substring candtmp)) ;; Use the copy to apply properties.
        (dolist (intvl (helm-pinyin-text-properties (apply orig-fn candpy rest)))
          (pcase intvl
            (`(,beg ,end ,props)
              (when (and (memq 'face props)
                         (memq 'helm-match props))
                (helm-add-face-text-properties beg end 'helm-match nil candtmp)))))
        (put-text-property 0 (length candpy) 'display candtmp candpy)
        (if (consp candidate)
            (cons candtmp (cdr candidate))
          candtmp))
    (apply orig-fn candidate rest)))

(defun helm-pinyin--initials-before-collecting (&rest _)
  "Initials before collecting."
  (setq helm-pinyin-matched-candidate-alist nil)
  (setq helm-pinyin-input-pinyin-p (not (string-match "\\cc" helm-pattern))))

(cl-defun helm-pinyin-mm-match (candidate &optional (pattern helm-pattern))
  "Call all match functions with pinyin of CANDIDATE."
  (let* ((pycand (helm-pinyin-convert-to-pinyin candidate))
         matched)
    (catch 'break
      (dolist (matchfn helm-pinyin-original-match-functions)
        (when (setq matched (funcall matchfn (car pycand)))
          (unless (zerop (cdr pycand))
            (push (cons candidate (car pycand)) helm-pinyin-matched-candidate-alist))
          (throw 'break nil))))
    matched))

(defun helm-pinyin--advice-match-functions (return)
  "Advice to pre-append pinyin match to the RETURN of `helm-match-functions'."
  (append '(helm-pinyin-mm-match) return))

(defvar helm-pinyin-enabled-sources '("Find Files" "Buffers" "Recentf"))

(defun helm-pinyin--advice-compute-matches (source)
  "Advice to do some initialization before `helm-compute-matches'."
  (setq helm-pinyin-buffers-source-p
        (string= "Buffers" (assoc-default 'name source)))
  (setq helm-pinyin-original-match-functions
        (helm-pinyin-call-original 'helm-match-functions source)))

;;; Advices for `helm-dynamic-completion'

(cl-defun helm-pinyin--advice-mm-match (orig-fn candidate
                                                &optional (pattern helm-pattern))
    "Advice around `helm-mm-match' (ORIG-FN) to filter CANDIDATE by pinyin."
   (let* ((pycand (helm-pinyin-convert-to-pinyin candidate))
          (matched (unless (zerop (cdr pycand))
                     (funcall orig-fn (car pycand) pattern))))
     (if matched
         (prog1 matched
           (push (cons candidate (car pycand))
                 helm-pinyin-matched-candidate-alist))
       (funcall orig-fn candidate pattern))))


;; helm-pinyin-mode

(defvar helm-pinyin-mode-line-indicator " HelmPy"
  "String to display in mode line when `helm-pinyin-mode' is activated.")

(defvar helm-pinyin-header-line-indicator "[Pinyin]"
  "String prepend to header line of helm window when `helm-pinyin-mode' is activated.")

(define-minor-mode helm-pinyin-mode
  "Toggle helm pinyin mode."
  :global t
  :group 'helm-pinyin
  :lighter helm-pinyin-mode-line-indicator
  :require 'helm-pinyin
  (if helm-pinyin-mode
      (progn
        (advice-add 'helm-match-functions
                    :filter-return #'helm-pinyin--advice-match-functions)
        (advice-add 'helm-fuzzy-default-highlight-match
                    :around #'helm-pinyin--advice-fuzzy-default-highlight-match)
        (advice-add 'helm--collect-matches
                    :before #'helm-pinyin--initials-before-collecting)
        (advice-add 'helm-compute-matches
                    :before #'helm-pinyin--advice-compute-matches)
        (advice-add 'helm-mm-match
                    :around #'helm-pinyin--advice-mm-match)
        (advice-add 'helm-completion--multi-all-completions-1
                    :before #'helm-pinyin--initials-before-collecting)
        (advice-add 'helm-display-mode-line
                    :around #'helm-pinyin--advice-set-default-prompt-display))
    (advice-remove 'helm-match-functions
                   #'helm-pinyin--advice-match-functions)
    (advice-remove 'helm-fuzzy-default-highlight-match
                   #'helm-pinyin--advice-fuzzy-default-highlight-match)
    (advice-remove 'helm--collect-matches
                   #'helm-pinyin--initials-before-collecting)
    (advice-remove 'helm-compute-matches
                   #'helm-pinyin--advice-compute-matches)
    (advice-remove 'helm-mm-match
                   #'helm-pinyin--advice-mm-match)
    (advice-remove 'helm-completion--multi-all-completions-1
                   #'helm-pinyin--initials-before-collecting)
    (advice-remove 'helm-display-mode-line
                   #'helm-pinyin--advice-set-default-prompt-display)))

(provide 'helm-pinyin)

;;; helm-pinyin.el ends here
