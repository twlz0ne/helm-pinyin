;;; helm-pinyin.el --- Pinyin support for helm -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/10/07
;; Version: 0.3.0
;; Last-Updated: 2023-05-26 12:45:20 +0800
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
;; │ (turn-on-helm-pinyin)
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

(defun helm-pinyin-call-original (adviced-fn &rest args)
  "Call the original function of ADVICED-FN with ARGS."
  (apply (advice--cdr (symbol-function adviced-fn)) args))

(defun helm-pinyin--advice-fuzzy-default-highlight-match (orig-fn candidate &rest rest)
  "Advice to highlight chinese characters matched by pinyin."
  (if-let ((candtmp (if (consp candidate) (car candidate) candidate))
           (candpy (assoc-default (if helm-pinyin-buffers-source-p
                                      (string-trim-right candtmp)
                                    candtmp)
                                  helm-pinyin-matched-candidate-alist)))
      (progn
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

(defun helm-pinyin--advice-collect-matches (_source-list)
  "Advice to do some initialization before `helm--collect-matches'."
  (setq helm-pinyin-matched-candidate-alist nil))

(defun helm-pinyin--advice-compute-matches (source)
  "Advice to do some initialization before `helm-compute-matches'."
  (setq helm-pinyin-buffers-source-p
        (string= "Buffers" (assoc-default 'name source)))
  (setq helm-pinyin-original-match-functions
        (helm-pinyin-call-original 'helm-match-functions source)))

(defun turn-on-helm-pinyin ()
  (interactive)
  (advice-add 'helm-match-functions
              :filter-return #'helm-pinyin--advice-match-functions)
  (advice-add 'helm-fuzzy-default-highlight-match
              :around #'helm-pinyin--advice-fuzzy-default-highlight-match)
  (advice-add 'helm--collect-matches
              :before #'helm-pinyin--advice-collect-matches)
  (advice-add 'helm-compute-matches
              :before #'helm-pinyin--advice-compute-matches))

(defun turn-off-helm-pinyin ()
  (interactive)
  (advice-remove 'helm-match-functions
                 #'helm-pinyin--advice-match-functions)
  (advice-remove 'helm-fuzzy-default-highlight-match
                 #'helm-pinyin--advice-fuzzy-default-highlight-match)
  (advice-remove 'helm--collect-matches
                 #'helm-pinyin--advice-collect-matches)
  (advice-remove 'helm-compute-matches
                 #'helm-pinyin--advice-compute-matches))



(provide 'helm-pinyin)

;;; helm-pinyin.el ends here
