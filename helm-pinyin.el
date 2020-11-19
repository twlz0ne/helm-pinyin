;;; helm-pinyin.el --- Pinyin support for helm -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/10/07
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/twlz0ne/helm-pinyin
;; Keywords: 

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

;;  0.1.0  2020/10/10  Initial version.

;;; Code:

(require 'cl-extra)
(require 'helm)
(require 'pinyinlib)

(defcustom helm-pinyin-pattern-max-length 8
  "Max lenght of pinyin pattern."
  :group 'helm-pinyin
  :type 'integer)


;;; functions

(defsubst helm-pinyin-find-files-p ()
  "Check if current source is find files."
  (string= "Find Files" (assoc-default 'name (helm-get-current-source))))

(defsubst helm-pinyin--mapconcat-pattern (pattern)
  "Transform pinyin string PATTERN in regexp for further fuzzy matching.

Like ‘helm--mapconcat-pattern’ but add pinyin match for the first
`helm-pinyin-pattern-max-length' characters of pattern.
E.g: helm.el$
    => \"[^z中]*[z中][^w文]*[w文]$\""
  (let ((ls (let ((l (split-string-and-unquote pattern "")))
              (if (> (length l) helm-pinyin-pattern-max-length)
                  (cons (cl-subseq l 0 helm-pinyin-pattern-max-length)
                        (cl-subseq l helm-pinyin-pattern-max-length))
                (cons l nil)))))
    (concat
     (mapconcat
      (lambda (c)
        (if (and (string= c "$")
                 (string-match "$\\'" pattern))
            c (let ((pinyin-pattern (pinyinlib-build-regexp-string c)))
                (if (< (length pinyin-pattern) 3)
                    c
                  (format "[^%s]*%s" (substring pinyin-pattern 1 -1) pinyin-pattern)))))
      (car ls) "")
     (mapconcat (lambda (c)
                  (if (and (string= c "$")
                           (string-match "$\\'" pattern))
                      c (regexp-quote c)))
                (cdr ls) ""))))

(defun helm-pinyin-mm-get-patterns (pattern)
  "Return a pattern string or a list of predicate/regexp cons.

FIXME: there isn't ‘helm-mm-3p-get-patterns’ function"
  (let ((fun (cl-ecase helm-mm-matching-method
               (multi1 #'helm-mm-1-get-pattern)
               (multi2 #'helm-mm-2-get-pattern)
               (multi3 #'helm-pinyin-mm-3-get-patterns)
               (multi3p nil)))) ; 
    (funcall fun pattern)))

(defun helm-pinyin-mm-3-get-patterns (pattern)
  "Return a list of predicate/regexp cons cells.

Like ‘helm-mm-3-get-patterns’ but but accpets a pinyin PATTERN."
  (unless (equal pattern helm-mm--3-pattern-str)
    (setq helm-mm--3-pattern-str pattern
          helm-mm--3-pattern-list
          (helm-pinyin-mm-3-get-patterns-internal pattern)))
  helm-mm--3-pattern-list)

(defun helm-pinyin-mm-3-get-patterns-internal (pattern)
  "Return a list of predicate/regexp cons cells.
E.g., ((identity . \"foo\") (not . \"bar\"))."
  (unless (string-empty-p pattern)
    (let ((patts (helm-mm-3-get-patterns-internal pattern)))
      (cond
       ((helm-pinyin-find-files-p) patts)
       (t (mapcar (pcase-lambda (`(,pred . ,patt))
                    (cons pred (helm-pinyin--mapconcat-pattern patt)))
                  patts))))))

(defun helm-pinyin-mm-match (candidate &optional parttern-or-patterns)
  "Call `helm-mm-matching-method' function against CANDIDATE.

FIXME: The PARTTERN-OR-PATTERNS can be a pattern string or a list of predicate/
regexp cons.  If ‘helm-mm-matching-method’ points to the wrong type, this function
will be failed."
  (let ((fun (cl-ecase helm-mm-matching-method
               (multi1 #'helm-mm-1-match)
               (multi2 #'helm-mm-2-match)
               (multi3 #'helm-pinyin-mm-3-match)
               (multi3p #'helm-mm-3p-match))))
    (funcall fun candidate parttern-or-patterns)))

(defun helm-pinyin-mm-3-match (candidate &optional patterns)
  "Check if PATTERNS match CANDIDATE.

Unlike ‘helm-mm-3-match’, this function accpets a list of predicate/regexp cons
instead of generate it from a string pattern."
  (if patterns
      (cl-loop for (predicate . regexp) in patterns
               always (funcall predicate
                               (condition-case _err
                                   ;; FIXME: Probably do nothing when
                                   ;; using fuzzy leaving the job
                                   ;; to the fuzzy fn.
                                   (string-match regexp candidate)
                                 (invalid-regexp nil))))
    t))


;;; advices

(defun helm-match-from-candidates!pinyin (cands matchfns match-part-fn limit source)
  "Override advice for ‘helm-match-from-candidates’.

Original:

   (loop ...
         (loop ...
               (generate-and-apply-patterns)))

Replaced with:

   (loop ...
         (generate-patterns)
         (loop ...
               (apply-patterns)))"
  (when cands ; nil in async sources.
    (condition-case-unless-debug err
        (cl-loop with hash = (make-hash-table :test 'equal)
                 with allow-dups = (assq 'allow-dups source)
                 with case-fold-search = (helm-set-case-fold-search)
                 with count = 0
                 for iter from 1
                 for fn in matchfns
                 for patts = (cl-case fn
                               (helm-mm-exact-match
                                (or (helm-mm-exact-get-pattern helm-pattern)
                                    helm-pattern))
                               (helm-pinyin-mm-match
                                (remove
                                 nil
                                 (let ((patts (helm-pinyin-mm-get-patterns helm-pattern)))
                                   (if (consp patts) patts
                                     (list patts)))))
                               (helm-mm-3-migemo-match
                                (when (executable-find "cmigemo")
                                  (or (helm-mm-migemo-get-pattern helm-pattern)
                                      helm-pattern))))
                 when (< count limit) nconc
                 (cl-loop for c in cands
                          for dup = (gethash c hash)
                          for disp = (helm-candidate-get-display c)
                          while (< count limit)
                          for target = (if (helm-attr 'match-on-real source)
                                           (or (cdr-safe c)
                                               (get-text-property 0 'helm-realvalue disp))
                                         disp)
                          for prop-part = (get-text-property 0 'match-part target)
                          for part = (and match-part-fn
                                          (or prop-part
                                              (funcall match-part-fn target)))
                          ;; When allowing dups check if DUP
                          ;; have been already found in previous loop
                          ;; by comparing its value with ITER.
                          when (and (or (and allow-dups dup (= dup iter))
                                        (null dup))
                                    (condition-case nil
                                        (if patts
                                            (funcall fn (or part target) patts)
                                          (funcall fn (or part target)))
                                      (invalid-regexp nil)))
                          do
                          (progn
                            ;; Give as value the iteration number of
                            ;; inner loop to be able to check if
                            ;; the duplicate have not been found in previous loop.
                            (puthash c iter hash)
                            (helm--maybe-process-filter-one-by-one-candidate c source)
                            (cl-incf count))
                          ;; Filter out nil candidates maybe returned by
                          ;; `helm--maybe-process-filter-one-by-one-candidate'.
                          and when c collect
                          (if (and part (not prop-part))
                              (if (consp c)
                                  (cons (propertize target 'match-part part) (cdr c))
                                (propertize c 'match-part part))
                            c)))
      (error (unless (eq (car err) 'invalid-regexp) ; Always ignore regexps errors.
               (helm-log-error "helm-match-from-candidates in source `%s': %s %s"
                               (assoc-default 'name source) (car err) (cdr err)))
             nil))))

(defun helm-ff--transform-pattern-for-completion@pinyin (orig-fn pattern)
  "Around advice for ‘helmhelm-ff--transform-pattern-for-completion’."
  (let* ((patts (split-string pattern " "))
         (basedir (or (helm-basedir (car patts)) "")))
    (mapconcat
     #'identity
     (remove-if
      (lambda (patt)
        (or (string-empty-p patt) (string= patt basedir)))
      (mapcar (lambda (patt)
                (let ((basename
                       (if (string-prefix-p basedir patt)
                           (substring patt (length basedir))
                         patt)))
                  (unless (string-empty-p basename)
                    (concat (regexp-quote basedir)
                            "\\("
                            (funcall orig-fn basename)
                            "\\|"
                            (helm-pinyin--mapconcat-pattern basename)
                            "\\)"))))
              patts))
     " ")))

(defun helm-find-files@pinyin (orig-fn arg)
  (let ((helm-mm-default-match-functions
         (remove 'helm-mm-match
                 `(,@helm-mm-default-match-functions helm-pinyin-mm-match))))
    (funcall orig-fn arg)))

(defun helm-mini@pinyin (orig-fn)
  (let ((helm-mm-default-match-functions
         (remove 'helm-mm-match
                 `(,@helm-mm-default-match-functions helm-pinyin-mm-match))))
    (funcall orig-fn)))



;;;###autoload
(defun turn-on-helm-pinyin ()
  (interactive)
  (advice-add 'helm-find-files :around #'helm-find-files@pinyin)
  (advice-add 'helm-mini :around #'helm-mini@pinyin)
  (advice-add 'helm-ff--transform-pattern-for-completion :around #'helm-ff--transform-pattern-for-completion@pinyin)
  (advice-add 'helm-match-from-candidates :override #'helm-match-from-candidates!pinyin))

;;;###autoload
(defun turn-off-helm-pinyin ()
  (interactive)
  (advice-remove 'helm-find-files #'helm-find-files@pinyin)
  (advice-remove 'helm-mini #'helm-mini@pinyin)
  (advice-remove 'helm-ff--transform-pattern-for-completion #'helm-ff--transform-pattern-for-completion@pinyin)
  (advice-remove 'helm-match-from-candidates #'helm-match-from-candidates!pinyin))

(provide 'helm-pinyin)

;;; helm-pinyin.el ends here
