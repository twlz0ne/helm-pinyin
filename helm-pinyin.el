;;; helm-pinyin.el --- Pinyin support for helm -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/10/07
;; Version: 0.2.0
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

;;  0.2.0  2021/10/07  Fix invalid-regexp issue.
;;  0.1.0  2020/10/07  Initial version.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'helm)
(require 'pinyinlib)

(defvar helm-pinyin--reg-max (- (expt 2 15 ) (* 5 (expt 2 10)))
  "Max sieze of reg buffer in c.")

(defvar pinyin-char-lens-alist
  (append
   (cl-loop for it in pinyinlib--punctuation-alist
            collect (cons (char-to-string (car it)) (length (cdr it))))
   (cl-loop for ch from ?a to ?z
            collect (cons (char-to-string ch)
                          (length (pinyinlib-build-regexp-char ch))))))


;;; utils

(defvar helm-pinyin-debug-p nil)

(defvar helm-pinyin--shorten-pinyin-regexp-matcher
  (rx (seq (group (or "[" "[^")) (group (one-or-more (not (any "]")))) "]"))
  "Regexp to match the pinyin pattern.")

(defun helm-pinyin--shorten-pinyin-regexp (pinyin-pattern &optional len)
  "Return shortened pinyin regexp, e.g.: “[^a阿]*[a阿]”."
  (if (stringp pinyin-pattern)
      (with-temp-buffer
        (insert pinyin-pattern)
        (goto-char (point-min))
        (let (match1 match2)
          (while (re-search-forward
                  helm-pinyin--shorten-pinyin-regexp-matcher nil t 1)
            (setq match1 (match-string 1))
            (setq match2 (match-string 2))
            (delete-region (match-beginning 0) (match-end 0))
            (insert match1 (substring match2 0 (min (or len 2) (length match2))) "]"))
          (buffer-string)))
    (if (and (consp pinyin-pattern) (stringp (cdar pinyin-pattern)))
        (cl-loop for (predicate . regexp) in pinyin-pattern
                 collect (cons predicate
                               (helm-pinyin--shorten-pinyin-regexp regexp len))))))

(defsubst helm-pinyin--rebuild-chars-order (dchars rchars)
  "Rebuild the order of DCHARS base on RCHARS.

E.g.: (fn \\='(\"c\" \"a\") \\='(\"a\" \"b\" \"c\"))
      => \\='((\"a\" . 0) (\"c\" . 2))"
  (let ((map (cl-mapcar #'cons (number-sequence 0 (1- (length rchars))) rchars)))
    (cl-sort (mapcar (lambda (char)
                       (let ((elt (rassoc char map)))
                         (when elt
                           (prog1 (cons char (car elt))
                             ;; Don't match next time
                             (setf (cdr elt) nil)))))
                     dchars)
             #'< :key #'cdr)))

(defsubst helm-pinyin--choose-pinyin-acronyms (chars max-len &optional mm-match-p)
  "Select as many chars as possible from CHARS to keep the re length < MAX-LEN.
\nIf mm-match-p not nil, means not a fuzzy match."
  (cl-loop with rem-len = max-len
           with acc-len = 0
           for char in chars
           for pair = (assoc char pinyin-char-lens-alist)
           when pair
           collect pair into pairs
           finally return
           (cl-loop with sorted-pairs = (cl-sort pairs #'< :key #'cdr)
                    with chosens = nil
                    for (char . num) in sorted-pairs
                    ;; For mm match, the first chinese character consumes a
                    ;; buffer size of 9 and each of rest consumes 5, E.g.:
                    ;;
                    ;;         [a]                   12
                    ;;         [a啊]                 21(+9)
                    ;;         [a啊阿]               26(+5)
                    ;;         [a啊阿嗷]             31(+5)
                    ;; 
                    ;; For fuzzy match, the first chinese character consumes a
                    ;; buffer size of 18 and each of rest consumes 10, E.g.:
                    ;;
                    ;;         [^a]*[a]              28
                    ;;         [^a啊]*[a啊]          46(+18)
                    ;;         [^a啊阿]*[a啊阿]      56(+10)
                    ;;         [^a啊阿嗷]*[a啊阿嗷]  66(+10)
                    ;;
                    do (setq rem-len
                             (if mm-match-p
                                 (- rem-len (* num 5) 4)
                               (- rem-len (* num 10) 8)))
                    while (< 0 rem-len)
                    collect char into chosens
                    do (setq acc-len (- max-len rem-len))
                    finally return
                    (progn
                      (when helm-pinyin-debug-p
                        (let ((inhibit-message nil))
                          (message "==> acyms(%s): %s, hanzi-nums: %s, oh: %s"
                                   (length chosens) chosens
                                   (apply #'+
                                          (mapcar (lambda (ch)
                                                    (cdr (assoc ch pinyin-char-lens-alist)))
                                                  chosens))
                                   acc-len)))
                      (cons (- max-len acc-len)
                            (helm-pinyin--rebuild-chars-order chosens chars))))))


;;; functions

(defsubst helm-pinyin-find-files-p ()
  "Check if current source is find files."
  (string= "Find Files" (assoc-default 'name (helm-get-current-source))))

(defsubst helm-pinyin--mapconcat-pattern (pattern &optional max-len mm-match-p)
  "Transform pinyin string PATTERN in regexp for further fuzzy matching.

Like ‘helm--mapconcat-pattern’ but generate pinyin regexp for fuzzy matching.

E.g: a.b$
     => \"[^a阿…]*[a阿…][^。.]*[。.][^b把…]*[b把…]$\"
     ^a.b$
     => \"a[.]b$\"."
  (let ((ls (split-string-and-unquote pattern "")))
    (if (string= "^" (car ls))
        ;; Exact match.
        (mapconcat (lambda (c)
                     (if (and (string= c "$")
                              (string-match "$\\'" pattern))
                         c (regexp-quote c)))
                   (cdr ls) "")
      ;; Fuzzy match.
      (cl-loop with rem-len = (- (or max-len helm-pinyin--reg-max)
                                 ;; Each [x] consumes a buffer size of 12
                                 ;; Each [^x]*[x] consumes a buffer size of 28
                                 (if mm-match-p
                                     (* 12 (length ls))
                                   (* 28 (length ls))))
               with pinyins =
                 (helm-pinyin--choose-pinyin-acronyms ls rem-len mm-match-p)
               for char in ls
               for index from 0
               for reg = 
               (if (and (string= char "$") (string-match "$\\'" pattern))
                   char
                 (if (rassoc index (cdr pinyins))
                       (let ((reg-pinyin (pinyinlib-build-regexp-string char)))
                         (if mm-match-p
                             reg-pinyin
                           (format "[^%s]*%s"
                                   (substring reg-pinyin 1 -1)
                                   reg-pinyin)))
                   (let ((reg-eng (regexp-quote char)))
                     (if mm-match-p
                         reg-eng
                       (format "[^%s]*[%s]" reg-eng reg-eng)))))
               collect reg into regs
               finally return
               (let ((ret (string-join regs "")))
                 (when helm-pinyin-debug-p
                   (let ((inhibit-message t))
                     (message "==> [-mapconcat-pattern] %s"
                              (list :reg-byges (string-bytes ret)
                                    :reg-len (length ret)
                                    :rem-len (list rem-len "->" (car pinyins))))))
                 ret)))))

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
                               (helm-mm-exact-match helm-pattern)
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
                          for target = (if (helm-get-attr 'match-on-real source)
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
                                    (condition-case err
                                        (if patts
                                            (funcall fn (or part target) patts)
                                          (funcall fn (or part target)))
                                      (invalid-regexp
                                       (when helm-pinyin-debug-p
                                         (let ((inhibit-message t))
                                           (message "invalid-regexp: %S" (cadr err))))
                                       nil)))
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
  "Around advice for ‘helm-ff--transform-pattern-for-completion’."
  (let* ((basedir (or (helm-basedir pattern) ""))
         (patts (split-string (string-remove-prefix basedir pattern) " "))
         (rem-len (- helm-pinyin--reg-max (length basedir) 4))) ;; 4: group op
    (if (not (cdr patts))
        (let* ((patt1 (car patts))
               (rem-len1 (- rem-len (length patt1))))
          (when helm-pinyin-debug-p
            (let ((inhibit-message t))
              (message "==> patt1: %s" patt1)
              (message "==> rem-len1: %s" rem-len1)))
          (concat (regexp-quote basedir)
                  (if (string-empty-p patt1)
                      ""
                    (helm-pinyin--mapconcat-pattern patt1 rem-len1))))
      (concat (regexp-quote basedir)
              " "
              (cl-loop for i from 0
                       for patt in (cl-remove-if #'string-empty-p patts)
                       for rem-len2 = (- rem-len (length patt))
                       collect (unless (string-empty-p patt)
                                 (when helm-pinyin-debug-p
                                   (let ((inhibit-message t))
                                     (message "==> patt: %s" patt)
                                     (message "==> rem-len2: %s" rem-len2)))
                                 (helm-pinyin--mapconcat-pattern
                                  patt rem-len2 'mm-match))
                         into regexps
                       finally return (string-join (remove nil regexps) " "))))))

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
