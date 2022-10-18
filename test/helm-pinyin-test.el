;;; helm-pinyin-test.el --- Test helm-pinyin -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'helm-pinyin)
(require 'helm-files)
(turn-on-helm-pinyin)

(when noninteractive
  (transient-mark-mode))

(defun --call-orign (adviced-fn &rest args)
  (apply (advice--cdr (symbol-function adviced-fn)) args))

(ert-deftest helm-pinyin-test-transform-pattern-for-completion-original ()
  "Tests to monitor changes in `helm'."

  (should
   (string= "/path/to/to" ;; Maybe there is a bug in `helm-basename'
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "/path/to/")))

  (should
   (string= "/path/to/[^f]*?f[^o]*?o[^o]*?o"
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "/path/to/foo")))

  (should
   (string= "/path/to/ foo bar"
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "/path/to/foo bar")))

  (should
   (string= "/path/to/  bar"
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "/path/to/ bar")))

  (should
   (string= "[^f]*?f[^o]*?o[^o]*?o"
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "foo")))

  (should
   (string= " foo bar"
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "foo bar"))))

(ert-deftest helm-pinyin-test-transform-pattern-for-completion ()
  (should
   (string= "/path/to/"
            (helm-ff--transform-pattern-for-completion "/path/to/")))

  (should
   (equal "/path/to/[^f发]*[f发][^o哦]*[o哦][^o哦]*[o哦]"
          (helm-pinyin--shorten-pinyin-regexp
           (helm-ff--transform-pattern-for-completion "/path/to/foo"))))

  (should
   (equal "/path/to/ [f发][o哦][o哦] [b把][a阿][r然]"
          (helm-pinyin--shorten-pinyin-regexp
           (helm-ff--transform-pattern-for-completion "/path/to/foo bar"))))

  (should
   (equal "/path/to/ [b把][a阿][r然]"
          (helm-pinyin--shorten-pinyin-regexp
           (helm-ff--transform-pattern-for-completion "/path/to/ bar"))))

  (should
   (equal "[^f发]*[f发][^o哦]*[o哦][^o哦]*[o哦]"
          (helm-pinyin--shorten-pinyin-regexp
           (helm-ff--transform-pattern-for-completion "foo"))))

  (should
   (equal " [f发][o哦][o哦] [b把][a阿][r然]"
          (helm-pinyin--shorten-pinyin-regexp
           (helm-ff--transform-pattern-for-completion "foo bar")))))

(ert-deftest helm-pinyin-test-mm-3-match ()
  (let ((s "/path/+to/file"))
    (should
     (funcall #'helm-pinyin-mm-3-match
              s
              (list
               (cons 'identity
                     (helm-ff--transform-pattern-for-completion s))))))

  (let ((s "/path/t o/file"))
    (should
     (funcall #'helm-pinyin-mm-3-match
              s
              (list
               (cons 'identity
                     (helm-ff--transform-pattern-for-completion s)))))))

(provide 'helm-pinyin-test)

;;; helm-pinyin-test.el ends here
