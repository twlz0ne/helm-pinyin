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
   (string= "/path/to/[^f]*f[^o]*o[^o]*o"
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "/path/to/foo")))

  (should
   (string= "/path/to/foo bar"
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "/path/to/foo bar")))

  (should
   (string= "/path/to/ bar"
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "/path/to/ bar")))

  (should
   (string= "[^f]*f[^o]*o[^o]*o"
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "foo")))

  (should
   (string= "foo bar"
            (--call-orign 'helm-ff--transform-pattern-for-completion
                          "foo bar"))))

(ert-deftest helm-pinyin-test-transform-pattern-for-completion ()
  (should
   (string= "/path/to/"
            (helm-ff--transform-pattern-for-completion "/path/to/")))

  (should
   (string-prefix-p "/path/to/\\([^f]*f[^o]*o[^o]*o\\|[^f发法罚乏伐阀砝筏垡珐反饭"
                    (helm-ff--transform-pattern-for-completion "/path/to/foo")))

  (should
   (string-prefix-p "/path/to/\\(foo\\|[^f发法罚乏伐阀砝筏垡珐反饭犯翻范凡烦返番"
                    (helm-ff--transform-pattern-for-completion "/path/to/ foo")))

  (let* ((s "/path/to/foo bar")
         (patts (split-string (helm-ff--transform-pattern-for-completion s) " ")))
    (should
     (string-prefix-p "/path/to/\\(foo\\|[^f发法罚乏伐阀砝筏垡珐反饭犯翻范凡烦返"
                      (nth 0 patts)))

    (should
     (string-prefix-p "\\(bar\\|[^b把八吧巴爸罢拔叭芭霸靶扒疤跋坝笆耙粑灞茇菝魃岜"
                      (nth 1 patts))))

  (should
   (string-prefix-p "\\([^f]*f[^o]*o[^o]*o\\|[^f发法罚乏伐阀砝筏垡珐反饭犯翻范凡"
                    (helm-ff--transform-pattern-for-completion "foo")))

  (let* ((s "foo bar")
         (patts (split-string (helm-ff--transform-pattern-for-completion s) " ")))
    (should
     (string-prefix-p "\\(foo\\|[^f发法罚乏伐阀砝筏垡珐反饭犯翻范凡烦返番贩繁泛帆"
                      (nth 0 patts)))

    (should
     (string-prefix-p "\\(bar\\|[^b把八吧巴爸罢拔叭芭霸靶扒疤跋坝笆耙粑灞茇菝魃岜"
                      (nth 1 patts)))))

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
