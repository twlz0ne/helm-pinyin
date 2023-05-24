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

(ert-deftest test-helm-pinyin-convert-to-pinyin ()
  (should (equal "csabc" (helm-pinyin-convert-to-pinyin "测试abc")))
  (should (equal "/path/to/csabc" (helm-pinyin-convert-to-pinyin "/path/to/测试abc")))
  (should (equal "/路径/csabc" (helm-pinyin-convert-to-pinyin "/路径/测试abc"))))

(ert-deftest test-helm-pinyin-text-properties ()
  (should (equal
           (if (<= emacs-major-version 27)
               '((2 3 (face nil)) (1 2 (face nil display "0")) (0 1 (face helm-match)))
               '((0 1 (face helm-match)) (1 2 (display "0" face nil)) (2 3 (face nil))))
           (let ((text (propertize "foo" 'face nil)))
             (put-text-property 0 1 'face 'helm-match text)
             (put-text-property 1 2 'display "0" text)
             (helm-pinyin-text-properties text)))))

(provide 'helm-pinyin-test)

;;; helm-pinyin-test.el ends here
