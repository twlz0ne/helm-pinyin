;;; -*- lexical-binding: t; -*-

;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'ert)
(require 'subr-x)
(require 'helm-files)
(require 'helm-pinyin)
(turn-on-helm-pinyin)

;;; utils

(defun th-input-setup (input keys)
  (lambda ()
    (insert input)
    (let ((hook (make-symbol "hook")))
      (add-hook 'helm-after-update-hook hook)
      (fset hook (lambda ()
                   (remove-hook 'helm-after-update-hook hook)
                   (unless (string-empty-p keys)
                     (run-with-timer 0 nil
                                     (lambda ()
                                       (execute-kbd-macro (kbd keys))))))))))

(defmacro th-with-ff (input &optional keys)
  `(minibuffer-with-setup-hook
       (th-input-setup ,input ,(or keys ""))
     (call-interactively #'helm-find-files)))

;;; steps

(Given "^I have files:$"
  (lambda (table)
    (prog1
     (setq default-directory (make-temp-file "test-helm-pinyin--" 'dir "/"))
     (mapc (pcase-lambda (`(,filename ,content))
             (with-temp-buffer
               (insert content)
               (write-region (point-min) (point-max) filename)))
           (cdr table)))))

(When "^I execute helm-ff with \"\\(.*\\)\" input and \"\\(.*\\)\" press$"
  (lambda (input key)
    (th-with-ff input key)))

(Then "^I should in buffer \"\\(.*\\)\""
  (lambda (buffer-name)
    (should (string= buffer-name (buffer-name)))))

