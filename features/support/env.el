(require 'f)

(defvar emacs-helm-pinyin-support-path
  (f-dirname load-file-name))

(defvar emacs-helm-pinyin-features-path
  (f-parent emacs-helm-pinyin-support-path))

(defvar emacs-helm-pinyin-root-path
  (f-parent emacs-helm-pinyin-features-path))

(add-to-list 'load-path emacs-helm-pinyin-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'emacs-helm-pinyin)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
