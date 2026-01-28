;;; init.el --- LLMacs entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal init.el that bootstraps the modular configuration.
;; All heavy lifting is deferred to lisp/ modules.

;;; Code:

;; Add lisp/ and modules/ to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Restore UI elements hidden in early-init after load
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq inhibit-redisplay nil
                  inhibit-message nil)
            (redisplay)))

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)))

;; Report startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "LLMacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

;; Load core modules in order
(require 'core-packages)    ; Package management first
(require 'core-ui)          ; Visual settings
(require 'core-editor)      ; Basic editing behavior
(require 'core-evil)        ; Evil mode (vim emulation)
(require 'core-keybindings) ; Spacemacs-style leader keys
(require 'core-completion)  ; Completion framework

;; Load feature modules
(require 'mod-project)      ; Project management (built-in project.el)
(require 'mod-git)          ; Git via magit
(require 'mod-go)           ; Go language support

(provide 'init)
;;; init.el ends here
