;;; core-ui.el --- UI configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual settings - prefer built-in where sufficient.

;;; Code:

;; Disable startup screens
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Clean UI (already done in early-init, but ensure)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)  ; Relative for evil motions

;; Highlight current line
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Cursor and visual feedback
(setq-default cursor-type 'bar)
(blink-cursor-mode -1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Mode line (built-in, minimal)
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-modified
                " "
                mode-line-buffer-identification
                " "
                mode-line-position
                " "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;; Breadcrumb - show symbol path in header line (uses eglot/imenu)
(use-package breadcrumb
  :hook (eglot-managed-mode . breadcrumb-local-mode))

;; Font (set if available)
(when (display-graphic-p)
  (when (member "JetBrains Mono" (font-family-list))
    (set-face-attribute 'default nil :font "JetBrains Mono" :height 140)))

;; Theme - use built-in modus themes (Emacs 28+)
(use-package emacs
  :config
  (load-theme 'modus-operandi-tinted t))  ; Light tinted theme, built-in since Emacs 28

(provide 'core-ui)
;;; core-ui.el ends here
