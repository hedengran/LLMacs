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

;; Show column number in mode line
(column-number-mode 1)

;; Cursor and visual feedback
(setq-default cursor-type 'bar)
(blink-cursor-mode -1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Mode line - doom-modeline for a clean, modern look
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 28
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-env-version nil
        doom-modeline-modal-icon nil
        doom-modeline-modal nil
        doom-modeline-column-zero-based nil
        doom-modeline-vcs-max-length 50))

;; Icons for doom-modeline
(use-package nerd-icons
  :demand t)

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
