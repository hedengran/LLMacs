;;; core-editor.el --- Basic editor behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; Essential editing settings using built-in features.

;;; Code:

;; Inherit environment variables from shell (macOS GUI Emacs)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :hook (after-init . exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-warn-duration-millis 500))

;; UTF-8 everywhere
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Indentation
(setq-default indent-tabs-mode nil  ; Spaces, not tabs
              tab-width 4)

;; Line handling
(setq-default truncate-lines t)           ; Don't wrap
(setq require-final-newline t)            ; Newline at EOF

;; Scrolling (smoother)
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; Horizontal scrolling
(setq hscroll-margin 5
      hscroll-step 1)
(setq mouse-wheel-tilt-scroll t           ; Enable horizontal mouse wheel
      mouse-wheel-flip-direction nil)

;; Pixel-precise smooth scrolling (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Files and backups
(setq auto-save-default nil               ; No #autosave# files
      make-backup-files nil               ; No backup~ files
      create-lockfiles nil)               ; No .#lockfiles

;; Revert buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Recent files (built-in)
(use-package recentf
  :straight nil  ; Built-in
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100
        recentf-exclude '("/tmp/" "/ssh:" "/.emacs.d/straight/")))

;; Save history (built-in)
(use-package savehist
  :straight nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)))

;; Remember cursor position (built-in)
(use-package saveplace
  :straight nil
  :hook (after-init . save-place-mode))

;; Electric pairs (built-in)
(use-package elec-pair
  :straight nil
  :hook (prog-mode . electric-pair-mode))

;; Compilation buffer settings
(use-package compile
  :straight nil
  :config
  ;; Interpret ANSI color codes in compilation buffer
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  ;; Scroll with output
  (setq compilation-scroll-output t))

;; Which-key (essential for discoverability)
(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.3
        which-key-sort-order 'which-key-prefix-then-key-order))

(provide 'core-editor)
;;; core-editor.el ends here
