;;; core-evil.el --- Evil mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil mode is the heart of this config.
;; Vim emulation with sensible defaults.

;;; Code:

;; Evil dependencies
(use-package undo-fu
  :demand t)  ; Needed before evil loads

;; Evil mode
(use-package evil
  :demand t  ; Load immediately - it's core
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil      ; We'll use evil-collection
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-undo-system 'undo-fu
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)

  ;; Cursor shapes per state
  (setq evil-normal-state-cursor '(box)
        evil-insert-state-cursor '(bar)
        evil-visual-state-cursor '(hollow)
        evil-emacs-state-cursor '(hbar))

  ;; Use Escape to quit prompts
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-escape-quit))

;; Evil collection - consistent vim bindings across modes
(use-package evil-collection
  :after evil
  :demand t
  :config
  (setq evil-collection-mode-list
        '(dired magit help ibuffer xref compile flymake))
  (evil-collection-init))

;; Temporary buffer behavior: q to quit, C-n/C-p to navigate
(defvar llmacs/special-buffer-modes
  '(help-mode
    compilation-mode
    xref--xref-buffer-mode
    messages-buffer-mode
    special-mode
    flymake-diagnostics-buffer-mode)
  "Modes where q should quit and C-n/C-p should navigate.")

(defun llmacs/setup-special-buffer-keys ()
  "Setup keys for temporary/special buffers."
  (when (derived-mode-p 'special-mode)
    (evil-local-set-key 'normal (kbd "q") #'quit-window)
    (evil-local-set-key 'normal (kbd "C-n") #'next-line)
    (evil-local-set-key 'normal (kbd "C-p") #'previous-line)))

(dolist (mode llmacs/special-buffer-modes)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook #'llmacs/setup-special-buffer-keys)))

;; Ensure xref buffers get proper navigation
(with-eval-after-load 'xref
  (evil-define-key 'normal xref--xref-buffer-mode-map
    (kbd "q") #'quit-window
    (kbd "C-n") #'xref-next-line
    (kbd "C-p") #'xref-prev-line
    (kbd "RET") #'xref-goto-xref
    (kbd "o") #'xref-show-location-at-point))

;; Compilation buffer navigation
(with-eval-after-load 'compile
  (evil-define-key 'normal compilation-mode-map
    (kbd "q") #'quit-window
    (kbd "C-n") #'compilation-next-error
    (kbd "C-p") #'compilation-previous-error
    (kbd "RET") #'compile-goto-error))

;; Commentary with gc
(use-package evil-commentary
  :after evil
  :demand t
  :config
  (evil-commentary-mode))

;; Surround with s
(use-package evil-surround
  :after evil
  :demand t
  :config
  (global-evil-surround-mode 1))

(provide 'core-evil)
;;; core-evil.el ends here
