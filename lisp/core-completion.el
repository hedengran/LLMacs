;;; core-completion.el --- Completion framework -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern, minimal completion using Vertico + friends.
;; These are lightweight and work well with built-in completion.

;;; Code:

;; Vertico - vertical completion UI
(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-count 10
        vertico-resize nil
        vertico-cycle t))

;; Orderless - flexible matching
(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia - annotations in minibuffer
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; Consult - enhanced commands
(use-package consult
  :general
  (llmacs/leader-keys
    "bb" '(consult-buffer :wk "switch")
    "fr" '(consult-recent-file :wk "recent")
    "/"  '(consult-ripgrep :wk "ripgrep")
    "ss" '(consult-line :wk "search buffer")
    "si" '(consult-imenu :wk "imenu"))
  :config
  (setq consult-narrow-key "<"
        consult-preview-key 'any))

;; Corfu - in-buffer completion (lighter than company)
(use-package corfu
  :hook (prog-mode . corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-quit-no-match 'separator))

(provide 'core-completion)
;;; core-completion.el ends here
