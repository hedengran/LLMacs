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

;; Orderless - flexible matching (space-delimited fuzzy filtering)
(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil
        ;; Allow space-separated patterns
        orderless-component-separator #'orderless-escapable-split-on-space
        ;; Match styles: literal, regexp, flex, initialism
        orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-flex)))

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
