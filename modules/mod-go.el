;;; mod-go.el --- Go language support -*- lexical-binding: t; -*-

;;; Commentary:
;; Go development with go-mode and eglot (built-in LSP).
;; Requires: go, gopls (go install golang.org/x/tools/gopls@latest)

;;; Code:

;; Go mode
(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . eglot-ensure)
         (go-mode . llmacs/go-mode-setup))
  :general
  (llmacs/local-leader-keys
    :keymaps 'go-mode-map
    "=" '(:ignore t :wk "format")
    "==" '(eglot-format-buffer :wk "format buffer")

    "g" '(:ignore t :wk "goto")
    "gd" '(xref-find-definitions :wk "definition")
    "gr" '(xref-find-references :wk "references")
    "gi" '(eglot-find-implementation :wk "implementation")

    "r" '(:ignore t :wk "refactor")
    "rr" '(eglot-rename :wk "rename")
    "ra" '(eglot-code-actions :wk "code action")

    "t" '(:ignore t :wk "test")
    "tt" '(llmacs/go-test-current :wk "test at point")
    "tf" '(llmacs/go-test-file :wk "test file")
    "tp" '(llmacs/go-test-package :wk "test package")

    "x" '(:ignore t :wk "execute")
    "xr" '(llmacs/go-run :wk "run")
    "xb" '(llmacs/go-build :wk "build"))
  :config
  (defun llmacs/go-mode-setup ()
    "Setup for Go buffers."
    (setq-local tab-width 4)
    ;; Format and organize imports on save
    (add-hook 'before-save-hook #'eglot-format-buffer nil t)
    (add-hook 'before-save-hook
              (lambda () (call-interactively #'eglot-code-action-organize-imports))
              nil t))

  ;; Test commands
  (defun llmacs/go-test-current ()
    "Run test at point."
    (interactive)
    (if-let ((test-name (llmacs/go-test-name-at-point)))
        (compile (format "go test -v -run '%s' ." test-name))
      (message "No test found at point")))

  (defun llmacs/go-test-name-at-point ()
    "Get test function name at point."
    (save-excursion
      (when (re-search-backward "func \\(Test[a-zA-Z0-9_]*\\)" nil t)
        (match-string 1))))

  (defun llmacs/go-test-file ()
    "Run tests in current file."
    (interactive)
    (compile "go test -v ."))

  (defun llmacs/go-test-package ()
    "Run all tests in package."
    (interactive)
    (compile "go test -v ./..."))

  (defun llmacs/go-run ()
    "Run current file."
    (interactive)
    (compile (format "go run %s" (buffer-file-name))))

  (defun llmacs/go-build ()
    "Build current package."
    (interactive)
    (compile "go build .")))

;; Eglot (built-in LSP client, Emacs 29+, or install for earlier)
(use-package eglot
  :straight nil  ; Built-in since Emacs 29
  :commands eglot-ensure
  :config
  (setq eglot-autoshutdown t  ; Shutdown LSP when last buffer closed
        eglot-events-buffer-size 0  ; Disable event logging for performance
        eglot-sync-connect nil))  ; Don't block on LSP connect

;; Add eglot to evil-collection
(with-eval-after-load 'evil-collection
  (unless (memq 'eglot evil-collection-mode-list)
    (add-to-list 'evil-collection-mode-list 'eglot)
    (evil-collection-init 'eglot)))

(provide 'mod-go)
;;; mod-go.el ends here
