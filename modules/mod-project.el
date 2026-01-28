;;; mod-project.el --- Project management -*- lexical-binding: t; -*-

;;; Commentary:
;; Project management using built-in project.el (Emacs 28+).
;; No external packages needed.

;;; Code:

(use-package project
  :straight nil  ; Built-in
  :general
  (llmacs/leader-keys
    "p" '(:ignore t :wk "project")
    "pp" '(project-switch-project :wk "switch")
    "pf" '(project-find-file :wk "find file")
    "pd" '(project-find-dir :wk "find dir")
    "pb" '(project-switch-to-buffer :wk "buffer")
    "pk" '(project-kill-buffers :wk "kill buffers")
    "p/" '(project-find-regexp :wk "search")
    "pc" '(project-compile :wk "compile")
    "pe" '(project-eshell :wk "eshell"))
  :config
  ;; Use ripgrep if available
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Remember projects
  (setq project-list-file (expand-file-name "projects" user-emacs-directory)))

;; Enhanced project search with consult
(use-package consult
  :general
  (llmacs/leader-keys
    "p/" '(consult-ripgrep :wk "ripgrep")))

(provide 'mod-project)
;;; mod-project.el ends here
