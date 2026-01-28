;;; mod-files.el --- File management with dirvish -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced file management using dirvish (modern dired).

;;; Code:

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :general
  (llmacs/leader-keys
    "fd" '(dirvish :wk "dirvish here")
    "fj" '(dirvish-side :wk "sidebar toggle"))
  :config
  (setq dirvish-attributes '(git-msg file-size collapse subtree-state)
        dirvish-mode-line-format '(:left (sort symlink) :right (vc-info yank index))
        dirvish-side-width 35))

(provide 'mod-files)
;;; mod-files.el ends here
