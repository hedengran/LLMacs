;;; mod-git.el --- Git integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Git support via magit, the best Git interface.

;;; Code:

(use-package magit
  :commands (magit-status magit-blame magit-log-current magit-file-dispatch)
  :general
  (llmacs/leader-keys
    "g" '(:ignore t :wk "git")
    "gs" '(magit-status :wk "status")
    "gb" '(magit-blame :wk "blame")
    "gl" '(magit-log-current :wk "log")
    "gf" '(magit-file-dispatch :wk "file actions")
    "gd" '(magit-diff-dwim :wk "diff")
    "gc" '(magit-commit :wk "commit")
    "gp" '(magit-push :wk "push")
    "gF" '(magit-pull :wk "pull"))
  (:keymaps '(magit-status-mode-map magit-diff-mode-map)
   :states 'normal
   "RET" #'llmacs/magit-open-file-other-window)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-save-repository-buffers 'dontask)

  (defun llmacs/magit-open-file-other-window ()
    "Open file at point in other window, reusing existing split."
    (interactive)
    (if-let ((file (magit-file-at-point t)))
        (let ((win (if (one-window-p)
                       (split-window-right)
                     (next-window))))
          (select-window win)
          (find-file file))
      (user-error "No file at point"))))

;; Show git status in fringe
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :general
  (:states 'normal
   "]h" '(diff-hl-next-hunk :wk "next hunk")
   "[h" '(diff-hl-previous-hunk :wk "prev hunk"))
  :config
  (setq diff-hl-draw-borders nil))

;; Add magit to evil-collection
(with-eval-after-load 'evil-collection
  (unless (memq 'magit evil-collection-mode-list)
    (add-to-list 'evil-collection-mode-list 'magit)
    (evil-collection-init 'magit)))

(provide 'mod-git)
;;; mod-git.el ends here
