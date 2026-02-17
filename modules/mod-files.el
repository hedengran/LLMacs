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
    "ft" '(dirvish-side :wk "sidebar toggle")
    "fT" '(llmacs/dirvish-side-find-file :wk "find file in tree"))
  :config
  (setq dirvish-attributes '(file-size subtree-state)
        dirvish-mode-line-format '(:left (sort symlink) :right (yank index))
        dirvish-side-width 35
        dirvish-side-window-parameters '((no-delete-other-windows . t)))
  (evil-define-key 'normal dirvish-mode-map
    (kbd "TAB") #'dirvish-subtree-toggle
    "q" #'dirvish-quit)
  ;; Allow side window to be resized
  (advice-add 'dirvish-side--new :after
              (lambda (&rest _)
                (when-let ((dv (with-current-buffer (window-buffer)
                                 (dirvish-curr))))
                  (setf (dv-size-fixed dv) nil)
                  (setq window-size-fixed nil))))
  ;; Hide . and .. entries (BSD ls on macOS lacks -B flag)
  (require 'dired-x)
  (setq dired-omit-files "^\\.\\.?$")
  (add-hook 'dired-mode-hook #'dired-omit-mode))

(defun llmacs/dirvish-side-find-file ()
  "Open dirvish side and jump to the current file."
  (interactive)
  (let ((file buffer-file-name))
    (dirvish-side)
    (when file
      (dirvish-subtree-expand-to file))))

(provide 'mod-files)
;;; mod-files.el ends here
