;;; core-keybindings.el --- Spacemacs-style keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; SPC as leader key, inspired by Spacemacs.
;; Uses general.el for clean keybinding definitions.

;;; Code:

(use-package general
  :demand t
  :config
  ;; Define leader key
  (general-create-definer llmacs/leader-keys
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Define local leader for mode-specific bindings
  (general-create-definer llmacs/local-leader-keys
    :states '(normal visual motion)
    :keymaps 'override
    :prefix ","
    :global-prefix "C-,")

  ;; Core keybindings
  (llmacs/leader-keys
    ;; Top level
    "SPC" '(execute-extended-command :wk "M-x")
    ":" '(eval-expression :wk "eval")
    ";" '(comment-line :wk "comment")
    "u" '(universal-argument :wk "universal arg")

    ;; Buffers
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch")
    "bd" '(kill-current-buffer :wk "kill")
    "bD" '(kill-buffer-and-window :wk "kill buf+win")
    "bn" '(next-buffer :wk "next")
    "bp" '(previous-buffer :wk "previous")
    "br" '(revert-buffer :wk "revert")
    "bs" '(save-buffer :wk "save")

    ;; Files
    "f" '(:ignore t :wk "file")
    "ff" '(find-file :wk "find")
    "fr" '(recentf-open-files :wk "recent")
    "fs" '(save-buffer :wk "save")
    "fS" '(write-file :wk "save as")

    ;; Windows
    "w" '(:ignore t :wk "window")
    "ww" '(other-window :wk "other")
    "wd" '(delete-window :wk "delete")
    "wD" '(delete-other-windows :wk "delete others")
    "wh" '(evil-window-left :wk "left")
    "wj" '(evil-window-down :wk "down")
    "wk" '(evil-window-up :wk "up")
    "wl" '(evil-window-right :wk "right")
    "ws" '(evil-window-split :wk "split h")
    "wv" '(evil-window-vsplit :wk "split v")
    "w=" '(balance-windows :wk "balance")

    ;; Quit/Session
    "q" '(:ignore t :wk "quit")
    "qq" '(save-buffers-kill-emacs :wk "quit")
    "qQ" '(kill-emacs :wk "quit no save")

    ;; Help
    "h" '(:ignore t :wk "help")
    "hf" '(describe-function :wk "function")
    "hv" '(describe-variable :wk "variable")
    "hk" '(describe-key :wk "key")
    "hm" '(describe-mode :wk "mode")
    "hp" '(describe-package :wk "package")
    "hr" '(llmacs/reload-config :wk "reload config")

    ;; Toggle
    "t" '(:ignore t :wk "toggle")
    "tl" '(display-line-numbers-mode :wk "line numbers")
    "tw" '(whitespace-mode :wk "whitespace")
    "tt" '(load-theme :wk "theme")

    ;; Compile
    "c" '(:ignore t :wk "compile")
    "cm" '(llmacs/make :wk "make")
    "cc" '(compile :wk "compile")
    "cr" '(recompile :wk "recompile")
    "ck" '(kill-compilation :wk "kill"))

  ;; Make ESC quit prompts
  (general-define-key
   :keymaps '(minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map)
   [escape] 'keyboard-escape-quit))

;; Reload config
(defun llmacs/reload-config ()
  "Reload Emacs configuration by re-evaluating all module files."
  (interactive)
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
        (modules-dir (expand-file-name "modules" user-emacs-directory)))
    ;; Reload core modules
    (dolist (file '("core-packages" "core-ui" "core-editor" "core-evil"
                    "core-keybindings" "core-completion"))
      (load-file (expand-file-name (concat file ".el") lisp-dir)))
    ;; Reload feature modules
    (dolist (file (directory-files modules-dir nil "^mod-.*\\.el$"))
      (load-file (expand-file-name file modules-dir))))
  (message "Config reloaded."))

;; Make with target completion
(defun llmacs/makefile-targets ()
  "Extract targets from Makefile in project root."
  (let* ((root (or (when (fboundp 'project-root)
                     (when-let ((proj (project-current)))
                       (project-root proj)))
                   default-directory))
         (makefile (expand-file-name "Makefile" root)))
    (when (file-exists-p makefile)
      (with-temp-buffer
        (insert-file-contents makefile)
        (let (targets)
          (goto-char (point-min))
          ;; Match target definitions (not .PHONY, not variable assignments)
          (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\)\\s-*:" nil t)
            (let ((target (match-string 1)))
              (unless (string-prefix-p "." target)
                (push target targets))))
          (nreverse targets))))))

(defun llmacs/make (target)
  "Run make with TARGET, with completion from Makefile."
  (interactive
   (list (completing-read "Make target: "
                          (llmacs/makefile-targets)
                          nil nil nil nil "default")))
  (let* ((root (or (when (fboundp 'project-root)
                     (when-let ((proj (project-current)))
                       (project-root proj)))
                   default-directory))
         (default-directory root))
    (compile (format "make %s" target))))

(provide 'core-keybindings)
;;; core-keybindings.el ends here
