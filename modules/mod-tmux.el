;;; mod-tmux.el --- Seamless Emacs-tmux navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; When at the edge of an Emacs frame inside tmux, C-w h/j/k/l and
;; SPC w h/j/k/l jump to the adjacent tmux pane instead of doing nothing.

;;; Code:

(defun llmacs/tmux-p ()
  "Return non-nil if running inside tmux."
  (getenv "TMUX"))

(defun llmacs/tmux-navigate (direction)
  "Navigate to the Emacs window in DIRECTION, or the tmux pane if at edge.
DIRECTION is one of `left', `down', `up', `right'."
  (let ((win (window-in-direction
              (pcase direction
                ('left 'left)
                ('down 'below)
                ('up 'above)
                ('right 'right)))))
    (if win
        (select-window win)
      (if (llmacs/tmux-p)
          (let ((tmux-flag (pcase direction
                             ('left "-L")
                             ('down "-D")
                             ('up "-U")
                             ('right "-R"))))
            (call-process "tmux" nil nil nil "select-pane" tmux-flag))
        ;; Not in tmux — fall back to normal evil behavior
        (pcase direction
          ('left (evil-window-left 1))
          ('down (evil-window-down 1))
          ('up (evil-window-up 1))
          ('right (evil-window-right 1)))))))

(defun llmacs/tmux-navigate-left ()
  "Navigate left (Emacs window or tmux pane)."
  (interactive)
  (llmacs/tmux-navigate 'left))

(defun llmacs/tmux-navigate-down ()
  "Navigate down (Emacs window or tmux pane)."
  (interactive)
  (llmacs/tmux-navigate 'down))

(defun llmacs/tmux-navigate-up ()
  "Navigate up (Emacs window or tmux pane)."
  (interactive)
  (llmacs/tmux-navigate 'up))

(defun llmacs/tmux-navigate-right ()
  "Navigate right (Emacs window or tmux pane)."
  (interactive)
  (llmacs/tmux-navigate 'right))

;; Override C-w h/j/k/l in evil normal and visual states
(with-eval-after-load 'evil
  (dolist (state '(normal visual))
    (evil-define-key state 'global
      (kbd "C-w h") #'llmacs/tmux-navigate-left
      (kbd "C-w j") #'llmacs/tmux-navigate-down
      (kbd "C-w k") #'llmacs/tmux-navigate-up
      (kbd "C-w l") #'llmacs/tmux-navigate-right)))

;; Override SPC w h/j/k/l leader bindings
(with-eval-after-load 'general
  (llmacs/leader-keys
    "wh" '(llmacs/tmux-navigate-left :wk "left")
    "wj" '(llmacs/tmux-navigate-down :wk "down")
    "wk" '(llmacs/tmux-navigate-up :wk "up")
    "wl" '(llmacs/tmux-navigate-right :wk "right")))

(provide 'mod-tmux)
;;; mod-tmux.el ends here
