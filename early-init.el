;;; early-init.el --- Pre-GUI Emacs optimizations -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded before init.el and before the GUI is initialized.
;; Critical for fast startup - disable expensive operations early.

;;; Code:

;; Increase GC threshold during startup (reset later)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent package.el from loading at startup (we use straight.el)
(setq package-enable-at-startup nil)

;; Disable expensive UI elements before they load
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable mode-line during init
(setq-default mode-line-format nil)

;; Prevent resizing frame during font changes
(setq frame-inhibit-implied-resize t)

;; Ignore X resources (slight speedup)
(advice-add #'x-apply-session-resources :override #'ignore)

;; Prevent glimpse of unstyled Emacs
(setq inhibit-redisplay t
      inhibit-message t)

;; Native compilation settings (Emacs 28+)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

(provide 'early-init)
;;; early-init.el ends here
