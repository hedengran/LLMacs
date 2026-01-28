;;; core-packages.el --- Package management with straight.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Bootstrap straight.el for reproducible, lazy package management.
;; Integrates with use-package for declarative configuration.

;;; Code:

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el
(setq straight-use-package-by-default t  ; use-package uses straight by default
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-vc-git-default-clone-depth 1)  ; Shallow clones for speed

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package defaults for lazy loading
(setq use-package-always-defer t        ; Always defer unless :demand
      use-package-expand-minimally t    ; Faster macro expansion
      use-package-verbose nil)          ; Quiet

(provide 'core-packages)
;;; core-packages.el ends here
