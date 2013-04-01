;;; .emacs

;;; Daemon Mode
(require 'server)
(or (server-running-p)
    (server-start))

;; Interfaces variables
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(column-number-mode t)
(global-hl-line-mode 1)
(global-linum-mode t)
(show-paren-mode 1)

;; Time in 24h format
(setq display-time-24h-format t)
(display-time-mode t)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Environment variables
(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Theme
(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-light)
(set-default-font "Inconsolata-14")

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; Backup settings
(setq
  backup-by-copying t
  backup-directory-alist '(("." . "~/.emacs.d/backup"))
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Tabulations settings
(setq indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; "Brevety is the soul of wit <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)

;; Haskell mode
(load "haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; size and pos
(setq default-directory "~/")
(setq default-frame-alist
  '((top . 300) (left . 40)
    (width . 87) (height . 37)))

;EOF
