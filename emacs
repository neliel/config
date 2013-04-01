;;; .emacs

;; Daemon
(require 'server)
(or (server-running-p)
    (server-start))

;; Interface
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(column-number-mode t)
(global-linum-mode t)

(global-hl-line-mode 1)
(show-paren-mode 1)

(display-time-mode t)
(setq display-time-24hr-format t)

;; Changes all yes/no questions to y/n type
(defalias 'yes-or-no-p 'y-or-n-p)

;; Environment variables
(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)
(set-default-font "Inconsolata-12")

;; Backup
(setq
  backup-by-copying t
  backup-directory-alist '(("." . "~/.emacs.d/backup"))
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Tabulations
(setq indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; "Brevety is the soul of wit <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)

;; Haskell
(load "haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Size & Pos
(setq default-directory "~/")
(setq default-frame-alist
  '((top . 99) (left . 16)
    (width . 66) (height . 33)))

;EOF
