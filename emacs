;;; .emacs

;; Daemon mode
(require 'server)
(or (server-running-p)
    (server-start))

;; Interface variables
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(column-number-mode t)
(global-linum-mode t)

(global-hl-line-mode 1)
(show-paren-mode 1)

(display-time-mode t)
(setq display-time-24hr-format t)

;; take the short answer, y/n is yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Environment variables
(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")
(load-theme 'solarized-light t)
(set-default-font "Inconsolata-11")

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

;; Org mode
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "FROZEN(f)" "|" "DONE(d)" "CANCELED(c)")))

;; "Brevety is the soul of wit <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)

;; Haskell
(load "haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; CEDET
(global-ede-mode 1)

;; Insert date
(global-set-key (kbd "C-c d") 'insert-date)
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
		 ((not prefix) "%d.%m.%Y")
		 ((equal prefix '(4)) "%Y-%m-%d")
		 ((equal prefix '(16)) "%A, %d. %B %Y")))
	(system-time-locale "fr_FR"))
    (insert (format-time-string format))))

;; Keybindings
(global-set-key [f1] 'shell)

;; Size & Pos
(setq default-directory "~/")
(setq frame-title-format "%b - GNU Emacs")
(setq default-frame-alist
  '((top . 99) (left . 16)
    (width . 84) (height . 42)))

;eof
