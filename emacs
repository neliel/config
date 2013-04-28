;;; .emacs

; -----------------------------------------------------------------------------

;; Daemon mode
(require 'server)
(or (server-running-p)
    (server-start))

; -----------------------------------------------------------------------------

;; Interfaces variables
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p) ; take the short answer, y/n is yes/no

(setq frame-title-format "%b - GNU Emacs")
(setq default-directory "~/")

(column-number-mode t)
(global-linum-mode t)

(global-hl-line-mode 1)
(show-paren-mode 1)

(display-time-mode t)
(setq display-time-24hr-format t)

; -----------------------------------------------------------------------------

;; *nix
(set-default-font "Inconsolata-11")

; -----------------------------------------------------------------------------

;; Frame settings
(setq default-frame-alist
  '((top . 99) (left . 16)
    (width . 84) (height . 42)))

; -----------------------------------------------------------------------------

;; Environment variables
(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized-light t)

; -----------------------------------------------------------------------------

;; Backup settings
(setq
  backup-by-copying t
  backup-directory-alist '(("." . "~/.emacs.d/backup"))
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

; -----------------------------------------------------------------------------

;; Keybindings
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key [f4] 'kill-this-buffer)

; -----------------------------------------------------------------------------

;; Tabulations settings
(setq indent-tabs-mode nil)
(setq standard-indent 2)
(add-hook 'before-save-hook  'delete-trailing-whitespace)

; -----------------------------------------------------------------------------

;; Org mode
(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "FROZEN(f)" "|" "DONE(d)" "CANCELED(c)")))

; -----------------------------------------------------------------------------

;; ECR (IRC client)
;; http://emacs-fu.blogspot.fr/2009/06/erc-emacs-irc-client.html
(require 'erc)

(erc-autojoin-mode t) ; joining && autojoing
; make sure to use wildcards for e.g. freenode as the actual server
; name can be be a bit different, which would screw up autoconnect
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "#haskell" "#haskell-fr")))

(erc-track-mode t) ; check channels
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))

(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK")) ; don't show any of this

(global-set-key (kbd "C-c e") 'erc-start-or-switch) ; convenient shortcut

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ; ERC already active?
      (erc-track-switch-buffer 1) ; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "ratalparatil" :full-name "Ele Pal"))))

; -----------------------------------------------------------------------------

;; "Brevety is the soul of wit <foo@acm.org>
(defalias 'perl-mode 'cperl-mode)

; -----------------------------------------------------------------------------

;; Haskell support
(load "haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

; -----------------------------------------------------------------------------
;; AUCTeX
(load "site-start.d/auctex.el" nil t t)
(load "site-start.d/preview-latex.el" nil t t)

; -----------------------------------------------------------------------------
;; CEDET
(global-ede-mode 1) ; enable the Project management system

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(require 'semantic/ia)

(semantic-mode 1)

; -----------------------------------------------------------------------------
;; Insert filename
(global-set-key (kbd "C-c f") 'insert-inname)
(defun insert-inname ()
  (interactive)
  (insert (file-name-nondirectory (buffer-file-name))))

; -----------------------------------------------------------------------------
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

; -----------------------------------------------------------------------------

;eof
