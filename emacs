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

;; emacs23

(require 'color-theme)
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-light)

(delete-selection-mode 1)

; -----------------------------------------------------------------------------

;; debian-mint
(set-default-font "Inconsolata-12")

; -----------------------------------------------------------------------------

;; Frame settings for current desktop
(setq default-frame-alist
  '((top . 10) (left . 5)
    (width . 84) (height . 42)))

; -----------------------------------------------------------------------------

;; Environment variables
(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

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
(global-set-key (kbd "<XF86Forward>") 'next-buffer)
(global-set-key (kbd "<XF86Back>")  'previous-buffer)

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
(require 'erc-dcc)

(erc-autojoin-mode t) ; joining && autojoing
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "#haskell" "#haskell-fr")))

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))

(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(global-set-key (kbd "C-c e") 'erc-start-or-switch)

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC? ")
      (erc :server "irc.freenode.net" :port 6667 :nick "sarfraz_0" :full-name "Sarfraz K."))))

; -----------------------------------------------------------------------------

;; "Brevety is the soul of wit <foo@acm.org>
;(defalias 'perl-mode 'cperl-mode)

; -----------------------------------------------------------------------------
;; Haskell support
(load "haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

; -----------------------------------------------------------------------------
;; AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

; -----------------------------------------------------------------------------
;; CEDET
(global-ede-mode 1) ; enable the Project management system

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(require 'semantic/ia)

(semantic-mode 1)

; -----------------------------------------------------------------------------
;; AutoComplete

(require 'auto-complete-config)
(ac-config-default)

; -----------------------------------------------------------------------------

;; Ido mode
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
(require 'ido)
(ido-mode 'both)
(setq
  ido-save-directory-list-file "~/.emacs.d/ido.last"
  ido-case-fold  t
  ido-enable-last-directory-history t
  ido-max-work-directory-list 30
  ido-max-work-file-list 50
  ido-use-filename-at-point nil
  ido-use-url-at-point nil
  ido-enable-flex-matching nil
  ido-max-prospects 5
  ido-confirm-unique-completion t)

 (setq confirm-nonexistent-file-or-buffer nil)

; -----------------------------------------------------------------------------
;; Sepia
(defalias 'perl-mode 'sepia-mode)
(require 'sepia)

; -----------------------------------------------------------------------------
;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup)

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
