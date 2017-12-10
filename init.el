(require 'package)

(setq package-enable-at-startup nil)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)


(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(setq inhibit-startup-screen t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; turn off annoying bell
(setq visible-bell t)

(setq-default indent-tabs-mode nil);; don't use tabs to indent
(setq-default tab-width 4);; but maintain correct appearance

;; when a file is updated outside emacs,
;; make it update if it's already opened in emacs
(global-auto-revert-mode 1)

;; lazy mode :v
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

;; replace highlighted text when yanking
(delete-selection-mode 1)

(menu-bar-mode -1)

(tool-bar-mode -1)

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(scroll-bar-mode -1)
;; copy directly without asking
(setq dired-recursive-copies (quote always))

(global-prettify-symbols-mode 1)

(global-hl-line-mode t)

(setq org-confirm-babel-evaluate nil)

(setq next-line-add-newlines t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(defun selesdepselesnul/config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defconst lisp-family-mode-hooks
  '(emacs-lisp-mode-hook
    eval-expression-minibuffer-setup-hook
    ielm-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    scheme-mode-hook
    clojure-mode-hook))

(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (dolist (hook lisp-family-mode-hooks)
    (add-hook hook #'enable-paredit-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (dolist (hook lisp-family-mode-hooks)
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind (("C-x /" . company-complete-common)))


(use-package multiple-cursors
  :ensure t
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package cider
  :ensure t)

(use-package grandshell-theme
  :ensure t
  :config
  (load-theme 'grandshell))

(use-package aggressive-indent
  :ensure t
  :config
  (dolist (hook lisp-family-mode-hooks)
    (add-hook hook #'aggressive-indent-mode)))

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

(defun config-slime (path)
  (use-package slime
    :ensure t
    :config
    (setq inferior-lisp-program path)
    (setq slime-contribs '(slime-fancy slime-asdf))))

(cond ((eq 'gnu/linux system-type)
       (config-slime "/usr/bin/sbcl"))
      ((eq 'windows-nt system-type)
       (config-slime "~/ccl/wx86cl64.exe")))

(use-package racket-mode
  :ensure t)

(use-package quack
  :ensure t)

(use-package geiser
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package mmm-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package go-mode
  :ensure t)

(use-package dired+
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a25bd2ca94d2d4b86b2e2a6aa16528a47880784f4b09168a37c540e2dd721753" "57fe2bf84d81baecc6d89ed97bdb19936a3052fc2551ca178667fc45feef2381" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (grandshell-theme dired+ go-mode beacon sml-modeline counsel mmm-mode quack racket-mode geiser disable-mouse clj-refactor aggressive-indent zenburn magit rainbow-delimeters slime company-mode centered-window-mode sound-wav helm-config projectile neotree helm restclient rainbow-delimiters which-key try use-package powerline web-mode request ## cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
