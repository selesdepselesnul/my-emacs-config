;; -*- lexical-binding: t -*-
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

(defun selesdepselesnul/config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun selesdepselesnul/stumpwm-config ()
  (interactive)
  (find-file "~/.stumpwm.d/init.lisp"))

(defun selesdepselesnul/kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

;; prevents stale elisp bytecode from shadowing more up-to-date source files
(setq load-prefer-newer t)

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq inhibit-startup-screen t)

;; set font
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; turn off annoying bell
(setq visible-bell t)

;; using setq-default instead of setq because inden-tabs-mode & tab-width are local buffer variable
(setq-default indent-tabs-mode nil);; don't use tabs to indent
(setq-default tab-width 4);; but maintain correct appearance

;; when a file is updated outside emacs,
;; make it update if it's already opened in emacs
(global-auto-revert-mode 1)

;; jusy use y / n instead of yes / no when asking for confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

;; replace highlighted text when yanking
(delete-selection-mode 1)

;; disable some gui stuff
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; for pretty unicode symbol
(global-prettify-symbols-mode 1)

(global-hl-line-mode t)

(setq make-backup-files nil)

;; get rid of ^M in windows
(defun no-dos-please-were-unixish ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-dos$" coding-str)
      (setq coding-str
            (concat (substring coding-str 0 (match-beginning 0)) "-unix"))
      (message "CODING: %s" coding-str)
      (set-buffer-file-coding-system (intern coding-str)))))

(add-hook 'find-file-hooks 'no-dos-please-were-unixish)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default header-line-format
              (list "|"
                    (make-string 79 ?-)
                    "|"))

(setq use-package-verbose t)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn 't))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package dired
  :config
  ;; dired : always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  ;; auto refresh dired when file changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (setq dired-dwim-target t))

(use-package try
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (setq web-mode-enable-current-column-highlight t))

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
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind (("M-<mouse-1>" . mc/add-cursor-on-click)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (multiple-cursors-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package cider
  :ensure t)

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

(use-package rust-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package php-mode
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "444238426b59b360fb74f46b521933f126778777c68c67841c31e0a68b0cc920" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "a25bd2ca94d2d4b86b2e2a6aa16528a47880784f4b09168a37c540e2dd721753" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (php-mode benchmark-init dired nasm-mode fsharp-mode haskell-mode elm-mode rust-mode alecs-theme dired+ go-mode beacon sml-modeline base16-theme counsel mmm-mode quack racket-mode geiser disable-mouse clj-refactor zenburn magit rainbow-delimeters slime company-mode centered-window-mode sound-wav helm-config projectile neotree helm restclient rainbow-delimiters which-key try use-package powerline web-mode request ## cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
