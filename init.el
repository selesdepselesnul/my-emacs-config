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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

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
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package cider
  :ensure t)

(when (eq 'gnu/linux system-type)
  (use-package slime
    :ensure t
    :config
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (setq slime-contribs '(slime-fancy))))

(use-package zenburn-theme
  :ensure t
  :config
  (add-hook 'after-init-hook (lambda () (load-theme 'zenburn))))

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
    (setq slime-contribs '(slime-fancy))))

(cond ((eq 'gnu/linux system-type)
       (config-slime "/usr/bin/sbcl"))
      ((eq 'windows-nt system-type)
       (config-slime "~/ccl/wx86cl64.exe")))

(setq inhibit-startup-screen t)

;; turn off annoying bell
(setq visible-bell t)

;; when a file is updated outside emacs, make it update if it's already opened in emacs
(global-auto-revert-mode 1)

;; line-number
(global-linum-mode t)

;; lazy mode :v
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

;; replace highlighted text when yanking
(delete-selection-mode 1)

(menu-bar-mode -1)

(tool-bar-mode -1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq-default cursor-type 'bar)

(ido-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (clj-refactor aggressive-indent zenburn magit rainbow-delimeters slime company-mode centered-window-mode sound-wav helm-config projectile neotree helm restclient rainbow-delimiters which-key try use-package powerline web-mode request ## cider elpy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
