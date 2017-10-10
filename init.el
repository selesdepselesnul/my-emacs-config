(require 'package)

(setq package-enable-at-startup nil)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; turn off annoying bell
(setq visible-bell t)

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; auto-complete
(ac-config-default)

;; line-number
(global-linum-mode t)

;; smex
(require 'smex) ; Not needed if you use package.el (in case i'm using legacy emacs in any other place)
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
		  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)

;; multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; lazy mode :v
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

(menu-bar-mode -1)

(tool-bar-mode -1)

(add-hook 'after-init-hook (lambda () (load-theme 'dracula)))

(set-default 'cursor-type 'hbar)

(ido-mode)
