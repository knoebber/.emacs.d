;; Getting started with emacs.
;; 04/30/2019
;; Cx-Cc: quit
;; Cx-cf find file
;; M-x Execute command
;; C-g cancel
;; Cx-k Kill buffer
;; C-x C-s Save the current buffer
;; C-x s Save all buffers
;; C-h ? help
;; C-h k - help about a hotkey combo
;; C-h f - help for a function
;; M-j Start a new line with a comment
;; To install config
;; list-package RET package-install use-package RET
;; emacs-helm: C-j to autocomplete candidate

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; Setup use-package
;; https://github.com/jwiegley/use-package
(eval-when-compile (require 'use-package))

;; Always download packages that are not found
(setq use-package-always-ensure t)

;; Sync emacs path and shell path
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))
  
;; Setup helm
(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode t))

;; Setup magit
(use-package magit)
(use-package evil-magit
  :config
  (require 'evil-magit))

;; Setup evil leader
(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    "RET" 'eshell
    "x" 'helm-M-x
    "b" 'helm-mini
    "f" 'helm-find-files
    "n" 'rename-buffer
    "o" 'other-window
    "gs" 'magit-status
    "k" (lambda () (interactive) (kill-buffer nil))
    "r" (lambda() (interactive) (load-file "~/.emacs.d/init.el"))
    "e" (lambda() (interactive) (find-file "~/.emacs.d/init.el"))
    ;; C-i is tab
    "C-i" (lambda() (interactive) (switch-to-buffer nil))))

;; Setup evil
(use-package evil
  :config 
  (require 'evil)
  (evil-mode t))

'(package-selected-packages (quote (evil-visual-mark-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell spaceline spacemacs-theme use-package helm evil-visual-mark-mode))))

;; Setup origami
(use-package origami
  :config
  (require `origami)
  (origami-mode))

;; Setup theme and powerline
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(use-package spaceline
  :config
  (require `spaceline-config)
  (spaceline-emacs-theme))

;; Setup golang support
(use-package go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;; Setup webmode
(use-package web-mode
  :config
  (require `web-mode)
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)))

(defun my-web-mode-hook ()
  ;; Hooks for Web mode
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Setup linting
(use-package flycheck
  :config
  (require `flycheck))

;; Turn on flycheck on for web-mode
(add-hook 'web-mode-hook #'flycheck-mode)

;; Disable default linter
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint json-jsonlist)))

;; Use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; Use eslint file from local project folder
(use-package add-node-modules-path)
(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook #'add-node-modules-path))
  
;; General config
(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

