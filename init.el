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

;; Setup use-packge
;; https://github.com/jwiegley/use-package
(eval-when-compile (require 'use-package))

;; Always download packages that are not found
(setq use-package-always-ensure t)

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
    "n" 'rename-buffer
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
    (spaceline spacemacs-theme use-package helm evil-visual-mark-mode))))

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

;; General config
(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

