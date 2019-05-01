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
;; C-h f - help for a functio
;; M-j Start a new line with a comment

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

(use-package evil
  :config 
    (require 'evil)
    (evil-mode t))

;; Inserted by Evil Mode, don't edit:
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

(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files))
  :config
    (require 'helm-config)
    (helm-mode 1)
    (helm-autoresize-mode t))


(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(use-package spaceline
  :config
  (require `spaceline-config)
  (spaceline-emacs-theme))


;; General config
(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

