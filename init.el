;;; init.el --- Summary: Emacs
;;; Commentary:
;;; First Emacs install date: 04/30/2019
;;; Cx-Cc: quit Emacs
;;; C-x C-s Save the current buffer
;;; C-x s Save all buffers
;;; C-h ? help
;;; C-h k - help about a hotkey combo
;;; C-h f - help for a function
;;; M-j Start a new line with a comment
;;; To install config
;;; list-package RET package-install use-package RET
;;; emacs-helm: C-j to autocomplete candidate

;;; Code:
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
  (helm-autoresize-mode t)
  (setq helm-buffer-max-length nil))

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
    "b" 'helm-mini ;; In helm mini, use C-SPC to select buffers, M-D to kill all marked
    "f" 'helm-find-files
    "hk" 'describe-key
    "hf" 'describe-function
    "n" 'rename-buffer
    "j" 'other-window
    "vn" 'narrow-to-region
    "vw" 'widen
    "o" 'delete-other-windows
    "g" 'magit-status
    ";" 'comment-line
    "tt" 'go-test ;; TODO make go-* only available in go-mode.
    "tf" 'go-run-current-test
    "ts" 'go-run-current-sub-test
    "tc" 'go-coverage-shorcut
    "ti" 'go-insert-subtest
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
 '(line-number-mode nil)
 '(package-selected-packages
   (quote
    (markdown-mode company company-mode exec-path-from-shell spaceline spacemacs-theme use-package helm evil-visual-mark-mode))))

;; Setup origami
(use-package origami
  :config
  (add-hook 'prog-mode-hook 'origami-mode))

;; Setup theme and powerline
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

(use-package spaceline
  :config
  (require `spaceline-config)
  (spaceline-emacs-theme))

;; Setup golang support
(use-package go-mode
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
    :bind (("s-b" . godef-jump-other-window)))
  

;; Setup webmode
(use-package web-mode
  :mode "\\.jsx?\\'"
  :init
  (defun my-web-mode-hook ()
    ;; Hooks for Web mode
    (setq-default indent-tabs-mode nil)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil))
  (add-hook 'web-mode-hook  'my-web-mode-hook))


(defun compile-post ()
  "Use a script from personal-website to compile a blog markdown file."
  (interactive)
  (string-match "\\([[:digit:]]\\)\.md" (buffer-name))
  (shell-command (concat "create-post " (match-string 1 (buffer-name)))))

(defun compile-blog-and-upload ()
  "Use scripts from personal-website to compile and upload a blog post."
  (interactive)
  (string-match "\\([[:digit:]]\\)\.md" (buffer-name))
  (shell-command (format "create-post %s && upload-site" (match-string 1 (buffer-name)))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :bind (
	 ("C-c m" . compile-post)
	 ("C-c c" . compile-blog-and-upload)))

;; Setup linting with flycheck
;; Use 'C-c ! v' to check flycheck status in buffer.
(use-package flycheck
  :init
  (add-hook 'prog-mode-hook 'global-flycheck-mode)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; Setup auto completion
(use-package company
  :init
  (global-company-mode))

;;; General config
(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;;; Enable banned commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defun insert-current-date ()
    "Insert the current date."
    (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun go-insert-subtest(name)
  "Insert a subtest with NAME."
  (interactive "Mtest name ")
  (insert (format "\nt.Run(\"%s\", func(t *testing.T){\n})\n" name)))

(defun go-test (pattern)
  "Test go functions matching PATTERN and generate a coverage file."
  (interactive "Mgo test -run ")
  (let ((cmd (format "go test -run '%s' -coverprofile=coverage.out" pattern)))
    (message cmd)
    (eshell-command cmd)))

(defun go-coverage-shorcut ()
  "Run go-coverage with coverage.out ."
  (interactive)
  (go-coverage "coverage.out"))

(defun go-current-function-name ()
  "Get the name of the go function that the cursor is in.  Handles receivers."
  (save-excursion
    (re-search-backward "^func[[:space:]]\\(([^)]+)[[:space:]]\\)?\\([[:word:]]+\\)")
    (match-string 2)))

(defun go-sub-test-name ()
  "Get the name of the current subtest that the cursor is in."
  (save-excursion
    (re-search-backward "t.Run(\"\\([[:word:][:space:]]+\\)")
    (match-string 1)))


(defun go-run-current-test ()
  "Run the current top level go test."
  (interactive)
  (go-test (concat (go-current-function-name) "\b")))

(defun go-run-current-sub-test ()
  "Run the current sub test."
  (interactive)
  (go-test (concat (go-current-function-name) "\b/" (go-sub-test-name) "\b/")))



;;;"^func[[:space:]]\\(Test.+\\)("


;;; init.el ends here
