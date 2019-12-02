;;; package --- Minimal Emacs init file

;;; Commentary:
;;; Simple Emacs setup for C/C++ development using language server

;;; Helper functions

;; Jump to matching paren
(defun match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

;; Do not store the absolute path for a symlink
(defun recentf-clear(regx)
  (interactive "MRegex for cleanup: ")
  (let ((orig-recentf-exclude recentf-exclude))
    (setq recentf-exclude (list regx))
    (recentf-cleanup)
    (setq recentf-exclude orig-recentf-exclude))
  ;; Cleanup the file open history
  (let ((files file-name-history))
    (setq file-name-history nil)
    (while files
      (let ((file (car files)))
	(setq files (cdr files))
	(if (not (string-match-p regx file))
	    (add-to-list 'file-name-history file))))))

(defun vc-registered-readonly()
  (interactive)
  (if (vc-registered buffer-file-name)
      (read-only-mode t)))

;;; Customizations

(defconst *home (expand-file-name (concat "~" init-file-user)))
(setq user-mail-address (concat (user-login-name) "@atlassian.com"))
(add-to-list 'load-path (concat *home "/.site-lisp/"))

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(setq initial-frame-alist '((width . 120) (height . 40)))
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(set-frame-font "Monaco 13" nil t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(line-number-mode +1)
(column-number-mode t)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)

;; Backup and refresh on changes
(add-to-list 'backup-directory-alist
	     `("*" . (concat *home "/.backup")))
(global-auto-revert-mode t)

(use-package cc-mode
  :ensure t
  :hook ((c-mode c++-mode) . (lambda () (show-paren-mode t)))
  :bind (("C-%" . match-paren)))

(use-package expand-region
  :ensure t
  :bind ("M-m" . er/expand-region))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package magit
  :ensure t
  :bind (("C-M-g" . magit-status)))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))

(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 15)
  (add-to-list 'recentf-exclude
	       (lambda (p) (not (string-match-p "^/Users/" p))))
  (recentf-mode +1)
  (recentf-load-list)
  (global-set-key "\C-xc" 'recentf-clear)
  (global-set-key "\C-xo" 'recentf-open-files))

(use-package crux
  :preface
  (require 'recentf)
  :ensure t
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-c n" . crux-cleanup-buffer-or-region)
  ("C-c f" . crux-recentf-find-file)
  ("C-a" . crux-move-beginning-of-line)
  ([home] . crux-move-beginning-of-line))

(use-package cc-mode
  :ensure t
  :config
  (setq-default indent-tabs-mode t))

(use-package go-mode
  :ensure t
  :defer nil
  :bind (
	 ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
	 ;; uncomment the following lines
	 ;; ("C-c C-j" . lsp-find-definition)
	 ;; ("C-c C-d" . lsp-describe-thing-at-point)
	 )
  :hook ((go-mode . lsp-deferred)
	 (before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind (
	 ("C-t" . pop-tag-mark)
	 ("C-]" . lsp-find-definition)
	 ("C-r" . lsp-find-references)
	 )
  :config
  (add-hook 'go-mode 'lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
	lsp-ui-sideline-enable nil)
  :ensure t)

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config (push 'company-lsp company-backends))

(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "ccls"
	lsp-prefer-flymake nil
	lsp-enable-snippet nil
	lsp-enable-file-watchers nil
	flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
	 (lambda () (require 'ccls) (lsp))))

;; (mapcar (lambda (mode)
;;	  (define-key mode [(control ?t)] 'pop-tag-mark)
;;	  (define-key mode [(control ?\])] 'lsp-find-definition)
;;	  (define-key mode [(control ?r)] 'lsp-find-references))
;;	(list lsp-mode-map))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(require 'server)
(setq server-use-tcp t)
(if (not (server-running-p)) (server-start))

;; Programming
(setq c-default-style "linux")
(delete-selection-mode +1)
(global-display-fill-column-indicator-mode +1)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'find-file-hook 'vc-registered-readonly)

;; Useful key bindings
(global-set-key [f2] 'save-buffer)
(global-set-key [f4] 'other-window)
(global-set-key [f9] 'kill-this-buffer)
(global-set-key [end] 'end-of-line)
(global-set-key [backspace] '(lambda ()
			       (interactive)
			       (if (region-active-p)
				   (delete-active-region)
				 (c-hungry-backspace))))

(provide 'init)
;;; init ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(package-selected-packages
   '(flycheck go-mode yaml-mode crux lsp-mode lsp-ui ccls which-key use-package smartparens magit expand-region company company-lsp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
