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
  (setq ccls-executable "clangd"
	lsp-prefer-flymake nil
	lsp-enable-snippet nil
	lsp-enable-file-watchers nil
	flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
	 (lambda () (require 'ccls) (lsp))))

(require 'server)
(setq server-use-tcp t)
(if (not (server-running-p)) (server-start))

;; Programming
;;-----------------------------------------------------------------------------
;; Local C/C++ style
;; clang-format -style="{BasedOnStyle: llvm, IndentWidth: 4, AccessModifierOffset: -4}" -dump-config
;;-----------------------------------------------------------------------------
(c-add-style "dky"
	     '("gnu"
	       (c-basic-offset . 4)	; Guessed value
	       (c-offsets-alist
		(access-label . 0)	; Guessed value
		(block-close . 0)	; Guessed value
		(class-close . 0)	; Guessed value
		(cpp-define-intro . +)	; Guessed value
		(defun-block-intro . +)	; Guessed value
		(defun-close . 0)	; Guessed value
		(inclass . +)		; Guessed value
		(inline-close . 0)	; Guessed value
		(member-init-intro . +)	; Guessed value
		(statement . 0)		; Guessed value
		(statement-block-intro . +) ; Guessed value
		(stream-op . 5)		; Guessed value
		(topmost-intro . 0)	; Guessed value
		(annotation-top-cont . 0)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont c-lineup-gcc-asm-reg 0)
		(arglist-cont-nonempty . c-lineup-arglist)
		(arglist-intro . c-lineup-arglist-intro-after-paren)
		(block-open . 0)
		(brace-entry-open . 0)
		(brace-list-close . 0)
		(brace-list-entry . 0)
		(brace-list-intro first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
		(brace-list-open . +)
		(c . c-lineup-C-comments)
		(case-label . 0)
		(catch-clause . 0)
		(class-open . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-macro . -1000)
		(cpp-macro-cont . +)
		(defun-open . 0)
		(do-while-closure . 0)
		(else-clause . 0)
		(extern-lang-close . 0)
		(extern-lang-open . 0)
		(friend . 0)
		(func-decl-cont . +)
		(incomposition . +)
		(inexpr-class . +)
		(inexpr-statement . +)
		(inextern-lang . +)
		(inher-cont . c-lineup-multi-inher)
		(inher-intro . +)
		(inlambda . 0)
		(inline-open . 0)
		(inmodule . +)
		(innamespace . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . 5)
		(label . 0)
		(lambda-intro-cont . +)
		(member-init-cont . c-lineup-multi-inher)
		(module-close . 0)
		(module-open . 0)
		(namespace-close . 0)
		(namespace-open . 0)
		(objc-method-args-cont . c-lineup-ObjC-method-args)
		(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		(objc-method-intro .
				   [0])
		(statement-case-intro . +)
		(statement-case-open . +)
		(statement-cont . +)
		(string . -1000)
		(substatement . +)
		(substatement-label . 0)
		(substatement-open . +)
		(template-args-cont c-lineup-template-args +)
		(topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont))))

(delete-selection-mode +1)
(global-display-fill-column-indicator-mode +1)

;; Hooks
(add-hook 'lsp-mode-hook 'which-func-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'find-file-hook 'vc-registered-readonly)
(add-hook 'c-mode-common-hook (lambda ()
				(define-key c-mode-base-map "\C-q" 'comment-region)
				(define-key c-mode-base-map "\C-uq" 'uncomment-region)
				(c-set-style "dky")))

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
