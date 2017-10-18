;; settings/editor.el --- Emacs configuration of Chuan Su
;;; Copyright (c) 2016 Chuan Su <chuan.su@outlook.com>

;;; Code:

;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; set margin widths
(setq-default right-margin-width 1)
(unless (display-graphic-p)
  ;; set margin left for terminal mode
  (setq-default left-margin-width 1)
  )

;; Get rid of Welcome screeen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Life is Good")

;; encoding - Set preferred encoding system as UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Other settings
(setq tramp-default-method "ssh")
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq-default cursor-type 'bar)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)
;;(setq-default global-visual-line-mode t)

;; Set default font
(add-to-list 'default-frame-alist '(font . "Fira Code 12")) ;; "Andale Mono 12","Inconsolata 14"
(set-default-font "Fira Code 12")

;; Key bindings
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c {") 'previous-buffer)
(global-set-key (kbd "C-c }") 'next-buffer)

;; hooks
(add-hook 'before-save-hook 'whitespace-cleanup) ;; clean white space before save
(add-hook 'sql-interactive-mode-hook(lambda () (toggle-truncate-lines t))) ;; sql

(provide 'editor)

;; end settings/editor.el
