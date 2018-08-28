;;; init.el --- Emacs configuration of Chuan Su
;;; Copyright (c) 2016 Chuan Su <chuan.su@outlook.com>

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defvar user-cache-directory
  (expand-file-name ".cache"  user-emacs-directory))

(defvar user-backup-directory
  (expand-file-name ".backup"  user-emacs-directory))

(defvar user-settings-directory
  (expand-file-name "settings" user-emacs-directory))

(setq backup-directory-alist
      `(("." . ,user-backup-directory)))

;; Keep emacs Custom-settings in separate file
(setq custom-file
      (expand-file-name "custom.el" user-settings-directory)) ;; keep emacs custom-settings in separate file

(make-directory user-cache-directory  t)
(make-directory user-backup-directory t)

(add-to-list 'load-path user-settings-directory)
(load custom-file)

(require 'editor)
(require 'packages)
(require 'use-package)

;;favorite theme
(use-package solarized
  :if (display-graphic-p)
  :config
  (progn
    ;; Disable variable pitch fonts in Solarized theme
    (setq solarized-use-variable-pitch nil
          ;; Prefer italics over bold
          solarized-use-less-bold t
          solarized-use-more-italic t
          solarized-distinct-doc-face t ; Emphasize docstrings
          ;; I find different font sizes irritating.
          solarized-height-minus-1 1.0
          solarized-height-plus-1 1.0
          solarized-height-plus-2 1.0
          solarized-height-plus-3 1.0
          solarized-height-plus-4 1.0)
    (load-theme 'solarized-dark 'no-confirm)))

(use-package zenburn-theme
  :if (not (display-graphic-p))
  :init (load-theme 'zenburn 'no-confirm))

;; (use-package zenburn-theme
;;   :config
;;   (progn
;;     (load-theme 'zenburn 'no-confirm)))

(use-package fringe
  :config
  (set-fringe-mode '(8 . 0)))

;; which-key
(use-package which-key
  :ensure t
  :config
  (progn
    (setq which-key-popup-type 'side-window) ;;Default
    ;; (setq which-key-popup-type 'minibuffer)
    (setq which-key-compute-remaps t) ;Show correct descriptions for remapped keys
    (setq which-key-allow-multiple-replacements t) ;Default = nil))
    )
  (which-key-mode))

;; move-text M-up / M-down
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; ; Auto-revert buffers of changed files
(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil         ; Shut up, please!
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)
  )

(use-package neotree
  :ensure t
  :bind (("C-c ." . neotree-toggle))
  :config (setq neo-window-width 32
                neo-create-file-auto-open t
                neo-banner-message nil
                neo-show-updir-line t
                neo-mode-line-type 'neotree
                neo-smart-open t
                neo-show-hidden-files t
                neo-auto-indent-point t))

;; Mac only, reveal current buffer in finder
(use-package reveal-in-osx-finder
  :if (memq window-system '(mac ns))
  :ensure t
  ;; Bind analogous to `dired-jump' at C-c f j
  :bind (("C-c f J" . reveal-in-osx-finder)))

;; Mac only
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Mac only
(use-package pbcopy
  :if (memq window-system '(mac ns))
  :config
  (turn-on-pbcopy))

(use-package avy-jump              ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c j i" . avy-goto-char-in-line)
         ("C-c j j" . avy-goto-char)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)
         ("C-c j b" . avy-pop-mark)
         ("C-c j k" . avy-goto-char-2)))

(use-package goto-chg
  :ensure t
  :bind (("C-c j p" . goto-last-change)
         ("C-c j n" . goto-last-change-reverse)))

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("M-s" . counsel-grep)
   ("C-M-i" . counsel-imenu)
   ("C-x C-f" . counsel-find-file)
   ("C-c d" . counsel-dired-jump)
   ("C-c g g". counsel-git-grep)
   ("C-c g f". counsel-git))
  :config
  (ivy-mode 1)
  (setq counsel-find-file-at-point t)
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done))


(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("C-x w <left>"  . windmove-left)
         ("C-x w <right>" . windmove-right)
         ("C-x w <up>"    . windmove-up)
         ("C-x w <down>"  . windmove-down))
  :config (windmove-default-keybindings 'shift))


(use-package multiple-cursors           ; Edit text with multiple cursors
  :ensure t
  :bind (("M-p". mc/mark-previous-like-this)
         ("M-n". mc/mark-next-like-this)
         ("M-a" . mc/mark-all-like-this)
         ("C-c e". mc/edit-lines))
  :config
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))
(use-package expand-region
  :ensure t
  :bind (("C-c >" . er/expand-region)))

(use-package visual-regexp
  :ensure t
  :init
  (use-package visual-regexp-steroids :ensure t)
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark) ; Need multiple cursors
         ("C-M-r" . vr/isearch-backward)
         ("C-M-s" . vr/isearch-forward)))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

;; Line numbers in display margin
(use-package nlinum
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'erlang-mode-hook 'nlinum-mode)
  :bind (("C-c t l" . nlinum-mode)))

(use-package smartparens-config
  :ensure smartparens
  :bind (:map smartparens-mode-map
              ("C-M-a"  . sp-beginning-of-sexp)
              ("C-M-e"  . sp-end-of-sexp)
              ("C-M-f"  . sp-forward-sexp)
              ("C-M-b"  . sp-backward-sexp)
              ("C-M-n"  . sp-next-sexp)
              ("C-M-p"  . sp-previous-sexp)
              ("C-M-k"  . sp-kill-sexp)
              ("M-k"    . sp-backward-kill-sexp)
              ("M-["    . sp-backward-unwrap-sexp)
              ("M-]"    . sp-unwrap-sexp))
  :init
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)
  :config
  (progn
    (show-smartparens-global-mode t))
  :diminish smartparens-mode)

(use-package company
  :ensure t
  :diminish company-mode
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)
  :diminish company-mode)

(use-package restclient                 ; ReST REPL for Emacs
  :ensure t
  :defer t)

(use-package company-restclient         ; Company support for restclient
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-restclient))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-x m"   . magit-status)))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (setq dired-auto-revert-buffer t    ; Revert on re-visiting
        ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
        ;; uses human-readable sizes, and `-F' appends file-type classifiers
        ;; to file names (for better highlighting)
        dired-listing-switches "-alhF"
        dired-ls-F-marks-symlinks t   ; -F marks links with @
        ;; Inhibit prompts for simple recursive operations
        dired-recursive-copies 'always
        ;; Auto-copy to other Dired split window
        dired-dwim-target t)

  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
    ;; `--group-directories-first' lists directories before files, and `-v'
    ;; sorts numbers in file names naturally, i.e. "image1" goes before
    ;; "image02"
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v"))))

(use-package paren
  :config (show-paren-mode))

(use-package flycheck
  :ensure t
  :bind (("C-c t f" . global-flycheck-mode)))

(use-package projectile
  :ensure t
  :bind (([remap compile] . projectile-compile-project))
  :init (projectile-global-mode)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-completion-system 'helm
        projectile-find-dir-includes-top-level t)

  (defun lunaryorn-neotree-project-root (&optional directory)
    "Open a NeoTree browser for a project DIRECTORY."
    (interactive)
    (let ((default-directory (or directory default-directory)))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (neotree-find (projectile-project-root)))))

  (progn
    (bind-keys
     :map projectile-mode-map
     ("C-x p f" . projectile-find-file)
     ("C-c p d" . projectile-dired)
     ("C-c p D" . projectile-find-dir))
    (setq projectile-known-projects-file (expand-file-name  "projectile-bookmarks.eld" user-cache-directory)
          projectile-cache-file (expand-file-name  "projectile.cache" user-cache-directory))
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ivy)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store"))

  :diminish projectile-mode)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package js2-mode                   ; Powerful Javascript mode
  :ensure t
  :defer t
  :interpreter ("node"   . js2-mode)
  :mode (("\\.js\\'"     . js2-mode)
         ("\\.jsx\\'"    . js2-jsx-mode)
         ("\\.json$"     . js-mode)
         ("\\.template$" . json-mode)
         ("Jakefile$"    . js2-mode))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2")))
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
                               (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))
    (setq js2-skip-preprocessor-directives t
          js2-mode-show-parse-errors nil
          js2-mode-show-strict-warnings nil
          js2-highlight-level 3           ; Try to highlight most ECMA built-ins
          )
    (setq-default js2-additional-externs
                  '("$" "unsafeWindow" "localStorage" "jQuery"
                    "setTimeout" "setInterval" "location" "skewer"
                    "console" "phantom"))))
(use-package php-mode
  :ensure t
  :mode "\\.php[345]?\\'"
  :init
  (add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)
  )

(use-package groovy-mode
  :ensure t
  :mode (("\\.gradle\\'"     . groovy-mode)
         ("\\.groovy\\'"    . groovy-mode))
  :init
  (add-hook 'groovy-mode-hook (lambda ()
                              (setq c-basic-offset 2)))
  )

(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"
  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode))

(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

(use-package erlang
  :ensure t
  ;; We need to specify erlang-mode explicitely as the package is not called
  ;; erlang-mode.
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)
         ("\\.xrl\\'" . erlang-mode)
         ("sys\\.config\\'" . erlang-mode)
         ("rebar\\.config\\'" . erlang-mode)
         ("\\.app\\(\\.src\\)?\\'" . erlang-mode))
  :config
  (setq erlang-indent-level 2)
  (progn
    (add-hook 'erlang-mode-hook 'flycheck-mode)))

(use-package web-mode
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode)))
  :config
  (progn
    (add-hook 'web-mode-hook
              (lambda ()
                (setq web-mode-style-padding 2)
                (setq web-mode-script-padding 2)
                (setq web-mode-enable-css-colorization t)
                (setq web-mode-enable-current-element-highlight t)
                (setq web-mode-markup-indent-offset 2)
                (setq web-mode-enable-auto-pairing t)))))
;; emmet
(use-package emmet-mode
  :ensure t
  :commands (emmet-expand-line emmet-expand)
  :defer 2
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode))
  :config
  (progn
    (bind-key "C-j" 'emmet-expand-line emmet-mode-keymap)
    (bind-key "<C-return>" 'emmet-expand emmet-mode-keymap)
    (setq emmet-indentation 2)))

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq-local paragraph-separate ".*>-$\\|[   ]*$")
              (setq-local paragraph-start paragraph-separate))))

(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mkd\\'" . markdown-mode)
         (".simplenote\\'" . markdown-mode)))

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :ensure t)

;; pretty prints the selection on a json document
;; uses python.
;; adjust the python path and executable.
;; see http://stackoverflow.com/questions/1548605/emacs-lisp-shell-command-on-region
(defun pretty-print-json(&optional b e)
  (interactive "r")
  (shell-command-on-region b e "python -m json.tool" (current-buffer) t)
  )

;; org mode
(use-package org
  :defer t
  :ensure org-plus-contrib
  :config
  (progn
    (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"")
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
    ))

;;; init.el ends here
