;;; init.el --- Emacs configuration of Chuan Su
;;; Copyright (c) 2016 Chuan Su <chuan.su@outlook.com>

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil
      ;; work around package.el bug in Emacs 25
      package--init-file-ensured t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;;(package-refresh-contents)


;list the packages you want
(setq package-list '(
  solarized-theme
  nlinum
  avy
  flx
  swiper
  ivy
  counsel
  company
  ibuffer
  neotree
  reveal-in-osx-finder
  evil-nerd-commenter
  multiple-cursors
  smartparens
  rainbow-delimiters
  magit
  restclient
  company-restclient
  projectile
  projectile-rails
  dockerfile-mode
  flycheck
  js2-mode
  php-mode
  rvm
  web-mode
  yaml-mode
  markdown-mode
))

;list the repositories containing them
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                        ("melpa" . "http://melpa.org/packages/")))




;;fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;;install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;activate all the packages (in particular autoloads)
(package-initialize)

(defvar user-cache-directory (expand-file-name ".cache"  user-emacs-directory))
(make-directory user-cache-directory t)

;; Some global keybindings
(global-set-key (kbd "C-j") #'join-line)
(global-set-key (kbd "M-g") #'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<C-tab>") 'bury-buffer)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq-default cursor-type 'bar)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(evilnc-default-hotkeys) ;; comment and uncomment-lines, "M ;"



;; set default theme

(use-package solarized                  ; My colour theme
  :disabled t
  :ensure solarized-theme
  :config
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

  (load-theme 'solarized-dark 'no-confirm))

;;set default theme
(load-theme 'solarized-dark t)
;; set default font
(set-frame-font "Inconsolata 14")
(add-to-list 'default-frame-alist
               (cons 'font "Inconsolata 14"))

; Auto-revert buffers of changed files
(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil         ; Shut up, please!
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)
  )

;; neotree
(use-package neotree
  :ensure t
  :bind (("C-." . neotree-toggle))
  :config (setq neo-window-width 32
                neo-create-file-auto-open t
                neo-banner-message nil
                neo-show-updir-line t
                neo-mode-line-type 'neotree
                neo-smart-open t
                neo-show-hidden-files t
                neo-auto-indent-point t))

;; Reveal current buffer in finder
(use-package reveal-in-osx-finder
  :ensure t
  ;; Bind analogous to `dired-jump' at C-c f j
  :bind (("C-c f J" . reveal-in-osx-finder)))


(use-package flx
  :ensure t)

(use-package avy-jump                   ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c j j" . avy-goto-char)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)
         ("C-c j b" . avy-pop-mark)
         ("C-c j k" . avy-goto-char-2)
         ("C-c j i" . avy-goto-char-in-line)))

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-M-i" . counsel-imenu)
   ("C-x C-f" . counsel-find-file)
   ("C-c d" . counsel-dired-jump))
  :config
  (ivy-mode 1)
  (setq counsel-find-file-at-point t)
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done))

(use-package swiper
  :ensure t
  :defer nil
  :init (ivy-mode 1)
  :config
  (global-set-key "\C-s" 'swiper)
  (setf ivy-wrap t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (define-key ivy-minibuffer-map (kbd "C-s") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-r") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-l")
    (lambda ()
      "Be like like Helm."
      (interactive)
      (unless (eql (char-before) ?/)
        (ivy-backward-kill-word))
      (ivy-backward-delete-char))))


(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("C-c w <left>"  . windmove-left)
         ("C-c w <right>" . windmove-right)
         ("C-c w <up>"    . windmove-up)
         ("C-c w <down>"  . windmove-down))
  :config (windmove-default-keybindings 'shift))


(use-package multiple-cursors           ; Edit text with multiple cursors
  :ensure t
  :bind (("C-<". mc/mark-previous-like-this)
         ("C->". mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C-," . mc/mark-all-like-this))
  :config
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))


;;(require 'rainbow-delimiters)
;; prog-mode
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

; Line numbers in display margin
(use-package nlinum
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode)
  :bind (("C-c t l" . nlinum-mode)))

(use-package smartparens
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'markdown-mode-hook 'smartparens-mode)
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
  :bind (("C-x g"   . magit-status)))

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
    (add-hook 'projectile-mode-hook 'projectile-rails-on)
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

(use-package projectile-rails
  :config
  (progn
    (define-key projectile-rails-mode-map (kbd "s-m")   'projectile-rails-find-model)
    (define-key projectile-rails-mode-map (kbd "s-c")   'projectile-rails-find-controller)
    (define-key projectile-rails-mode-map (kbd "s-v")   'projectile-rails-find-view)
    ))


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

(use-package web-mode
  :defer t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode)))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-style-padding 2)
                        (setq web-mode-script-padding 2)
                        (setq web-mode-enable-css-colorization t)
                        (setq web-mode-enable-current-element-highlight t)
                        (setq web-mode-enable-auto-pairing t)))))

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

;; pretty prints the selection on a json document
;; uses python.
;; adjust the python path and executable.
;; see http://stackoverflow.com/questions/1548605/emacs-lisp-shell-command-on-region
(defun pretty-print-json(&optional b e)
  (interactive "r")
  (shell-command-on-region b e "python -m json.tool" (current-buffer) t)
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(inhibit-startup-screen t)
 '(package-selected-packages nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote left)))

;; Custom web-mode colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-current-element-highlight-face ((t (:background nil :foreground "magenta")))))

;;; init.el ends here
