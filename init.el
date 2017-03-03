
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;;(package-refresh-contents)


;;list the packages you want
;;(setq package-list '(ac-js2 auto-complete auto-indent-mode smex flx-ido ido-ubiquitous ido-vertical-mode dockerfile-mode
 ;;flycheck js2-mode php-mode rvm  magit move-text projectile projectile-rails project-explorer zenburn-theme  web-mode yaml-mode evil-nerd-commenter multiple-cursors smartparens
 ;;markdown-mode restclient rainbow-delimiters
;;))

; list the repositories containing them
;(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
 ;                        ("melpa" . "http://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)



; fetch the list of packages available
;;(unless package-archive-contents
  ;;(package-refresh-contents))

; install the missing packages
;;(dolist (package package-list)
  ;;(unless (package-installed-p package)
    ;;(package-install package)))


;; set default theme
(load-theme 'solarized-dark t)
;; set default font
(set-default-font "Inconsolata 14")
;; neotree
(require 'neotree)
(global-set-key (kbd "C-.") 'neotree-toggle)

(ivy-mode 1)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)


(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "M-g")   'goto-line)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<C-tab>") 'bury-buffer)
;; windows switch
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; comment and uncomment-lines, "M ;"
(evilnc-default-hotkeys)

(require 'rainbow-delimiters)
;; prog-mode
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(setq linum-format "%d")
(add-hook 'prog-mode-hook 'linum-mode)
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
;; company
(add-hook 'after-init-hook 'global-company-mode)

;; Magit rules!
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-completing-read-function 'ivy-completing-read)


;;(add-hook 'prog-mode-hook 'aggressive-indent-mode)

(setq-default cursor-type 'bar)
(show-paren-mode 1)
(setq show-paren-delay 0)
(add-hook 'markdown-mode-hook 'smartparens-mode)


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
(global-flycheck-mode)

;; ruby-mode
(setq ruby-indent-level 2)
(require 'rvm)
(rvm-use-default)

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'ivy)

;; projectile-rails
(require 'projectile-rails)
(define-key projectile-rails-mode-map (kbd "s-m")   'projectile-rails-find-model)
(define-key projectile-rails-mode-map (kbd "s-c")   'projectile-rails-find-controller)
(define-key projectile-rails-mode-map (kbd "s-v")   'projectile-rails-find-view)
(define-key projectile-rails-mode-map (kbd "s-RET") 'projectile-rails-goto-file-at-point)
(define-key projectile-rails-mode-map (kbd "C-c g") 'projectile-rails-mode-goto-map)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;;js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(setq web-mode-enable-css-colorization t)
(setq web-mode-ac-sources-alist
  '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
    ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-pairing t)
;;(setq web-mode-enable-current-column-highlight t)
;; Custom web-mode colors
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(web-mode-current-element-highlight-face ((t (:foreground "#FF8A4B"))))
 ;;'(web-mode-html-tag-bracket-face ((t (:foreground "#ffffff"))))
 ;;'(web-mode-html-tag-face ((t (:foreground "#f92672")))))


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
    (default)))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    ()))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote left)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-current-element-highlight-face ((t (:background nil :foreground "#FF8A4B")))))
