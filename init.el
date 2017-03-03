
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

;; (require 'ido)
;; (ido-mode t)
;; (ido-everywhere t)
;; (require 'flx-ido)
;; (flx-ido-mode t)
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)
;; (require 'ido-ubiquitous)
;; (ido-ubiquitous-mode t)
;; (require 'ido-vertical-mode)
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)
;; (ido-vertical-mode t)
(ivy-mode 1)
;;(setq ivy-use-virtual-buffers t)
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

;; set default theme
;;(load-theme 'zenburn t)
(load-theme 'solarized-dark t)
;; set default font
(set-default-font "Inconsolata 14")
;; projectile
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
;; project explorer toggle
(require 'neotree)
(global-set-key (kbd "C-.") 'neotree-toggle)

;;(global-set-key (kbd "C-c .") 'project-explorer-toggle)
(setq projectile-completion-system 'ivy)

;; move-text
;;(require 'move-text)
;;(move-text-default-bindings)
;; multi cursor
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; comment and uncomment-lines, "M ;"
(evilnc-default-hotkeys)

(require 'rainbow-delimiters)

;;(global-rainbow-delimiters-mode)

;; prog-mode
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'prog-mode-hook 'aggressive-indent-mode)

(setq-default cursor-type 'bar)
(show-paren-mode 1)
(setq show-paren-delay 0)
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'markdown-mode-hook 'smartparens-mode)

(setq linum-format "%d")
(add-hook 'prog-mode-hook 'linum-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(add-hook 'after-init-hook 'global-company-mode)
;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete/dict")
;; (ac-config-default)

;; ruby-mode
(setq ruby-indent-level 2)
(require 'rvm)
(rvm-use-default)

;; projectile-rails
(require 'projectile-rails)
(define-key projectile-rails-mode-map (kbd "s-m")   'projectile-rails-find-model)
(define-key projectile-rails-mode-map (kbd "s-c")   'projectile-rails-find-controller)
(define-key projectile-rails-mode-map (kbd "s-v")   'projectile-rails-find-view)
(define-key projectile-rails-mode-map (kbd "s-RET") 'projectile-rails-goto-file-at-point)
(define-key projectile-rails-mode-map (kbd "C-c g") 'projectile-rails-mode-goto-map)

;;js2-mode
;;major mode for editing .js
;;ac-js2
;;for other liraries.js check ac-js2 readme
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;(add-hook 'js-mode-hook 'js2-minor-mode)
;;(add-hook 'js2-mode-hook 'ac-js2-mode)
;;(setq ac-js2-evaluate-calls t)

(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
(global-flycheck-mode)

;; Magit rules!
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-completing-read-function 'ivy-completing-read)


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
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "ffc01b1b3a7cc43c6d0f25ff5573c21fe6cdf2e4e6ab0e4667856f1a90b98c60" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4" "19553e40ff93a241cdfce090fd60e1420e0b371eec1d134a0da21f6f6488a74a" "b84c377b535f71f00e76bfc7ea8c1dcf75c459348292b01ed79e2916fb6451f8" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "37def0fac11a4890922af9febc8394e3b6e3c68904a294a2d440b1904e979c7e" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "fa11f855b5f606f84e50106a7360c72aac88fee5f6fb8084aa4329009b61c5a2" "49de25b465bc3c2498bcd4c1575fa0090bd56fc79cdb49b919b49eaea17ee1dd" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (neotree company counsel ivy zenburn-theme yaml-mode web-mode solarized-theme smex smartparens rvm restclient rainbow-delimiters projectile-rails project-explorer php-mode multiple-cursors move-text markdown-mode magit labburn-theme ido-vertical-mode ido-ubiquitous gruvbox-theme flycheck flx-ido evil-nerd-commenter dockerfile-mode auto-indent-mode auto-complete aggressive-indent ac-js2)))
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
