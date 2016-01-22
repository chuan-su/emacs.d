(require 'package)
(add-to-list 'package-archives   
             '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

; list the packages you want
(setq package-list '(ac-emmet ac-js2 auto-complete auto-indent-mode autopair dash dockerfile-mode
  emmet-mode epl exec-path-from-shell flycheck git-commit js2-mode let-alist 
  magit monokai-theme move-text pkg-info popup projectile simple-httpd skewer-mode sublime-themes 
  web-beautify web-mode yaml-mode))

; list the repositories containing them
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(load-theme 'spolsky t)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq-default cursor-type 'bar)

;; auto-pair
;; to enable in all buffers
(require 'autopair)
(autopair-global-mode)

;;(require 'auto-indent-mode)
;;(setq auto-indent-assign-indent-level 4)
;;(auto-indent-global-mode) ;; auto-indent-mode

(setq linum-format "%d")
(add-hook 'prog-mode-hook 'linum-mode)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(require 'move-text)
(move-text-default-bindings) ;; move-text

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete/dict")
(ac-config-default)

(projectile-global-mode)
;;js2-mode
;;major mode for editing .js
;;ac-js2
;;for other liraries.js check ac-js2 readme
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'ac-modes 'erlang-mode)

;;(add-hook 'js-mode-hook 'js2-minor-mode)
;;(add-hook 'js2-mode-hook 'ac-js2-mode)
;;(setq ac-js2-evaluate-calls t)

(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
(setq magit-last-seen-setup-instructions "1.4.0")

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(setq web-mode-enable-css-colorization t)
(setq web-mode-ac-sources-alist
  '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
    ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(require 'web-beautify) ;; Not necessary if using ELPA package
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

;;(setq load-path (cons"/usr/local/Cellar/erlang/17.5/lib/erlang/lib/tools-2.7.2/emacs" load-path))
;;(setq erlang-root-dir "/usr/local/Cellar/erlang/17.5/lib/erlang/lib")
;;(setq exec-path (cons "/usr/local/Cellar/erlang/17.5/lib/erlang/bin" exec-path))
;;(require 'erlang-start)

;;(defun customizations-for-erlang-mode ()
  ;;(linum-mode t)  
;;)

;;(add-hook 'erlang-mode-hook 'customizations-for-erlang-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote left)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
