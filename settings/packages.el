;; settings/packages.el --- Emacs configuration of Chuan Su
;;; Copyright (c) 2016 Chuan Su <chuan.su@outlook.com>

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(defvar required-packages
  '(
    solarized-theme
    zenburn-theme
    use-package
    pbcopy
    avy
    ivy
    counsel
    company
    ibuffer
    neotree
    reveal-in-osx-finder
    exec-path-from-shell
    multiple-cursors
    smartparens
    rainbow-delimiters
    magit
    restclient
    company-restclient
    projectile
    dockerfile-mode
    flycheck
    js2-mode
    rvm
    web-mode
    emmet-mode
    yaml-mode
    markdown-mode
    expand-region
    visual-regexp
    move-text
    goto-chg
    which-key
    erlang
    evil
    evil-surround
    evil-nerd-commenter
    go-mode
    vue-mode
    php-mode
    ) "required packages.")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(provide 'packages)

;; end settings/packages.el
