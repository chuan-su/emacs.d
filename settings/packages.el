;; settings/packages.el --- Emacs configuration of Chuan Su
;;; Copyright (c) 2016 Chuan Su <chuan.su@outlook.com>

;;; Code:

(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(defvar required-packages
  '(
    solarized-theme
    zenburn-theme
    use-package
    pbcopy
    nlinum
    avy
    ivy
    counsel
    company
    ibuffer
    neotree
    reveal-in-osx-finder
    multiple-cursors
    smartparens
    rainbow-delimiters
    magit
    restclient
    company-restclient
    projectile
    dockerfile-mode
    scala-mode
    flycheck
    js2-mode
    php-mode
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
    golden-ratio
    ) "required packages.")

; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'packages)

;; end settings/packages.el
