
;; Enable backtrace
;; (setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/theirs")

(when (>= emacs-major-version 24)
  (require 'package)
  (require 'package-helper)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (with-package simple-httpd)
  (with-package js2-mode*
	      (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
  ;; (with-package (skewer-mode skewer-repl sgml-mode css-mode js2-mode)
  ;; 		(skewer-setup))
  )

(add-to-list 'load-path "~/.emacs.d/theirs/skewer-mode")

(require 'skewer-mode)
(autoload 'skewer-mode "skewer-mode")

(require 'skewer-setup)
(autoload 'skewer-setup "skewer-setup")
(skewer-setup)

(require 'skewer-repl)
(autoload 'skewer-repl "skewer-repl")


(require 'js2-mode)

;; Need some place to put elisp packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/theirs"))

;; personal customizations
(require 'mwt-global)

(put 'dired-find-alternate-file 'disabled nil)

;; Ahhh, yes, now I can work like I used to in 
;; Satimage's Smile :) (No pun intended)
(defun skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browsers."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))
