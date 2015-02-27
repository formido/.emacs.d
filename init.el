
;; Enable backtrace
;; (setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/theirs")

(when (>= emacs-major-version 24)
  (require 'package)
  (require 'package-helper)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  (with-package magit*)
  (with-package simple-httpd)
  (with-package gist*)
  (with-package web-mode*
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (defun web-mode-hooks ()
      "Hooks for web-mode."
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-enable-engine-detection t)
      )
    (add-hook 'web-mode-hook 'web-mode-hooks))
  (with-package js2-mode*
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
  (with-package (skewer-mode* skewer-repl* sgml-mode* css-mode*)
  		(skewer-setup))
  )

;; Need some place to put elisp packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/theirs"))

;; personal customizations
(require 'mwt-global)


;; Ahhh, yes, now I can work like I used to in 
;; Satimage's Smile :) (No pun intended)
(defun skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browsers."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (toggle-truncate-lines t)))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-mode t nil (mwheel))
 '(quack-default-program "~/bin/plt/bin/mzscheme")
 '(quack-programs (quote ("~/bin/plt/bin/mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M
    errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-remember-new-programs-p t)
 '(regex-tool-backend (quote perl))
 '(safe-local-variable-values (quote ((engine . django))))
 '(transient-mark-mode (quote identity)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
