
;; Enable backtrace
;; (setq debug-on-error t)

;; Need some place to put elisp packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; personal customizations
(require 'mwt-global)

