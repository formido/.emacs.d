
;; Enable backtrace
;; (setq debug-on-error t)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; Need some place to put elisp packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/theirs"))

;; personal customizations
(require 'mwt-global)

(put 'dired-find-alternate-file 'disabled nil)
