;; start emacs server so I can hack os x to start files from the Finder
;; probobably this will come in handy for other stuff in the future too
(server-start)

;; Who needs a toolbar?
(tool-bar-mode 0)

;; Need some place to put elisp packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; Turn on the mouse and visual region highlighting
(custom-set-variables
'(mouse-wheel-mode t nil (mwheel))
'(transient-mark-mode (quote identity)))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Display column numbers
(setq column-number-mode t)

;; Stop error bell
(setq visible-bell t)

(provide 'mwt-global)