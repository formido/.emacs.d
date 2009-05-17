;; If we're in a debug instance of Emacs, load default color-theme
;; to differentiate it
(if (not debug-on-error) 
    (progn 
      (require 'color-theme)
      (color-theme-initialize)
      (load "theirs/twilight-emacs/color-theme-twilight.el")
      (color-theme-twilight)))

;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; Set up ocaml
(add-to-list 'load-path (expand-file-name "~/.emacs.d/theirs/ocaml-mode-3.05"))
(setq auto-mode-alist
      (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(require 'caml)
(require 'inf-caml)

;; Set up Haskell
(load "~/.emacs.d/theirs/haskell-mode-2.4/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;; Set up Anything
(setq anything (expand-file-name "~/.emacs.d/theirs/anything"))
(add-to-list 'load-path anything)
(require 'anything-config)   
(global-set-key (kbd "C-2") 'anything)  
(global-set-key (kbd "M-2") 'universal-argument)   
(setq anything-sources '(anything-c-source-buffers+   
			 anything-c-source-locate   
			 anything-c-source-recentf   
			 anything-c-source-org-headline   
			 anything-c-source-buffer-not-found)) 

;; Turn off backups.
(setq make-backup-files nil) 
(setq backup-inhibited t)

;; Turn off autosaves.
(setq auto-save-mode nil)
(setq auto-save-default nil)

;; start emacs server so I can hack os x to start files from the Finder
;; probobably this will come in handy for other stuff in the future too
(server-start)

;; function to load files with error checking
(autoload 'safe-load "safe-load")

;; Who needs a toolbar?
(if (boundp 'tool-bar-mode)
    (tool-bar-mode 0))

;; Makes it easier to call what used to be M-
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Enable backward kill word for faster typing
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

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

;; Make sure parentheses match when writing file
(add-hook 'emacs-lisp-mode-hook 'my-prefhand--emacs-lisp-mode)
(defun my-prefhand--emacs-lisp-mode ()
  (add-hook 'local-write-file-hooks 'check-parens))

;; Set up jabber support
(add-to-list 'load-path (expand-file-name "~/.emacs.d/theirs/emacs-jabber"))
(require 'jabber)

;; Load GitHub's semi-official Emacs-pastie integration mode
(load "theirs/gist.el")

;; Load textile mode. Needs improvement, though.
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))
(add-to-list 'auto-mode-alist '("*github\\.com*" . textile-mode))

;; Load wikipedia mode.
;; (require 'wikipedia-mode)
;; (add-to-list 'auto-mode-alist '("\\terry\\.com" . wikipedia-mode))

;; NXHTML-MODE and NXML-MODE -------------------------------------------------
(load "theirs/nxml/autostart.el")

;; So we can test for which platform we're on
(setq platform-mac? (string-match "powerpc" system-configuration))
;;TODO: Update this line for home
(setq platform-ntemacs? (string-match "mingw" system-configuration))
(setq platform-cygemacs? (string-match "pc-cygwin" system-configuration))
(setq platform-linux? (string-match "linux" system-configuration))

;; Load lorem ipsum generator
(load "theirs/lorem-ipsum.el")

;; Platform specific configurations
(cond
 (platform-linux?
  
  ;; Set up er -------------------------------------------------------------
  (setq erlang-root-dir "/usr/local/lib/erlang")
  (setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.6.4" load-path))
  (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
  (require 'erlang-start)

  ;;Keeps emacs from hanging for 60s when using io commands
  ;;http://bob.pythonmac.org/archives/2007/03/14/erlang-mode-for-emacs/
  (defvar inferior-erlang-prompt-timeout t)

  ;; set up distel
  (require 'distel)
  (distel-setup)

  ;; Some Erlang customizations
  (add-hook 'erlang-mode-hook
	    (lambda ()
	      ;; when starting an Erlang shell in Emacs, default in the node name
	      (setq inferior-erlang-machine-options '("-sname" "emacs"))
	      ;; add Erlang functions to an imenu menu
	      (imenu-add-to-menubar "imenu")))

  (message "Loaded Linux configuration..."))

  
 (platform-mac?

  (autoload 'markdown-mode "theirs/markdown-mode.el"
   "Major mode for editing Markdown files" t)
  (setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

  ;; Set up scheme -------------------------------------------------------------
  (setq scheme-program-name "~/bin/plt/bin/mzscheme")

  ;; Set up er -------------------------------------------------------------
  (setq erlang-root-dir "/opt/local/lib/erlang")
  (setq load-path (cons "/opt/local/lib/erlang/lib/tools-2.5.5/emacs" load-path))
  (setq exec-path (cons "/opt/local/lib/erlang/bin" exec-path))
  (require 'erlang-start)

  ;;Keeps emacs from hanging for 60s when using io commands
  ;;http://bob.pythonmac.org/archives/2007/03/14/erlang-mode-for-emacs/
  (defvar inferior-erlang-prompt-timeout t)

  ;; set up distel
  (require 'distel)
  (distel-setup)

  ;; Some Erlang customizations
  (add-hook 'erlang-mode-hook
	    (lambda ()
	      ;; when starting an Erlang shell in Emacs, default in the node name
	      (setq inferior-erlang-machine-options '("-sname" "emacs"))
	      ;; add Erlang functions to an imenu menu
	      (imenu-add-to-menubar "imenu")))

  ;; Some stuff I might want to enable later
  ;; http://bc.tech.coop/blog/070528.html
  ;; ;; A number of the erlang-extended-mode key bindings are useful in the shell too
  ;; (defconst distel-shell-keys
  ;;   '(("\C-\M-i"   erl-complete)
  ;;     ("\M-?"      erl-complete)	
  ;;     ("\M-."      erl-find-source-under-point)
  ;;     ("\M-,"      erl-find-source-unwind) 
  ;;     ("\M-*"      erl-find-source-unwind) 
  ;;     )
  ;;   "Additional keys to bind when in Erlang shell.")

  ;; (add-hook 'erlang-shell-mode-hook
  ;; 	  (lambda ()
  ;; 	    ;; add some Distel bindings to the Erlang shell
  ;; 	    (dolist (spec distel-shell-keys)
  ;; 	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

  ;; load regex-tool if available
  (safe-load "theirs/regex-tool.el")

  ;; Set up quack --------------------------------------------------------------
  (require 'quack)
  
  ;; Set up ruby-mode ----------------------------------------------------------
  (autoload 'ruby-mode "theirs/ruby-mode/ruby-mode"
    "Mode for editing ruby source files" t)
  (setq auto-mode-alist
	(append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
  (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
			       interpreter-mode-alist))

  (autoload 'run-ruby "theirs/ruby-mode/inf-ruby"
    "Run an inferior Ruby process")
  (autoload 'inf-ruby-keys "theirs/ruby-mode/inf-ruby"
    "Set local key defs for inf-ruby in ruby-mode")
  (add-hook 'ruby-mode-hook
        '(lambda ()
           (inf-ruby-keys)))
  
  ;; make script files executable
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  ;; Put autosave files (ie #foo#) in one place, *not*
  ;; scattered all over the file system!
  ;;   (defvar autosave-dir
  ;;     (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
  
  ;;   (make-directory autosave-dir t)
  
  ;;   (defun auto-save-file-name-p (filename)
  ;;     (string-match "^#.*#$" (file-name-nondirectory filename)))
  
  ;;   (defun make-auto-save-file-name ()
  ;;     (concat autosave-dir
  ;; 	    (if buffer-file-name
  ;; 		(concat "#" (file-name-nondirectory buffer-file-name) "#")
  ;; 	      (expand-file-name
  ;; 	       (concat "#%" (buffer-name) "#")))))

  ;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
  ;; list contains regexp=>directory mappings; filenames matching a regexp are
  ;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
  ;; (defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
  ;; (setq backup-directory-alist (list (cons "." backup-dir)))

  ;; Save all backup file in this directory.
  ;; (setq backup-directory-alist (quote ((".*" . (expand-file-name "~/.emacs_backups/")))))


  ;; CEDET ---------------------------------------------------------------------

  ;; Load CEDET
  (load "theirs/cedet-1.0pre4/common/cedet.elc")

  ;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
  ;; Select one of the following:

  ;; * This enables the database and idle reparse engines
  ;;(semantic-load-enable-minimum-features)

  ;; * This enables some tools useful for coding, such as summary mode
  ;;   imenu support, and the semantic navigator
  ;;   (semantic-load-enable-code-helpers)

  ;; * This enables even more coding tools such as the nascent intellisense mode
  ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
  ;; (semantic-load-enable-guady-code-helpers)

  ;; * This turns on which-func support (Plus all other code helpers)
  (semantic-load-enable-excessive-code-helpers)

  ;; This turns on modes that aid in grammar writing and semantic tool
  ;; development.  It does not enable any other features such as code
  ;; helpers above.
  ;; (semantic-load-enable-semantic-debugging-helpers)

  ;; ---------------------------------------------------------------------------

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )

  ;; ---------------------------------------------------------------------------

  ;; Frame title bar formatting to show full path of file
  (setq-default
   frame-title-format
   (list '((buffer-file-name " %f" (dired-directory 
				    dired-directory
				    (revert-buffer-function " %b"
							    ("%b - Dir:  " default-directory)))))))

  (setq-default
   icon-title-format
   (list '((buffer-file-name " %f" (dired-directory
				    dired-directory
				    (revert-buffer-function " %b"
							    ("%b - Dir:  " default-directory)))))))
  (message "...loaded Mac configuration"))
 
 ((or platform-cygemacs? platform-ntemacs?)
  (load "windows.el")))

;; Load Steve Yegge's javascript mode
;;(autoload 'theirs/js2-mode "js2" nil t)
(load "theirs/js2.elc")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsm$" . js2-mode))

;; Load MozlRepl
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'js2-mode-hook 'java-custom-setup)
(defun java-custom-setup ()
  (moz-minor-mode 1))

    
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
 '(transient-mark-mode (quote identity)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(provide 'mwt-global)