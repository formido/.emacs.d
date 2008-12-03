;; start emacs server so I can hack os x to start files from the Finder
;; probobably this will come in handy for other stuff in the future too
(server-start)

;; function to load files with error checking
(autoload 'safe-load "safe-load")

;; Who needs a toolbar?
(tool-bar-mode 0)

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

;; Load GitHub's semi-official Emacs-pastie integration mode
(load "lisp/gist.el")

;; set up visual theme
(add-to-list 'load-path "~/.emacs.d/lisp/twilight")
(require 'color-theme)
(color-theme-initialize)
;;(color-theme-snow)
(color-theme-twilight)

;; So we can test for which platform we're on
(setq platform-mac? (string-match "powerpc" system-configuration))
;;TODO: Update this line for home
(setq platform-homepc? (string-match "XXXXX" system-configuration))
(setq platform-workpc? (string-match "pc-cygwin" system-configuration))

;; Platform specific configurations
(cond
 (platform-mac?

  (autoload 'markdown-mode "lisp/markdown-mode.el"
   "Major mode for editing Markdown files" t)
  (setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

  ;; Set up scheme -------------------------------------------------------------
  (setq scheme-program-name "~/bin/plt/bin/mzscheme")

  ;; Set up erlang -------------------------------------------------------------
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
  (safe-load "lisp/regex-tool.el")

  ;; Set up quack --------------------------------------------------------------
  (require 'quack)
  
  ;; Set up ruby-mode ----------------------------------------------------------
  (autoload 'ruby-mode "lisp/ruby-mode/ruby-mode"
    "Mode for editing ruby source files" t)
  (setq auto-mode-alist
	(append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
  (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
			       interpreter-mode-alist))

  (autoload 'run-ruby "lisp/ruby-mode/inf-ruby"
    "Run an inferior Ruby process")
  (autoload 'inf-ruby-keys "lisp/ruby-mode/inf-ruby"
    "Set local key defs for inf-ruby in ruby-mode")
  (add-hook 'ruby-mode-hook
        '(lambda ()
           (inf-ruby-keys)))
  
  ;; make script files executable
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  ;; Put autosave files (ie #foo#) in one place, *not*
  ;; scattered all over the file system!
  (defvar autosave-dir
    (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

  (make-directory autosave-dir t)

  (defun auto-save-file-name-p (filename)
    (string-match "^#.*#$" (file-name-nondirectory filename)))

  (defun make-auto-save-file-name ()
    (concat autosave-dir
	    (if buffer-file-name
		(concat "#" (file-name-nondirectory buffer-file-name) "#")
	      (expand-file-name
	       (concat "#%" (buffer-name) "#")))))

  ;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
  ;; list contains regexp=>directory mappings; filenames matching a regexp are
  ;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
  (defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
  (setq backup-directory-alist (list (cons "." backup-dir)))

  ;; CEDET ---------------------------------------------------------------------

  ;; Load CEDET
  (load "lisp/cedet-1.0pre4/common/cedet.elc")

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

  ;; NXHTML-MODE and NXML-MODE -------------------------------------------------
  (load "lisp/nxml/autostart.el")

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
 
 (platform-workpc?
  ;; TODO: Decide whether to remove this. Don't seem to need it.
  ;; TODO: Emacswiki has a cygwin setup script. Should I use? But it uses cygwin-mount.el
  ;; which my NTEmacs doesn't seem to need!
  ;(require 'cygwin-mount)
  ;(cygwin-mount-activate)

  ;; use bash as the default shell
  (setq shell-file-name "bash")
  (setenv "SHELL" shell-file-name)
  (setenv "PATH" (concat (getenv "PATH") ":/bin" ":/usr/local/bin"))
  (setq explicit-shell-file-name shell-file-name)
  (setq explicit-shell-args '("--login" "-i"))
  (setq shell-command-switch "-ic")
  (setq w32-quote-process-args t)
  ;; Not sure I need these
  ;(defun bash ()
    ;(interactive)
    ;(let ((binary-process-input t)
	  ;(binary-process-output nil))
      ;(shell)))

  (message "...loaded Work PC configuration"))

 (platform-homepc?
  (require 'cygwin-mount)
  (cygwin-mount-activate)

  ;; use bash as the default shell
  (setq exec-path (cons "C:\cygwin\bin" exec-path))
  (setq shell-file-name "bash")
  (setenv "SHELL" shell-file-name)
  (setenv "PATH" (concat (getenv "PATH") ";C:\\bin" ";C:\\usr\\local\\bin"))
  (setq explicit-shell-file-name shell-file-name)
  (setq explicit-shell-args '("--login" "-i"))
  (setq shell-command-switch "-ic")
  (setq w32-quote-process-args t)
  (defun bash ()
    (interactive)
    (let ((binary-process-input t)
	  (binary-process-output nil))
      (shell)))

  (message "...loaded Home PC configuration")))
    
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