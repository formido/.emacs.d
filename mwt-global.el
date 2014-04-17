;; If we're in a debug instance of Emacs, load default color-theme
;; to differentiate it
(if (not debug-on-error) 
    (progn 
      (require 'color-theme)
      (color-theme-initialize)
      (load "theirs/twilight-emacs/color-theme-twilight.el")
      ;;(color-theme-twilight)
      ))

;; set up magit
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/theirs/magit"))
;; (require 'magit)

;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; make it easier to switch between windows
(global-set-key (kbd "ESC <left>") 'windmove-left)
(global-set-key (kbd "ESC <up>") 'windmove-up)
(global-set-key (kbd "ESC <right>") 'windmove-right)
(global-set-key (kbd "ESC <down>") 'windmove-down)

;; Set up ocaml
(add-to-list 'load-path (expand-file-name "~/.emacs.d/theirs/ocaml-mode-3.05"))
(setq auto-mode-alist
      (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(require 'caml)
(require 'inf-caml)

;; set up modes for cappuccino/objective-j development
(add-to-list 'load-path (expand-file-name "~/.emacs.d/theirs/cappuccino"))
(require 'objj-mode)

;; Set up Haskell
(load "~/.emacs.d/theirs/haskell-mode-2.4/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;; Set up Anything
;;(setq anything (expand-file-name "~/.emacs.d/theirs/anything"))
;;(add-to-list 'load-path anything)
;;(require 'anything-config)   
;;(global-set-key (kbd "C-2") 'anything)  
;;(global-set-key (kbd "M-2") 'universal-argument)
;;(setq anything-sources '(anything-c-source-buffers+   
;;			 anything-c-source-locate   
;;			 anything-c-source-recentf   
;;			 anything-c-source-org-headline   
;;			 anything-c-source-buffer-not-found))

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
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/theirs/emacs-jabber"))
;;(require 'jabber)

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
;; (load "theirs/nxml/autostart.el")

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
  
  ;; Set up erlang -------------------------------------------------------------
  (load-file "~/.emacs.d/mine/erlang.macs")

  (setq erlang-root-dir "/usr/local/lib/erlang")
  (setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.6.4" load-path))
  (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
  (require 'erlang-start)

  ;;Keeps emacs from hanging for 60s when using io commands
  ;;http://bob.pythonmac.org/archives/2007/03/14/erlang-mode-for-emacs/
  (defvar inferior-erlang-prompt-timeout t)

  ;; set up distel
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/theirs/distel/elisp"))
  (require 'distel)
  (distel-setup)

  ;; Some Erlang customizations
  ;; TODO: this project configuration stuff should go elsewhere
  (add-hook 'erlang-mode-hook
	    (lambda ()
	      (setq inferior-erlang-machine-options '("-sname" "emacs"
						      "-setcookie" "magiccookie"
						      "-pa" "/var/www/html/flashcard2/ebin"
						      "-pa" "/var/www/html/flashcard2/deps/erlmongo"
						      "-pa" "/var/www/html/flashcard2/deps/*/ebin"
						      "-pa" "/var/www/html/flashcard2/deps/*/deps/*/ebin"
						      "-rest_api" "db_name" "'<<\"tests\">>'" 
						      "-boot" "start_sasl"
						      "-s" "rest_api"))
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
(autoload 'theirs/js2-mode "js2" nil t)
(load "theirs/js2.elc")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsm$" . js2-mode))

;; Load MozlRepl
;;(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;;(add-hook 'js2-mode-hook 'java-custom-setup)
;;(defun java-custom-setup ()
  ;;(moz-minor-mode 1))

    
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

(defmacro tty-color-define-1 (n c r g b)
  `(tty-color-define ,n ,c (list (* 257 ,r) (* 257 ,g) (* 257 ,b))))
 (defun tty-color-closest-to-rgb-txt ()
  "define the colors"
  (tty-color-define-1 "grey22" 16 55 55 55)
  (tty-color-define-1 "DarkSlateGrey" 17 55 55 95)
  (tty-color-define-1 "SlateBlue4" 18 55 55 135)
  (tty-color-define-1 "SlateBlue4" 19 55 55 175)
  (tty-color-define-1 "RoyalBlue3" 20 55 55 215)
  (tty-color-define-1 "RoyalBlue2" 21 55 55 255)
  (tty-color-define-1 "DarkSlateGrey" 22 55 95 55)
  (tty-color-define-1 "DarkSlateGrey" 23 55 95 95)
  (tty-color-define-1 "SteelBlue4" 24 55 95 135)
  (tty-color-define-1 "RoyalBlue3" 25 55 95 175)
  (tty-color-define-1 "RoyalBlue3" 26 55 95 215)
  (tty-color-define-1 "RoyalBlue2" 27 55 95 255)
  (tty-color-define-1 "ForestGreen" 28 55 135 55)
  (tty-color-define-1 "SeaGreen4" 29 55 135 95)
  (tty-color-define-1 "aquamarine4" 30 55 135 135)
  (tty-color-define-1 "SteelBlue" 31 55 135 175)
  (tty-color-define-1 "SteelBlue3" 32 55 135 215)
  (tty-color-define-1 "RoyalBlue1" 33 55 135 255)
  (tty-color-define-1 "LimeGreen" 34 55 175 55)
  (tty-color-define-1 "MediumSeaGreen" 35 55 175 95)
  (tty-color-define-1 "MediumSeaGreen" 36 55 175 135)
  (tty-color-define-1 "LightSeaGreen" 37 55 175 175)
  (tty-color-define-1 "SteelBlue3" 38 55 175 215)
  (tty-color-define-1 "DodgerBlue1" 39 55 175 255)
  (tty-color-define-1 "LimeGreen" 40 55 215 55)
  (tty-color-define-1 "SeaGreen3" 41 55 215 95)
  (tty-color-define-1 "SeaGreen3" 42 55 215 135)
  (tty-color-define-1 "MediumTurquoise" 43 55 215 175)
  (tty-color-define-1 "turquoise" 44 55 215 215)
  (tty-color-define-1 "turquoise" 45 55 215 255)
  (tty-color-define-1 "LimeGreen" 46 55 255 55)
  (tty-color-define-1 "SeaGreen2" 47 55 255 95)
  (tty-color-define-1 "SeaGreen2" 48 55 255 135)
  (tty-color-define-1 "SeaGreen1" 49 55 255 175)
  (tty-color-define-1 "turquoise" 50 55 255 215)
  (tty-color-define-1 "cyan1" 51 55 255 255)
  (tty-color-define-1 "grey27" 52 95 55 55)
  (tty-color-define-1 "grey32" 53 95 55 95)
  (tty-color-define-1 "MediumPurple4" 54 95 55 135)
  (tty-color-define-1 "MediumPurple4" 55 95 55 175)
  (tty-color-define-1 "purple3" 56 95 55 215)
  (tty-color-define-1 "BlueViolet" 57 95 55 255)
  (tty-color-define-1 "DarkOliveGreen" 58 95 95 55)
  (tty-color-define-1 "grey37" 59 95 95 95)
  (tty-color-define-1 "MediumPurple4" 60 95 95 135)
  (tty-color-define-1 "SlateBlue3" 61 95 95 175)
  (tty-color-define-1 "SlateBlue3" 62 95 95 215)
  (tty-color-define-1 "RoyalBlue1" 63 95 95 255)
  (tty-color-define-1 "DarkOliveGreen4" 64 95 135 55)
  (tty-color-define-1 "DarkSeaGreen4" 65 95 135 95)
  (tty-color-define-1 "PaleTurquoise4" 66 95 135 135)
  (tty-color-define-1 "SteelBlue" 67 95 135 175)
  (tty-color-define-1 "SteelBlue3" 68 95 135 215)
  (tty-color-define-1 "CornflowerBlue" 69 95 135 255)
  (tty-color-define-1 "DarkOliveGreen4" 70 95 175 55)
  (tty-color-define-1 "DarkSeaGreen4" 71 95 175 95)
  (tty-color-define-1 "CadetBlue" 72 95 175 135)
  (tty-color-define-1 "CadetBlue" 73 95 175 175)
  (tty-color-define-1 "SkyBlue3" 74 95 175 215)
  (tty-color-define-1 "SteelBlue1" 75 95 175 255)
  (tty-color-define-1 "LimeGreen" 76 95 215 55)
  (tty-color-define-1 "PaleGreen3" 77 95 215 95)
  (tty-color-define-1 "SeaGreen3" 78 95 215 135)
  (tty-color-define-1 "aquamarine3" 79 95 215 175)
  (tty-color-define-1 "MediumTurquoise" 80 95 215 215)
  (tty-color-define-1 "SteelBlue1" 81 95 215 255)
  (tty-color-define-1 "chartreuse2" 82 95 255 55)
  (tty-color-define-1 "SeaGreen2" 83 95 255 95)
  (tty-color-define-1 "SeaGreen1" 84 95 255 135)
  (tty-color-define-1 "SeaGreen1" 85 95 255 175)
  (tty-color-define-1 "aquamarine1" 86 95 255 215)
  (tty-color-define-1 "DarkSlateGray2" 87 95 255 255)
  (tty-color-define-1 "IndianRed4" 88 135 55 55)
  (tty-color-define-1 "HotPink4" 89 135 55 95)
  (tty-color-define-1 "MediumOrchid4" 90 135 55 135)
  (tty-color-define-1 "DarkOrchid" 91 135 55 175)
  (tty-color-define-1 "BlueViolet" 92 135 55 215)
  (tty-color-define-1 "purple1" 93 135 55 255)
  (tty-color-define-1 "tan4" 94 135 95 55)
  (tty-color-define-1 "LightPink4" 95 135 95 95)
  (tty-color-define-1 "plum4" 96 135 95 135)
  (tty-color-define-1 "MediumPurple3" 97 135 95 175)
  (tty-color-define-1 "MediumPurple3" 98 135 95 215)
  (tty-color-define-1 "SlateBlue1" 99 135 95 255)
  (tty-color-define-1 "LightGoldenrod4" 100 135 135 55)
  (tty-color-define-1 "wheat4" 101 135 135 95)
  (tty-color-define-1 "grey53" 102 135 135 135)
  (tty-color-define-1 "LightSlateGrey" 103 135 135 175)
  (tty-color-define-1 "MediumPurple" 104 135 135 215)
  (tty-color-define-1 "LightSlateBlue" 105 135 135 255)
  (tty-color-define-1 "OliveDrab3" 106 135 175 55)
  (tty-color-define-1 "DarkOliveGreen3" 107 135 175 95)
  (tty-color-define-1 "DarkSeaGreen" 108 135 175 135)
  (tty-color-define-1 "LightSkyBlue3" 109 135 175 175)
  (tty-color-define-1 "LightSkyBlue3" 110 135 175 215)
  (tty-color-define-1 "SkyBlue2" 111 135 175 255)
  (tty-color-define-1 "OliveDrab3" 112 135 215 55)
  (tty-color-define-1 "DarkOliveGreen3" 113 135 215 95)
  (tty-color-define-1 "PaleGreen3" 114 135 215 135)
  (tty-color-define-1 "DarkSeaGreen3" 115 135 215 175)
  (tty-color-define-1 "DarkSlateGray3" 116 135 215 215)
  (tty-color-define-1 "SkyBlue1" 117 135 215 255)
  (tty-color-define-1 "GreenYellow" 118 135 255 55)
  (tty-color-define-1 "LightGreen" 119 135 255 95)
  (tty-color-define-1 "LightGreen" 120 135 255 135)
  (tty-color-define-1 "PaleGreen1" 121 135 255 175)
  (tty-color-define-1 "aquamarine1" 122 135 255 215)
  (tty-color-define-1 "DarkSlateGray1" 123 135 255 255)
  (tty-color-define-1 "brown" 124 175 55 55)
  (tty-color-define-1 "maroon" 125 175 55 95)
  (tty-color-define-1 "VioletRed3" 126 175 55 135)
  (tty-color-define-1 "DarkOrchid" 127 175 55 175)
  (tty-color-define-1 "DarkOrchid2" 128 175 55 215)
  (tty-color-define-1 "DarkOrchid1" 129 175 55 255)
  (tty-color-define-1 "sienna" 130 175 95 55)
  (tty-color-define-1 "IndianRed" 131 175 95 95)
  (tty-color-define-1 "HotPink3" 132 175 95 135)
  (tty-color-define-1 "MediumOrchid3" 133 175 95 175)
  (tty-color-define-1 "MediumOrchid" 134 175 95 215)
  (tty-color-define-1 "MediumPurple2" 135 175 95 255)
  (tty-color-define-1 "tan3" 136 175 135 55)
  (tty-color-define-1 "LightSalmon3" 137 175 135 95)
  (tty-color-define-1 "RosyBrown" 138 175 135 135)
  (tty-color-define-1 "grey63" 139 175 135 175)
  (tty-color-define-1 "MediumPurple2" 140 175 135 215)
  (tty-color-define-1 "MediumPurple1" 141 175 135 255)
  (tty-color-define-1 "OliveDrab3" 142 175 175 55)
  (tty-color-define-1 "DarkKhaki" 143 175 175 95)
  (tty-color-define-1 "NavajoWhite3" 144 175 175 135)
  (tty-color-define-1 "grey69" 145 175 175 175)
  (tty-color-define-1 "LightSteelBlue3" 146 175 175 215)
  (tty-color-define-1 "LightSteelBlue" 147 175 175 255)
  (tty-color-define-1 "OliveDrab2" 148 175 215 55)
  (tty-color-define-1 "DarkOliveGreen3" 149 175 215 95)
  (tty-color-define-1 "DarkSeaGreen3" 150 175 215 135)
  (tty-color-define-1 "DarkSeaGreen2" 151 175 215 175)
  (tty-color-define-1 "LightCyan3" 152 175 215 215)
  (tty-color-define-1 "LightSkyBlue1" 153 175 215 255)
  (tty-color-define-1 "GreenYellow" 154 175 255 55)
  (tty-color-define-1 "DarkOliveGreen2" 155 175 255 95)
  (tty-color-define-1 "PaleGreen1" 156 175 255 135)
  (tty-color-define-1 "DarkSeaGreen2" 157 175 255 175)
  (tty-color-define-1 "DarkSeaGreen1" 158 175 255 215)
  (tty-color-define-1 "PaleTurquoise1" 159 175 255 255)
  (tty-color-define-1 "brown3" 160 215 55 55)
  (tty-color-define-1 "VioletRed3" 161 215 55 95)
  (tty-color-define-1 "VioletRed3" 162 215 55 135)
  (tty-color-define-1 "maroon2" 163 215 55 175)
  (tty-color-define-1 "MediumOrchid" 164 215 55 215)
  (tty-color-define-1 "DarkOrchid1" 165 215 55 255)
  (tty-color-define-1 "sienna3" 166 215 95 55)
  (tty-color-define-1 "IndianRed" 167 215 95 95)
  (tty-color-define-1 "HotPink3" 168 215 95 135)
  (tty-color-define-1 "HotPink2" 169 215 95 175)
  (tty-color-define-1 "orchid" 170 215 95 215)
  (tty-color-define-1 "MediumOrchid1" 171 215 95 255)
  (tty-color-define-1 "tan3" 172 215 135 55)
  (tty-color-define-1 "LightSalmon3" 173 215 135 95)
  (tty-color-define-1 "LightPink3" 174 215 135 135)
  (tty-color-define-1 "pink3" 175 215 135 175)
  (tty-color-define-1 "plum3" 176 215 135 215)
  (tty-color-define-1 "violet" 177 215 135 255)
  (tty-color-define-1 "goldenrod" 178 215 175 55)
  (tty-color-define-1 "LightGoldenrod3" 179 215 175 95)
  (tty-color-define-1 "tan" 180 215 175 135)
  (tty-color-define-1 "MistyRose3" 181 215 175 175)
  (tty-color-define-1 "thistle3" 182 215 175 215)
  (tty-color-define-1 "plum2" 183 215 175 255)
  (tty-color-define-1 "OliveDrab2" 184 215 215 55)
  (tty-color-define-1 "khaki3" 185 215 215 95)
  (tty-color-define-1 "LightGoldenrod2" 186 215 215 135)
  (tty-color-define-1 "LightYellow3" 187 215 215 175)
  (tty-color-define-1 "grey84" 188 215 215 215)
  (tty-color-define-1 "LightSteelBlue1" 189 215 215 255)
  (tty-color-define-1 "OliveDrab1" 190 215 255 55)
  (tty-color-define-1 "DarkOliveGreen1" 191 215 255 95)
  (tty-color-define-1 "DarkOliveGreen1" 192 215 255 135)
  (tty-color-define-1 "DarkSeaGreen1" 193 215 255 175)
  (tty-color-define-1 "honeydew2" 194 215 255 215)
  (tty-color-define-1 "LightCyan1" 195 215 255 255)
  (tty-color-define-1 "firebrick1" 196 255 55 55)
  (tty-color-define-1 "brown1" 197 255 55 95)
  (tty-color-define-1 "VioletRed1" 198 255 55 135)
  (tty-color-define-1 "maroon1" 199 255 55 175)
  (tty-color-define-1 "maroon1" 200 255 55 215)
  (tty-color-define-1 "magenta1" 201 255 55 255)
  (tty-color-define-1 "tomato1" 202 255 95 55)
  (tty-color-define-1 "IndianRed1" 203 255 95 95)
  (tty-color-define-1 "IndianRed1" 204 255 95 135)
  (tty-color-define-1 "HotPink" 205 255 95 175)
  (tty-color-define-1 "HotPink" 206 255 95 215)
  (tty-color-define-1 "MediumOrchid1" 207 255 95 255)
  (tty-color-define-1 "sienna1" 208 255 135 55)
  (tty-color-define-1 "salmon1" 209 255 135 95)
  (tty-color-define-1 "LightCoral" 210 255 135 135)
  (tty-color-define-1 "PaleVioletRed1" 211 255 135 175)
  (tty-color-define-1 "orchid2" 212 255 135 215)
  (tty-color-define-1 "orchid1" 213 255 135 255)
  (tty-color-define-1 "goldenrod1" 214 255 175 55)
  (tty-color-define-1 "SandyBrown" 215 255 175 95)
  (tty-color-define-1 "LightSalmon1" 216 255 175 135)
  (tty-color-define-1 "LightPink1" 217 255 175 175)
  (tty-color-define-1 "pink1" 218 255 175 215)
  (tty-color-define-1 "plum1" 219 255 175 255)
  (tty-color-define-1 "goldenrod1" 220 255 215 55)
  (tty-color-define-1 "LightGoldenrod2" 221 255 215 95)
  (tty-color-define-1 "LightGoldenrod2" 222 255 215 135)
  (tty-color-define-1 "NavajoWhite1" 223 255 215 175)
  (tty-color-define-1 "MistyRose1" 224 255 215 215)
  (tty-color-define-1 "thistle1" 225 255 215 255)
  (tty-color-define-1 "yellow1" 226 255 255 55)
  (tty-color-define-1 "LightGoldenrod1" 227 255 255 95)
  (tty-color-define-1 "khaki1" 228 255 255 135)
  (tty-color-define-1 "wheat1" 229 255 255 175)
  (tty-color-define-1 "cornsilk1" 230 255 255 215)
  (tty-color-define-1 "grey100" 231 255 255 255)
  (tty-color-define-1 "grey3" 232 8 8 8)
  (tty-color-define-1 "grey7" 233 18 18 18)
  (tty-color-define-1 "grey11" 234 28 28 28)
  (tty-color-define-1 "grey15" 235 38 38 38)
  (tty-color-define-1 "grey19" 236 48 48 48)
  (tty-color-define-1 "grey23" 237 58 58 58)
  (tty-color-define-1 "grey27" 238 68 68 68)
  (tty-color-define-1 "grey31" 239 78 78 78)
  (tty-color-define-1 "grey35" 240 88 88 88)
  (tty-color-define-1 "grey39" 241 98 98 98)
  (tty-color-define-1 "grey42" 242 108 108 108)
  (tty-color-define-1 "grey46" 243 118 118 118)
  (tty-color-define-1 "grey50" 244 128 128 128)
  (tty-color-define-1 "grey54" 245 138 138 138)
  (tty-color-define-1 "grey58" 246 148 148 148)
  (tty-color-define-1 "grey62" 247 158 158 158)
  (tty-color-define-1 "grey66" 248 168 168 168)
  (tty-color-define-1 "grey70" 249 178 178 178)
  (tty-color-define-1 "grey74" 250 188 188 188)
  (tty-color-define-1 "grey78" 251 198 198 198)
  (tty-color-define-1 "grey82" 252 208 208 208)
  (tty-color-define-1 "grey86" 253 218 218 218)
  (tty-color-define-1 "grey90" 254 228 228 228)
  (tty-color-define-1 "grey93" 255 238 238 238))
 (tty-color-closest-to-rgb-txt) ; finally activate it.

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)        ; GNU Emacs
  (setq font-lock-auto-fontify t))   ; XEmacs

(provide 'mwt-global)