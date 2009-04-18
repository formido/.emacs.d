(cond
 (platform-cygemacs?
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

  (message "...loaded Cygwin configuration"))
 (platform-ntemacs?
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

  ;; Set up erlang-mode
  (setq erlang-start "C:/ProgramFiles/erl5.7/lib/tools-2.6.3/emacs/erlang-start.el")
  (if (file-exists-p erlang-start)
      (progn
	(setq load-path (cons  "C:/ProgramFiles/erl5.7/lib/tools-2.6.3/emacs"
			       load-path))
	(setq erlang-root-dir "C:/ProgramFiles/erl5.7")
	(setq exec-path (cons "C:/ProgramFiles/erl5.7/bin" exec-path))
	(require 'erlang-start))
    (message (concat "Warning: Couldn't find: " erlang-start)))

  ;; Set up distel for erlang
  (setq distel "c:/Documents and Settings/Michael/My Documents/.emacs.d/theirs/distel/elisp")
  (if (file-exists-p distel)
      (progn
	(add-to-list 'load-path distel)
	(require 'distel)
	(distel-setup)

	;; Customizations from Bill Clementson: http://bc.tech.coop/blog/070528.html
	;; Some Erlang customizations
	(add-hook 'erlang-mode-hook
		  (lambda ()
		    ;; when starting an Erlang shell in Emacs, default in the node name
		    (setq inferior-erlang-machine-options '("-sname" "emacs"))
		    ;; add Erlang functions to an imenu menu
		    (imenu-add-to-menubar "imenu")))

	;; A number of the erlang-extended-mode key bindings are useful in the shell too
	(defconst distel-shell-keys
	  '(("\C-\M-i"   erl-complete)
	    ("\M-?"      erl-complete)	
	    ("\M-."      erl-find-source-under-point)
	    ("\M-,"      erl-find-source-unwind) 
	    ("\M-*"      erl-find-source-unwind) 
	    )
	  "Additional keys to bind when in Erlang shell.")

	(add-hook 'erlang-shell-mode-hook
		  (lambda ()
		    ;; add some Distel bindings to the Erlang shell
		    (dolist (spec distel-shell-keys)
		      (define-key erlang-shell-mode-map (car spec) (cadr spec))))))
	
	(message (concat "Warning: Couldn't find: " distel)))

  (message "...loaded NTEmacs configuration")))
