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

  ;; TODO: Make this load conditionally
  ;; Set up erlang-mode
  (setq load-path (cons  "C:/Program Files/erl5.7/lib/tools-2.6.3/emacs"
			 load-path))
  (setq erlang-root-dir "C:/Program Files/erl5.7")
  (setq exec-path (cons "C:/Program Files/erl5.7/bin" exec-path))
  (require 'erlang-start)

  ;; TODO: Make this load conditionally
  ;; Set up distel for erlang
  (add-to-list 'load-path "c:/Documents and Settings/Michael/My Documents/.emacs.d/theirs/distel/elisp")
  (require 'distel)
  (distel-setup)
  
  ;; ------------------------------------------------------
  ;; TODO: Make these load conditionally
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
		(define-key erlang-shell-mode-map (car spec) (cadr spec)))))
  ;; ------------------------------------------------------

  (message "...loaded NTEmacs configuration")))
