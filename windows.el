(cond
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