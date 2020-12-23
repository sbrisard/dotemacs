(defun sb-init-ispell-hunspell ()
  "Initialization of ispell, using hunspell"
  (defun sb-update-env-dicpath (value)
    (setenv "DICPATH" (mapconcat #'identity value ";")))

  (setenv "DICTIONARY" "en_US")

  (defcustom sb-dict-path nil
    "The emacs equivalent of the DICPATH environment variable.

This is a list of directories where hunspell can find dictionaries."
    :type '(repeat string)
    :group 'sb
    :tag "Path to Hunspell dictionaries"
    :initialize (lambda (symbol value)
		  (custom-initialize-reset symbol value)
		  (sb-update-env-dicpath value))
    :set (lambda (symbol value)
	   (set-default symbol value)
	   (sb-update-env-dicpath value)))

  (setq ispell-tex-skip-alists
	(list
	 (append (car ispell-tex-skip-alists)
		 '(("\\\\cite"            ispell-tex-arg-end)
		   ("\\\\nocite"          ispell-tex-arg-end)
		   ("\\\\includegraphics" ispell-tex-arg-end)
		   ("\\\\author"          ispell-tex-arg-end)
		   ("\\\\ref"             ispell-tex-arg-end)
		   ("\\\\eqref"           ispell-tex-arg-end)
		   ("\\\\label"           ispell-tex-arg-end)
		   ("\\\\cite[tp]"        ispell-tex-arg-end)))
	 (cadr ispell-tex-skip-alists))))

(sb-init-ispell-hunspell)

(defun sb-init--find-org-agenda-directories ()
  "Return the list of all subdirectories of my notes directory.

The notes directory is located in the `notes` subdirectory of
``sb-path-to-local-documents''.  Some subdirectoriesgit (like
./.git) are excluded."
  (let ((root (expand-file-name "notes" sb-path-to-local-documents)))
    (cons root (cl-remove-if (lambda (name) (or (not (file-directory-p name))
						(string-suffix-p ".git" name)))
			     (directory-files root t directory-files-no-dot-files-regexp)))))

(defun sb-git-stage-commit-and-push-all ()
  "Stage, commit and push all changes in current git repository.

This function runs the following commands

    git commit -a -m msg
    git push

The default commit message is \"DD/MM/YYYY HH:MM\". The gt
push.default variable must be set.

This function uses magit only to display the current status."
  (interactive)
  (shell-command (concat "git commit -a -m \""
			       (format-time-string "%d/%m/%Y %H:%M")
			       "\""))
  (shell-command "git push")
  (magit-status))


(defun sb-init-magit ()
  (require 'magit)
  (custom-add-to-group 'sb 'magit-git-executable 'custom-variable)
  (custom-add-to-group 'sb 'magit-repository-directories 'custom-variable)

  (setq magit-process-ensure-unix-line-ending t)
  (global-set-key (kbd "C-x g") 'magit-status)

  (delete 'Git vc-handled-backends)
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(sb-init-magit)
