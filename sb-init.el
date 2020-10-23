(defun sb-load-file-when-exists (filename)
  "Load specified file if it exists. Do nothing otherwise."
  (when (file-exists-p filename) (load-file filename)))


(defun sb-insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S")))


(defun sb-init-vanilla-emacs ()
  "Initialize vanilla emacs."
  (setq column-number-mode t
	confirm-nonexistent-file-or-buffer t
	default-directory (concat (getenv "HOME") "/")
	delete-active-region nil
	dnd-open-file-other-window nil
	find-file-run-dired nil
	indent-tabs-mode nil
	indicate-empty-lines nil
	inhibit-startup-screen t
	make-backup-files nil
	ps-paper-type (quote a4)
	safe-local-variable-values (quote ((buffer-auto-save-file-name)
					   (org-confirm-babel-evaluate)))
	show-paren-style (quote expression)
	;; This ensures that Org timestamps allways appear in english
	system-time-locale "C"
	visible-bell t)

  ;; This is required for M-x package-list-packages to work properly
  (prefer-coding-system 'utf-8)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (global-auto-revert-mode)
  (global-hl-line-mode)
  (show-paren-mode)

  (add-to-list 'exec-path invocation-directory))

(sb-init-vanilla-emacs)


(defun sb-init-package ()
  "Initialize package manager and download uninstalled packages."
  (let ((contents-refreshed nil))
    (require 'package)
    (setq package-archives
	  '(("gnu" . "http://elpa.gnu.org/packages/")
            ("melpa" . "http://melpa.org/packages/")
            ("melpa-stable" . "http://stable.melpa.org/packages/")
            ("elpy" . "http://jorgenschaefer.github.io/packages/")
					;("org" . "https://orgmode.org/elpa/")
            ))
    (if (< emacs-major-version 27) (package-initialize))
    (defun sb-package-install-unless-installed (pkg)
      (unless (package-installed-p pkg)
	(unless contents-refreshed
	  (package-refresh-contents)
	  (setq contents-refreshed t))
	(package-install pkg)))
    (sb-package-install-unless-installed 'auctex)
    (sb-package-install-unless-installed 'clang-format)
    (sb-package-install-unless-installed 'cmake-mode)
    (sb-package-install-unless-installed 'counsel)
    (sb-package-install-unless-installed 'elpy)
    (sb-package-install-unless-installed 'htmlize)
    (sb-package-install-unless-installed 'ivy)
    (sb-package-install-unless-installed 'js2-mode)
    (sb-package-install-unless-installed 'js2-refactor)
    (sb-package-install-unless-installed 'lsp-mode)
    (sb-package-install-unless-installed 'lsp-ui)
    (sb-package-install-unless-installed 'magit)
    (sb-package-install-unless-installed 'markdown-mode)
    (sb-package-install-unless-installed 'meson-mode)
    (sb-package-install-unless-installed 'ob-ipython)
    (sb-package-install-unless-installed 'spaceline)
    (sb-package-install-unless-installed 'spacemacs-theme)
    (sb-package-install-unless-installed 'xref-js2)))

(sb-init-package)


(defun sb-init-whitespace ()
  "Initialize whitespace package."
  (require 'whitespace)
  (setq whitespace-line-column 80
	whitespace-style (quote (face lines-tail))))

(sb-init-whitespace)


(defun sb-init-input-method ()
  (setq default-input-method 'rfc1345)
  (mapc (lambda (mode)
	  (add-hook mode (lambda ()
			   (activate-input-method default-input-method))))
	'(org-mode-hook
	  text-mode-hook)))

(sb-init-input-method)


(defun sb-init-epa-file ()
  "Initialize GnuPG interface for Emacs."
  (require 'epa-file)
  (epa-file-enable))

(sb-init-epa-file)


(defun sb-init-key-bindings-and-keymaps ()
  "Initialize a few key bindings and my personal keymap."
  ;; Unlike zap-to-char (bound to M-z), zap-up-to-char kills up to,
  ;; but not including ARGth occurrence of CHAR. It is remapped to
  ;; M-z.
  (autoload 'zap-up-to-char "misc")
  (global-set-key (kbd "M-z") 'zap-up-to-char)

  ;; cycle-spacing is more flexible than just-one-space (initially
  ;; bound to M-SPC).
  (global-set-key (kbd "M-SPC") 'cycle-spacing)

  ;; Under Mac OS X, right-alt must be mapped to Alt Gr.
  (when (string-equal "darwin" (symbol-name system-type))
    (setq mac-option-modifier 'none
	  mac-command-modifier 'meta))

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-unset-key (kbd "<C-next>"))
  (global-unset-key (kbd "<C-prior>"))

  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-x g") 'magit-status)

  ;; My personnal keymap is called `sb-map`, and the prefix key that
  ;; is assigned to this keymap is `C-&`.
  (define-prefix-command 'sb-map)
  (global-set-key (kbd "C-&") 'sb-map)
  (define-key sb-map (kbd "C") 'sb-git-stage-commit-and-push-all)
  (define-key sb-map (kbd "t") 'sb-insert-timestamp))

(sb-init-key-bindings-and-keymaps)


(defun sb-init-my-customization-group ()
  (defgroup sb nil
    "My customization group.

Gathers platform-dependent custom variables (both built-in and
user-defined) that are required by my setup. These custom variables
should be set at each new fresh install (but sensible defaults are
defined)."
    :tag "sb")

  (defcustom sb-path-to-local-documents "~/Documents"
    "Path to the documents directory.

It is in particular assumed that

  - my org notes are located in the notes/ subdirectory,
  - my blog files are located in the blog/ subdirectory.

Under Linux, this variable might be set to

    /home/username/Documents

while under Windows, it might be set to

    C:\\Users\\username\\Documents

In most cases, the default value

    ~/Documents

should work."
    :type 'string :group 'sb :tag "Path to local documents")

  ;; Add some existing variables to my customization group, so as to
  ;; remember that they must be configured.
  (custom-add-to-group 'sb 'ediff-diff-program 'custom-variable)
  (custom-add-to-group 'sb 'ediff-diff3-program 'custom-variable)
  (custom-add-to-group 'sb 'epg-gpg-program 'custom-variable)
  (custom-add-to-group 'sb 'url-proxy-services 'custom-variable))

(sb-init-my-customization-group)


(defun sb-init-appearance ()
  "Initialize appearance of Emacs (fonts, themes, etc.)."
  (setq initial-frame-alist '((top . 0) (left . 0) (fullscreen . maximized))
	spacemacs-theme-org-height nil)
  (load-theme 'spacemacs-light)
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)

  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  ;; Disable window decorations
  (set-frame-parameter nil 'undecorated t)
  (split-window-right)

  ;; Use w32-select-font
  (set-face-font 'default "DejaVu Sans Mono-10")
  )

(sb-init-appearance)


(defun sb-init-ibuffer ()
  (setq ibuffer-default-sorting-mode (quote filename/process)
	ibuffer-show-empty-filter-groups nil
	ibuffer-saved-filter-groups
	(quote
	 (("sb-ibuffer-groups"
	   ("Notes professionnelles" (filename . "notes/professionnelles"))
	   ("Notes personnelles" (filename . "notes/personnelles"))
	   ("Blog" (filename . "/blog"))
	   (".emacs" (filename . ".emacs.d"))))))
  (add-hook 'ibuffer-mode-hook
	    (lambda () (ibuffer-switch-to-saved-filter-groups
			"sb-ibuffer-groups"))))

(sb-init-ibuffer)

(defun sb-init-flymake ()
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(sb-init-flymake)

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


(defun sb-init-ivy ()
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-case-fold-search-default (quote always)))

(sb-init-ivy)

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

  (delete 'Git vc-handled-backends)
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(sb-init-magit)

(defun sb-init-raise-frame ()
  (select-frame-set-input-focus (selected-frame)))

(defun sb-init-auctex ()
  (require 'tex)
  (custom-add-to-group 'sb 'TeX-view-program-list 'custom-variable)
  (custom-add-to-group 'sb 'TeX-view-program-selection 'custom-variable)

  (setq LaTeX-command "latex"
	LaTeX-electric-left-right-brace t
	TeX-auto-save t
	TeX-command "tex"
	TeX-electric-math (quote ("\\(" . "\\)"))
	TeX-parse-self t
	TeX-PDF-mode t
	TeX-raise-frame-function #'sb-init-raise-frame
	Tex-source-correlate-method (quote synctex)
	TeX-source-correlate-mode t
	TeX-source-correlate-start-server t
	;; TODO: is this really necessary?
	font-latex-match-reference-keywords '(("citeauthor" "*{")
					      ("citetext" "{")
					      ("citeyear" "{")
					      ("citeyearpar" "{")
					      ("citep" "*[{")
					      ("citet" "*[{")
					      ("citealt" "*[{")
					      ("citealp" "*[{")))

  (add-to-list 'TeX-view-program-selection '(output-pdf "SumatraPDF"))

  (put 'TeX-view-program-list 'variable-documentation
       (concat (get 'TeX-view-program-list 'variable-documentation)
	       "\n\n------------------------------------------------------------------------\nNote (SB): for SumatraPDF (Windows platforms), set this variable to\n\n    \"C:\\opt\\SumatraPDF-3.0\\SumatraPDF.exe\n    -reuse-instance -forward-search %b %n %o\".\n\nFor Skim (MacOS X platforms), set this variable to\n\n    \"/Applications/Skim.app/Contents/SharedSupport/displayline\n    -r -b %n %o %b\".\n\nUpdate `TeX-view-program-selection' accordingly."))

  (setf TeX-view-program-selection
	(cons '(output-pdf "SumatraPDF")
	      (cl-remove 'output-pdf TeX-view-program-selection
			 :test (lambda (left right) (equal left (car right))))))

  (add-hook 'LaTeX-mode-hook 'whitespace-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (LaTeX-add-environments
					 '("axiom" LaTeX-env-label)
					 '("theorem" LaTeX-env-label)
					 '("remark" LaTeX-env-label)
					 '("definition" LaTeX-env-label)
					 '("example" LaTeX-env-label)
					 '("question" LaTeX-env-label)
					 '("problem" LaTeX-env-label)))))

(sb-init-auctex)


(defun sb-init-reftex ()
  (require 'reftex)

  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  (setq reftex-load-hook (quote (imenu-add-menubar-index))
	reftex-mode-hook (quote (imenu-add-menubar-index))
	reftex-plug-into-AUCTeX t
	reftex-insert-label-flags (quote (nil t))
	reftex-ref-macro-prompt nil
	reftex-label-alist
	'(("axiom"   ?a "ax:"  "~\\ref{%s}" nil ("axiom" "ax.") -1)
	  ("theorem" ?h "thr:" "~\\ref{%s}" nil ("theorem" "th.") -1)
	  ("remark"  ?r "rem:" "~\\ref{%s}" nil ("remark" "rem.") -1)
	  ("definition"  ?d "def:" "~\\ref{%s}" nil ("definition" "def.") -1)
	  ("example" ?x "ex:" "~\\ref{%s}" nil ("example" "ex.") -1)
	  ("question" ?q "q:" "~\\ref{%s}" nil ("question") -1)
	  ("problem" ?p "pb:" "~\\ref{%s}" nil ("problem" "pb." "exercice") -1))))

(sb-init-reftex)


(defun sb-init-bratex ()
  (add-to-list 'load-path "~/.emacs.d/lisp/bratex")
  (require 'bratex)
  (add-hook 'LaTeX-mode-hook #'bratex-config))

(sb-init-bratex)


(defun sb-init-c ()
  (setq-default c-basic-offset 2))

(sb-init-c)


(defun sb-init-astyle ()
  (defcustom astyle-executable "astyle" "Full path to the astyle executable."
    :type 'string :group 'sb :tag "Astyle executable")

  (defun astyle-reformat ()
    "Call astyle on the current buffer.

This function calls the `astyle-executable' with the --project
option. This requires that a project options file be found in the
directory where the current buffer lives, or one of its parents."
    (interactive)
    (when (buffer-modified-p) (error "Buffer has unsaved modifications"))
    (shell-command (concat astyle-executable " --project "
			   (file-name-nondirectory (buffer-file-name)))))

  (define-key sb-map (kbd "f") #'astyle-reformat))

;;(sb-init-astyle)


(defun sb-init-clang-format ()
  (custom-add-to-group 'sb 'clang-format-executable 'custom-variable)
  (define-key sb-map (kbd "f") #'clang-format-buffer))

(sb-init-clang-format)


(defun sb-init-python ()
  (custom-add-to-group 'sb 'python-shell-interpreter-args 'custom-variable)
  (custom-add-to-group 'sb 'python-shell-interpreter-interactive-arg
		       'custom-variable)

  ;; When running =M-x run-python", I get the following error message
  ;;
  ;;     Warning (python): Your ‘python-shell-interpreter’ doesn’t seem
  ;;     to support readline, yet ‘python-shell-completion-native’ was t
  ;;     and "ipython3" is not part of the
  ;;     ‘python-shell-completion-native-disabled-interpreters’
  ;;     list. Native completions have been disabled locally.
  ;;
  ;; A work around (under windows) seems to be
  ;;
  ;;   1. Install pyreadline
  ;;   2. Set `python-shell-completion-native' to t
  ;;   3. Use simple prompt with Jupyter console: set
  ;;      `python-shell-interpreter-args' to
  ;;
  ;;     -i C:\\Users\\brisard\\Miniconda3\\Scripts\\jupyter-script.py console
  ;;     --simple-prompt
  ;;
  ;; See also this https://github.com/jorgenschaefer/elpy/issues/887.
  (setq python-shell-completion-native-enable nil
	python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil)
  (add-hook 'python-mode-hook (lambda() (setq show-trailing-whitespace t)))
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (add-hook 'python-mode-hook 'whitespace-mode)

  (elpy-enable)
  (setq elpy-modules (quote (elpy-module-eldoc
                             elpy-module-flymake
                             elpy-module-sane-defaults)))
  (setq elpy-test-runner (quote elpy-test-test-discover-runner))
  (add-hook 'elpy-mode-hook 'whitespace-mode)

  (setq python-flymake-command (quote ("flake8" "-"))))

(sb-init-python)

(defun sb-init-javascript ()
  (require 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

  (require 'js2-refactor)
  (require 'xref-js2)

  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)

  (add-hook 'js2-mode-hook
	    (lambda () (add-hook 'xref-backend-functions
				 #'xref-js2-xref-backend nil t))))

(sb-init-javascript)

(defun sb-init-lsp ()
  (require 'lsp-mode)
  (require 'lsp-pyls)
  (require 'lsp-ui)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'python-mode-hook #'lsp) ;; pyls must be on the PATH
  (custom-add-to-group 'sb 'lsp-clients-clangd-executable 'custom-variable)
  (add-hook 'c-mode-hook #'lsp)

  (require 'lsp-python-ms)
  (custom-add-to-group 'sb 'lsp-python-ms-dir 'custom-variable)
  (custom-add-to-group 'sb 'lsp-python-ms-executable 'custom-variable))

(sb-init-lsp)

(defun sb-init-maxima ()
  (defun sb--set-maxima-mode-path (symbol value)
    "Setter for the `sb-maxima-mode-path' custom variable."
    (progn (when (boundp symbol) (delete (default-value symbol) load-path))
	   (add-to-list 'load-path value)
	   (set-default symbol value)))

  (defun sb--init-maxima-mode-path (symbol value)
    "Initializer for the `sb-maxima-mode-path' custom variable."
    (progn (add-to-list 'load-path value)
	   (custom-initialize-reset symbol value)))
  (defcustom sb-maxima-mode-path ""
    "Path to the folder hosting elisp files for maxima-mode.

This is the path to the files: maxima.el, maxima-font-lock.el. On
windows platforms, it is something like:

    C:\\maxima-5.40.0\\share\\maxima\\5.40.0\\emacs"
    :type 'string :group 'sb :tag "Path to maxima-mode files"
    :initialize 'sb--init-maxima-mode-path :set 'sb--set-maxima-mode-path)
  (custom-add-to-group 'sb 'maxima-command 'custom-variable)

  (autoload 'maxima-mode "maxima" "Major mode for writing Maxima programs" t)
  (autoload 'maxima "maxima" "Run Maxima interactively" t)
  (setq auto-mode-alist (cons '("\\.ma[cx]" . maxima-mode) auto-mode-alist)))

(sb-init-maxima)


(defun sb-init-ispell-hunspell ()
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
	(list (append (car ispell-tex-skip-alists)
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


(defun sb-init-blog ()
  (sb-load-file-when-exists (expand-file-name "blog/src/sb-blog.el"
                                              sb-path-to-local-documents)))

(sb-init-blog)
