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
    (package-initialize)
    (defun sb-package-install-unless-installed (pkg)
      (unless (package-installed-p pkg)
	(unless contents-refreshed
	  (package-refresh-contents)
	  (setq contents-refreshed t))
	(package-install pkg)))
    (sb-package-install-unless-installed 'auctex)
    (sb-package-install-unless-installed 'counsel)
    (sb-package-install-unless-installed 'elpy)
    (sb-package-install-unless-installed 'htmlize)
    (sb-package-install-unless-installed 'ivy)
    (sb-package-install-unless-installed 'magit)
    (sb-package-install-unless-installed 'ob-ipython)
    (sb-package-install-unless-installed 'spacemacs-theme)))

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
  (define-key sb-map (kbd "C") 'sb-git-stage-commit-and-push-all))

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
  (custom-add-to-group 'sb 'url-proxy-services 'custom-variable))

(sb-init-my-customization-group)


(defun sb-init-appearance ()
  "Initialize appearance of Emacs (fonts, themes, etc.)."
  (setq initial-frame-alist '((top . 0) (left . 0))
	spacemacs-theme-org-height nil)
  (load-theme 'spacemacs-dark)

  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  (set-face-font 'default "DejaVu Sans Mono"))

(sb-init-appearance)


(defun sb-init-ibuffer ()
  (setq ibuffer-default-sorting-mode (quote filename/process)
	ibuffer-show-empty-filter-groups nil
	ibuffer-saved-filter-groups
	(quote
	 (("sb-ibuffer-groups"
	   ("Notes professionnelles" (filename . "notes/professionnelles"))
	   ("Notes personnelles" (filename . "notes/personnelles"))
	   (".emacs" (filename . ".emacs.d"))))))
  (add-hook 'ibuffer-mode-hook
	    (lambda () (ibuffer-switch-to-saved-filter-groups
			"sb-ibuffer-groups"))))

(sb-init-ibuffer)


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

(defun sb-init-org-mode ()
  (setq org-agenda-restore-windows-after-quit t
	org-agenda-window-setup 'current-window
	org-adapt-indentation nil
	org-confirm-babel-evaluate nil ; Potentially risky!
	org-duration-format (quote ((special . h:mm))) ; Don't display
					; long durations
					; in days.
	org-export-preserve-breaks nil
	org-export-time-stamp-file t
	org-export-with-archived-trees 'headline
	org-export-with-author t
	org-export-with-clocks nil
	org-export-with-creator 'comment
	org-export-with-date t
	org-export-with-drawers '(not "LOGBOOK")
	org-export-with-email nil
	org-export-with-emphasize t
	org-export-with-entities t
	org-export-with-fixed-width t
	org-export-with-footnotes t
	org-export-with-inlinetasks t
	org-export-with-planning nil
	org-export-with-priority nil
	org-export-with-section-numbers nil
	org-export-with-smart-quotes nil
	org-export-with-special-strings t
	org-export-with-statistics-cookies t
	org-export-with-sub-superscripts t
	org-export-with-tables t
	org-export-with-tags t
	org-export-with-tasks t
	org-export-with-timestamps t
	org-export-with-toc nil
	org-export-with-todo-keywords t
	org-html-htmlize-output-type 'css
	org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
				     (vm-imap . vm-visit-imap-folder-other-frame)
				     (gnus . org-gnus-no-new-news)
				     (file . find-file-other-window)
				     (wl . wl-other-frame)))
	org-log-into-drawer t
	org-src-fontify-natively t
	org-src-window-setup 'other-window
	org-startup-folded t
	org-startup-truncated nil
	org-table-copy-increment nil
	org-time-clocksum-format "%02d:%02d"
	org-todo-keywords '((sequence "A_FAIRE(a)" "EN_ATTENTE(e)" "UN_JOUR(u)"
				      "|" "FAIT(f)"))
	org-agenda-files (sb-init--find-org-agenda-directories))

  (eval-after-load "org-clock"
    '(defun org-clocktable-indent-string (level)
       "Return indentation string according to LEVEL.
LEVEL is an integer.  Indent by two spaces per level above 1."
       (if (= level 1) ""
	 (concat "→" (make-string (* 2 (- level 1)) 32))))))

(sb-init-org-mode)

(defun sb-init-org-babel ()
  ;; Displayed inlined images are automatically updated after
  ;; evaluating source blocks. Suggestion found on the ob-ipython
  ;; website:
  ;;
  ;;     https://github.com/gregsexton/ob-ipython
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (org-babel-do-load-languages 'org-babel-load-languages '((C . t)
							   (python . t)
							   (maxima . t)
							   (ipython . t))))

(sb-init-org-babel)


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

(defun sb-init-auctex ()
  (require 'tex)
  (custom-add-to-group 'sb 'TeX-view-program-list 'custom-variable)
  (custom-add-to-group 'sb 'TeX-view-program-selection 'custom-variable)

  (setq LaTeX-command "latex"
	LaTeX-electric-left-right-brace t
	TeX-auto-save nil
	TeX-command "tex"
	TeX-electric-math (quote ("\\(" . "\\)"))
	TeX-master t
	TeX-parse-self t
	TeX-PDF-mode t
	TeX-source-correlate-method (quote synctex)
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

  (add-hook 'LaTeX-mode-hook (lambda () (LaTeX-add-environments
					 '("axiom" LaTeX-env-label)
					 '("theorem" LaTeX-env-label)
					 '("remark" LaTeX-env-label)))))

(sb-init-auctex)


(defun sb-init-reftex ()
  (require 'reftex)

  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  (setq reftex-load-hook (quote (imenu-add-menubar-index))
	reftex-mode-hook (quote (imenu-add-menubar-index))
	reftex-plug-into-AUCTeX t
	reftex-insert-label-flags (quote (nil nil))
	reftex-ref-macro-prompt nil
	reftex-label-alist
	'(("axiom"   ?a "ax:"  "~\\ref{%s}" nil ("axiom"   "ax.") -2)
	  ("theorem" ?h "thr:" "~\\ref{%s}" nil ("theorem" "th.") -3)
	  ("remark"  ?r "rem:" "~\\ref{%s}" t   ("remark" "rem.") -4))))

(sb-init-reftex)


(defun sb-init-bratex ()
  (add-to-list 'load-path "~/.emacs.d/lisp/bratex")
  (require 'bratex)
  (add-hook 'LaTeX-mode-hook #'bratex-config))

(sb-init-bratex)


(defun sb-init-c ()
  (setq-default c-basic-offset 4))

(sb-init-c)

;; ┌─────────────────────────────┐
;; │ Python programming language │
;; └─────────────────────────────┘

(custom-add-to-group 'sb
		     'python-shell-interpreter-args 'custom-variable)
(custom-add-to-group 'sb
		     'python-shell-interpreter-interactive-arg 'custom-variable)

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
      python-shell-prompt-detect-enabled nil
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]:"
      python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
(add-hook 'python-mode-hook (lambda() (setq show-trailing-whitespace t)))

(elpy-enable)
(setq elpy-modules (quote (elpy-module-eldoc
                           elpy-module-flymake
                           elpy-module-sane-defaults)))
(setq elpy-test-runner (quote elpy-test-test-discover-runner))
(add-hook 'elpy-mode-hook 'whitespace-mode)

;; ┌────────────────────────────────┐
;; │ Maxima computer algebra system │
;; └────────────────────────────────┘

(defun sb-set-maxima-mode-path (symbol value)
  "Setter for the `sb-maxima-mode-path' custom variable."
  (progn (when (boundp symbol) (delete (default-value symbol) load-path))
	 (add-to-list 'load-path value)
	 (set-default symbol value)))

(defun sb-init-maxima-mode-path (symbol value)
  "Initializer for the `sb-maxima-mode-path' custom variable."
  (progn (add-to-list 'load-path value)
	 (custom-initialize-reset symbol value)))

(defcustom sb-maxima-mode-path ""
  "Path to the folder hosting elisp files for maxima-mode.

This is the path to the files: maxima.el, maxima-font-lock.el. On
windows platforms, it is something like:

    C:\\maxima-5.40.0\\share\\maxima\\5.40.0\\emacs"
  :type 'string :group 'sb :tag "Path to maxima-mode files"
  :initialize 'sb-init-maxima-mode-path :set 'sb-set-maxima-mode-path)

(autoload 'maxima-mode "maxima" "Major mode for writing Maxima programs" t)
(autoload 'maxima "maxima" "Run Maxima interactively" t)
(setq auto-mode-alist (cons '("\\.ma[cx]" . maxima-mode)
			    auto-mode-alist))

;; ┌─────────────────────┐
;; │ Ispell and hunspell │
;; └─────────────────────┘

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
		 ("\\\\cite[tp]"        ispell-tex-arg-end)
		 ))
       (cadr ispell-tex-skip-alists)))

;; ┌────────────────────────────┐
;; │ Blog related configuration │
;; └────────────────────────────┘

(defun sb-load-file-when-exists (filename)
  "Load specified file if it exists. Do nothing otherwise."
  (when (file-exists-p filename) (load-file filename)))

(sb-load-file-when-exists (expand-file-name "blog/sb-blog.el"
                                            sb-path-to-local-documents))
