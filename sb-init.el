;;; -*- lexical-binding: t -*-


;;; Utility functions

(defun sb-load-file-when-exists (filename)
  "Load specified file if it exists. Do nothing otherwise."
  (when (file-exists-p filename) (load-file filename)))


(defun sb-insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S")))


(defun sb-monospace-face-p (face)
  "Return non-nil if FACE is rendered with a monospace font.
Only meaningful in a graphical frame."
  (= (string-pixel-width (propertize (make-string 10 ?i) 'face face))
     (string-pixel-width (propertize (make-string 10 ?M) 'face face))))


(defun sb-set-fixed-pitch-family ()
  "Set the family of `fixed-pitch' to that of `default' if it is monospace.
Otherwise, use the first installed font in a list of fallbacks. This
matters for modes such as `markdown-mode', whose code and table faces
inherit from `fixed-pitch'."
  (when (display-graphic-p)
    (let ((family
	   (if (sb-monospace-face-p 'default)
	       (face-attribute 'default :family)
	     (seq-find (lambda (f) (find-font (font-spec :family f)))
		       '("JuliaMono" "Cascadia Mono" "Consolas" "DejaVu Sans Mono")))))
      (if family
	  (set-face-attribute 'fixed-pitch nil :family family)
	(message "sb-set-fixed-pitch-family: no monospace font found")))))


;;; Initialization functions

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


(defun sb-init-package ()
  "Initialize package manager and download uninstalled packages."
  (let ((contents-refreshed nil))
    (require 'package)
    (setq package-archives
	  '(("gnu" . "https://elpa.gnu.org/packages/")
            ("melpa" . "https://melpa.org/packages/")
            ("melpa-stable" . "https://stable.melpa.org/packages/")
            ))
    (if (< emacs-major-version 27) (package-initialize))
    (defun sb-package-install-unless-installed (pkg)
      (unless (package-installed-p pkg)
	(unless contents-refreshed
	  (package-refresh-contents)
	  (setq contents-refreshed t))
	(package-install pkg)))
    (sb-package-install-unless-installed 'auctex)
    (sb-package-install-unless-installed 'flymake-ruff)
    (sb-package-install-unless-installed 'julia-mode)
    (sb-package-install-unless-installed 'magit)
    (sb-package-install-unless-installed 'markdown-mode)
    (sb-package-install-unless-installed 'reformatter)
    (sb-package-install-unless-installed 'yaml-mode)
    ))

(defun sb-init-whitespace ()
  "Initialize whitespace package."
  (require 'whitespace)
  (setq whitespace-line-column 80
	whitespace-style (quote (face lines-tail)))
  (custom-theme-set-faces 'user '(whitespace-line ((default . (:inherit flymake-note))) t))
  )

(defun sb-init-input-method ()
  (setq default-input-method 'rfc1345)
  (mapc (lambda (mode)
	  (add-hook mode (lambda ()
			   (activate-input-method default-input-method))))
	'(org-mode-hook
	  text-mode-hook)))

(defun sb-init-epa-file ()
  "Initialize GnuPG interface for Emacs.

The following variables must be custom-set

- `epg-gpg-program'.
"
  (require 'epa-file)
  (epa-file-enable)
  (setq epg-pinentry-mode 'loopback))


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

  ;; My personnal keymap is called `sb-map`, and the prefix key that
  ;; is assigned to this keymap is `C-&`.
  (define-prefix-command 'sb-map)
  (global-set-key (kbd "C-&") 'sb-map)
  (define-key sb-map (kbd "C") 'sb-git-stage-commit-and-push-all)
  (define-key sb-map (kbd "t") 'sb-insert-timestamp))


(defun sb-init-appearance ()
  "Initialize appearance of Emacs (fonts, themes, etc.)."
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (load-theme 'tango)

  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  ;; Disable window decorations
  ;;(set-frame-parameter nil 'undecorated t)
  (split-window-right)

  ;; Use w32-select-font
  (set-face-font 'default "JuliaMono-11")
  (sb-set-fixed-pitch-family)
  )

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

(defun sb-init-flymake ()
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))


(defun sb-init-raise-frame ()
  (select-frame-set-input-focus (selected-frame)))


(defun sb-find-sumatrapdf ()
  "Return the path to the SumatraPDF executable, or nil if not found.
Look in `exec-path' first, then in C:/opt/SumatraPDF-*/; if several
versions are installed there, return the most recent one."
  (or (executable-find "SumatraPDF")
      (car (last (sort (file-expand-wildcards
			"C:/opt/SumatraPDF-*/SumatraPDF*.exe")
		       #'string-version-lessp)))))


(defun sb-set-pdf-viewer (viewer)
  "Use VIEWER for PDF output in `TeX-view-program-selection'."
  (setq TeX-view-program-selection
	(cons (list 'output-pdf viewer)
	      (seq-remove (lambda (entry) (eq (car entry) 'output-pdf))
			  TeX-view-program-selection))))

(defun sb-init-auctex ()
  "Initialize the AUCTeX package.

The PDF viewer depends on the system: qpdfview under Linux, SumatraPDF
under Windows (found by `sb-find-sumatrapdf'; the default \"start\"
viewer is used if it is not found).

Inverse search is configured in Sumatra under File → Preferences → Options…

    \"C:\\opt\\emacs-28.1\\bin\\emacsclient.exe -n +%l \"%f\"\"

In qpdfview, set the \"Source Editor\" in under
Edit → Settings → Behavior to:

    \"emacsclient +%2:%3 \"%1\"
"
  (require 'tex)

  (setq LaTeX-command "latex"
	LaTeX-electric-left-right-brace t
	TeX-auto-save t
	TeX-command "tex"
	TeX-electric-math (quote ("\\(" . "\\)"))
	TeX-parse-self t
	TeX-PDF-mode t
	TeX-raise-frame-function #'sb-init-raise-frame
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

  (pcase system-type
    ('windows-nt
     (let ((sumatra (sb-find-sumatrapdf)))
       (if (not sumatra)
	   (progn
	     (message "sb-init-auctex: SumatraPDF not found, using \"start\"")
	     (sb-set-pdf-viewer "start"))
	 ;; Same name as the built-in viewer, which it overrides, but
	 ;; with the full path to the executable.
	 (add-to-list 'TeX-view-program-list
		      `("SumatraPDF"
			(,(format "\"%s\" -reuse-instance" sumatra)
			 (mode-io-correlate " -forward-search \"%b\" %n")
			 " %o")
			,sumatra))
	 (sb-set-pdf-viewer "SumatraPDF"))))
    ('gnu/linux
     (add-to-list 'TeX-view-program-list
		  '("qpdfview"
		    ("qpdfview --unique %o"
		     (mode-io-correlate "#src:%b:%n:0"))
		    "qpdfview"))
     (sb-set-pdf-viewer "qpdfview")))

  (add-hook 'LaTeX-mode-hook 'whitespace-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (LaTeX-add-environments
					 '("axiom" LaTeX-env-label)
					 '("theorem" LaTeX-env-label)
					 '("remark" LaTeX-env-label)
					 '("definition" LaTeX-env-label)
					 '("example" LaTeX-env-label)
					 '("question" LaTeX-env-label)
					 '("problem" LaTeX-env-label)))))

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


(defun sb-init-bratex ()
  (add-to-list 'load-path "~/.emacs.d/lisp/bratex")
  (require 'bratex)
  (add-hook 'LaTeX-mode-hook #'bratex-config))


(defun sb-init-prettify-symbols-mode ()
  "Enable `prettify-symbols-mode' in LaTeX buffers, with per-file extra symbols.
Extra symbols are read from the file-local variable `sb-tex-prettify-extra'."
  (defvar-local sb-tex-prettify-extra nil
    "Extra (STRING . SYMBOL) pairs for `prettify-symbols-mode' in this buffer.
Meant to be set as a file-local variable in .tex files, for example:

  %%% sb-tex-prettify-extra: ((\"\\\\tsigma\" . ?𝛔) (\"\\\\grad\" . ?∇))")

  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (prettify-symbols-mode 1)
              ;; File-local variables are applied *after* the mode hooks,
              ;; hence the buffer-local hook.
              (add-hook 'hack-local-variables-hook
                        (lambda ()
                          (when sb-tex-prettify-extra
                            (setq-local prettify-symbols-alist
                                        (append sb-tex-prettify-extra
                                                prettify-symbols-alist))
                            ;; Toggle to rebuild the font-lock keywords.
                            (when prettify-symbols-mode
                              (prettify-symbols-mode -1)
                              (prettify-symbols-mode 1))))
                        nil t))))


(defun sb-init-c ()
  (setq-default c-basic-offset 2))

(defun sb-init-python ()
  (use-package python
    :mode ("\\.py\\'" . python-mode)
    :config
    (setq python-indent-offset 4
	  python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt"
          python-shell-echo-input t))
  (use-package flymake-ruff
    :ensure t
    :hook (python-mode . flymake-ruff-load))
  (use-package reformatter
    :ensure t
    :config
    (reformatter-define ruff-format
      :program "ruff"
      :args '("format" "-")))
  (add-hook 'python-mode-hook #'whitespace-mode)
  (add-hook 'python-mode-hook #'flymake-mode)
  (add-hook 'python-mode-hook #'ruff-format-on-save-mode)
  )

(defun sb-init-maxima ()
  "Initialize maxima-mode.

The following variables must be custom-set

- `maxima-command',
- `sb-maxima-mode-path'.
"
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
    :type 'string :group nil :tag "Path to maxima-mode files"
    :initialize 'sb--init-maxima-mode-path :set 'sb--set-maxima-mode-path)

  (autoload 'maxima-mode "maxima" "Major mode for writing Maxima programs" t)
  (autoload 'maxima "maxima" "Run Maxima interactively" t)
  (setq auto-mode-alist (cons '("\\.ma[cx]" . maxima-mode) auto-mode-alist)))


(defun sb-init-magit ()
  (require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)

  (delete 'Git vc-handled-backends)
  ;; (setenv "GIT_ASKPASS" "git-gui--askpass")
  )

(defun sb-init-misc-modes ()
  "Initialize miscellaneous major modes."
  (use-package julia-mode    :ensure t :mode "\\.jl\\'")
  (use-package markdown-mode
    :ensure t
    :mode "\\.q?md\\'"
    :config
    (setq markdown-command "pandoc"
	  markdown-yank-dnd-method 'open))
  (use-package yaml-mode     :ensure t :mode "\\.yml\\'"))


;;; Entry point

(defun sb-init-all()
  (sb-init-vanilla-emacs)
  (sb-init-package)
  (sb-init-whitespace)
  (sb-init-input-method)
  (sb-init-epa-file)
  (sb-init-key-bindings-and-keymaps)
  (sb-init-appearance)
  (sb-init-ibuffer)
  (sb-init-flymake)
  (sb-init-auctex)
  (sb-init-reftex)
  (sb-init-bratex)
  (sb-init-prettify-symbols-mode)
  (sb-init-c)
  (sb-init-python)
  (sb-init-maxima)
  (sb-init-magit)
  (sb-init-misc-modes)
  )


(sb-init-all)
