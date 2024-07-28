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
    ;;(sb-package-install-unless-installed 'auctex)
    ;; (sb-package-install-unless-installed 'counsel)
    ;; (sb-package-install-unless-installed 'elpy)
    ;; (sb-package-install-unless-installed 'ivy)
    ;; (sb-package-install-unless-installed 'julia-mode)
    (sb-package-install-unless-installed 'magit)
    (sb-package-install-unless-installed 'markdown-mode)
    (sb-package-install-unless-installed 'quarto-mode)
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

(defun sb-init-mail ()
  "Initialize mail-mode.

The following variables should be custom-set

- `smtpmail-default-smtp-server',
- `smtpmail-smtp-service',
- `smtpmail-smtp-user',
- `user-full-name',
- `user-mail-address'.

For these variables to be clickable, first require `smtpmail'."
  (setq mail-self-blind t
	mail-user-agent 'sendmail-user-agent
	send-mail-function 'smtpmail-send-it
	smtpmail-stream-type 'ssl))

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


(defun sb-init-auctex ()
  "Initialize the AUCTeX package.

Update `TeX-view-program-selection' to chose the viewer.

SumatraPDF and qpdfview are preconfigured, however, they do not appear
in the list of available viewers.

The full path to the SumatraPDF executable must be defined in
`sb-sumatrapdf-path`.

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

 (setq TeX-view-program-list '(("SumatraPDF" "C:\\Progra~1\\SumatraPDF\\SumatraPDF.exe -reuse-instance -forward-search %b %n %o") nil))

  (setq TeX-view-program-list
	'(("qpdfview"
	   ("qpdfview --unique %o"
	    (mode-io-correlate "#src:%b:%n:0"))
	   "qpdfview")
	  ("SumatraPDF"
	   ((concat sb-sumatrapdf-path " -reuse-instance -forward-search %b %n %o")
	   "SumatraPDF.exe"))))

 ;; (setq TeX-view-program-selection
 ;;       '(((output-dvi style-pstricks)
 ;; 	  "dvips and start")
 ;; 	 (output-dvi "Yap")
 ;; 	 (output-pdf "SumatraPDF")
 ;; 	 (output-html "start")))

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


(defun sb-init-c ()
  (setq-default c-basic-offset 2))

(defun sb-init-python ()
  ;; (setq python-shell-interpreter "jupyter"
  ;; 	python-shell-interpreter-args "console --simple-prompt"
  ;; 	python-shell-prompt-detect-failure-warning nil)
  (add-hook 'python-mode-hook (lambda() (setq show-trailing-whitespace t)))
  (add-hook 'python-mode-hook 'whitespace-mode)

  ;; (elpy-enable)
  ;; (setq elpy-modules (quote (elpy-module-eldoc
  ;;                            elpy-module-flymake
  ;;                            elpy-module-sane-defaults)))
  ;; (setq elpy-test-runner (quote elpy-test-test-discover-runner))
  ;; (add-hook 'elpy-mode-hook 'whitespace-mode)
  )

(defun sb-init-julia()
  (require 'julia-mode))

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


(defun sb-init-quarto-mode ()
  (require 'quarto-mode))


(defun sb-init-yaml-mode()
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


(defun sb-init-all()
  (sb-init-vanilla-emacs)
;;  (sb-init-package)
  (sb-init-whitespace)
  (sb-init-input-method)
  (sb-init-epa-file)
  (sb-init-key-bindings-and-keymaps)
  (sb-init-appearance)
  (sb-init-ibuffer)
  (sb-init-flymake)
  ;;(sb-init-auctex)
  ;;(sb-init-reftex)
  (sb-init-bratex)
  (sb-init-c)
  ;;(sb-init-python)
  ;;(sb-init-julia)
  (sb-init-maxima)
  (sb-init-magit)
  (sb-init-quarto-mode)
  (sb-init-yaml-mode)
  )

(use-package julia-mode
  :ensure t
  :mode "\\.jl$")

;; (use-package eglot-jl
;;   :ensure t
;;   :after julia-mode)

;; (use-package julia-ts-mode
;;   :ensure t
;;   :mode "\\.jl$")

(sb-init-all)
