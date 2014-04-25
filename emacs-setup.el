
;; Contents
;; ========
;;
;; If the present file has been loaded, follow the links below with C-c o.
;; Otherwise, use M-x org-open-at-point-global.
;;
;; [[vars][Variables defined in this file]]
;; [[funs][Functions defined in this file]]
;; [[no-window][No-window configuration]]
;; [[appearance][Visual appearance in window-system mode]]
;; [[ispell][Ispell]]
;; [[magit][Magit]]
;; [[latex][LaTeX]]
;; [[maxima][Maxima]]
;; [[org][Org Mode]]
;; [[python][Python/Cython]]
;;
;;
;; In init.el, a variable should be defined
;; sb-path-to-emacs-setup
;; which points to the path to this file.

;; Variables defined in this file                                      <<vars>>
;; ==============================

;; Predicates to check whether emacs is run from Windows, Linux or MacOS.
(defvar windowsp (string-equal "windows-nt" (symbol-name system-type)))
(defvar darwinp (string-equal "darwin" (symbol-name system-type)))
(defvar linuxp (string-equal "gnu/linux" (symbol-name system-type)))

(cond (windowsp
       (defvar sb-default-frame-width 80)
       (defvar sb-default-frame-height 53)
       (defvar sb-default-frame-left 514))
      (darwinp
       (defvar sb-default-frame-width 80)
       (defvar sb-default-frame-height 57)
       (defvar sb-default-frame-left 504))
      (linuxp
       (defvar sb-default-frame-width 80)
       (defvar sb-default-frame-height 45)
       (defvar sb-default-frame-left 501)))

;; Local directory for emacs extensions.
(defvar path-to-site-lisp (concat sb-path-to-emacs-setup "site-lisp/"))
(add-to-list 'load-path path-to-site-lisp)
(let ((default-directory path-to-site-lisp))
  (normal-top-level-add-subdirs-to-load-path))

;; Functions defined in this file                                      <<funs>>
;; ==============================

(defun set-newline-and-indent ()
  "Bind newline-and-indent to RET. This function can be used as a hook."
  (local-set-key (kbd "RET") 'newline-and-indent))

;; No-window-system                                               <<no-window>>
;; ================

;; This section should be executed when emacs is run in both window-system and
;; no-window-system modes.

;; Miscellaneous
;; -------------

(setq-default buffer-file-coding-system 'iso-latin-1-unix)
(setq-default indent-tabs-mode nil)

(setq column-number-mode t)
(setq confirm-nonexistent-file-or-buffer t)
(setq default-directory "~/" )                        ; TODO Is this necessary?
(setq dnd-open-file-other-window nil)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq ps-paper-type (quote a4))
(setq visible-bell t)

(filesets-init)
(global-hl-line-mode)
(menu-bar-mode nil)
(set-scroll-bar-mode nil)
(show-paren-mode t)
(tool-bar-mode -1)

;; Trailing whitespaces
;; --------------------

;; In selected modes, show trailing whitespaces and empty lines at the end of
;; the buffer. This is defined as a mode hook (which requires a function).
(add-hook 'python-mode-hook (lambda() (setq show-trailing-whitespace t)))

;; In all modes, empty lines at the end of the buffer are shown, and trailing
;; white spaces are remoaved when buffer is saved.
(setq-default indicate-empty-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Emacs Lisp Package Archive (ELPA)
;; ---------------------------------

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; Platform-specific customizations
;; --------------------------------

;; Under Mac OS X, right-alt must be mapped to Alt Gr.
(when darwinp
  (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'meta))

;; On Windows 7, HOME is set to "C:/Users/brisard/AppData/Roaming/", which
;; allows correct location of ".emacs.d", but leads to incorrect value of "~".
;;(setenv "HOME" "C:/Users/brisard/")


;; Visual appearance in window-system mode                       <<appearance>>
;; =======================================

;; Geometry
;; --------

(setq default-frame-alist `((top . 0)
                            (left . ,sb-default-frame-left)
                            (width . ,sb-default-frame-width)
                            (height . ,sb-default-frame-height)))

(setq initial-frame-alist '((top . 0) (left . 0)))

;; Color theme
;; -----------

;; M-x customize-themes displays a selectable list of custom themes.
(load-theme 'solarized-light t)

;; Fonts
;; -----

;; Under Windows, the procedure to get the right font name is the following
;;   1. switch to the "*scratch*" buffer
;;   2. type (w32-select-font) followed by C-j (or M-x eval-print-last-sexp)
;;   3. select the desired font from the menu dialog that ensues
;;   4. copy the string that is displayed (something like:
;;      "-outline-Lucida Sans Typewriter-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1")
;;      and paste it into the .emacs.d/init.el file. The line to add is
;;          (set-face-font 'default "fontname")
;;      where fontname is the copied string.

(cond (windowsp (defvar sb-monospaced-font "Consolas-8"))
      (darwinp (defvar sb-monospaced-font "Inconsolata-10"))
      (linuxp (defvar sb-monospaced-font "EnvyCodeR-9")))

(defvar sb-variable-pitch-font sb-monospaced-font)

; TODO remove this conditional (run only in graphics mode.
(when window-system
  (set-face-font 'default sb-monospaced-font)
  (set-face-attribute 'variable-pitch nil :font sb-monospaced-font))

;; Ispell                                                            <<ispell>>
;; ======

; TODO simplify this section

(setq ispell-dictionary "francais")
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
                 ))
       (cadr ispell-tex-skip-alists)))

(eval-after-load "ispell"
  '(let ((list (car ispell-tex-skip-alists)))
     (add-to-list 'list '("\\\\cite[tp]" ispell-tex-arg-end))
     (setcar ispell-tex-skip-alists list)))

;; Magit (Emacs mode for Git)                                         <<magit>>
;; ==========================

(delete 'Git vc-handled-backends)

(when windowsp
  (setq exec-path (append exec-path
                          '("C:/Program Files (x86)/Git/bin/"))))

;; LaTeX                                                              <<latex>>
;; =====

;; AUCTeX
;; ------

(require 'tex)
(require 'preview)
;; (require 'tex-mik)
;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)

;; AUCTeX multifile
(setq TeX-parse-self t) ; Enable parse on load.
;;(setq TeX-auto-save t) ; Enable parse on save.

(setq font-latex-match-reference-keywords '(("citeauthor" "*{")
                                            ("citetext" "{")
                                            ("citeyear" "{")
                                            ("citeyearpar" "{")
                                            ("citep" "*[{")
                                            ("citet" "*[{")
                                            ("citealt" "*[{")
                                            ("citealp" "*[{")))

(setq LaTeX-command "latex")
(setq-default TeX-PDF-mode t)
(setq TeX-command "tex")
(setq TeX-source-correlate-method (quote synctex))
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)

(cond (windowsp
       (add-to-list 'TeX-expand-list '("%(sumatra)" (lambda () (format "\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\""))))
       (add-to-list 'TeX-view-program-list '("SumatraPDF" "%(sumatra) -reuse-instance -forward-search %b %n %o"))
       (setq TeX-view-program-selection '((output-pdf "SumatraPDF"))))

      (darwinp
       (add-to-list 'TeX-expand-list '("%(skim)" (lambda () (format "/Applications/Skim.app/Contents/SharedSupport/displayline"))))
       (add-to-list 'TeX-view-program-list '("Skim" "%(skim) -r -b %n %o %b"))
       (setq TeX-view-program-selection '((output-pdf "Skim")))))

;; From the AUCTeX FAQ
;;     When writing the log file, TeX puts information related to a file,
;;     including error messages, between a pair of parentheses. AUCTeX
;;     determines the file where the error happened by parsing the log file and
;;     counting the parentheses. This can fail when there are other, unbalanced
;;     parentheses present.
;;
;;     As a workaround you can activate so-called file:line:error messages for
;;     the log file. (Those are are easier to parse, but may lack some details.)
;;     Either you do this in the configuration of your TeX system (consult its
;;     manual to see where this is) or you add a command line switch to the
;;    (la)tex call, e.g. by customizing LaTeX-command-style or
;;    TeX-command-list.
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

;; RefTeX
;; ------

(require 'reftex)

;; Turn on RefTeX in emacs latex mode
(add-hook 'latex-mode-hook 'turn-on-reftex)

;; Turn on RefTeX in AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Add the buffer's index to the menu bar in latex mode
(setq reftex-load-hook (quote (imenu-add-menubar-index)))
(setq reftex-mode-hook (quote (imenu-add-menubar-index)))

;; Turn on the RefTeX-AUCTeX interface. When on, both packages interact
;; closely.
(setq reftex-plug-into-AUCTeX t)

(setq reftex-insert-label-flags (quote ("s" "st")))

;; Maxima                                                            <<maxima>>
;; ======

(autoload 'maxima-mode "maxima" "Major mode for writing Maxima programs" t)
(autoload 'maxima "maxima" "Run Maxima interactively" t)
(setq auto-mode-alist (cons '("\\.ma[cx]" . maxima-mode) auto-mode-alist))

(cond (windowsp
       (setq maxima-command "C:/Program Files (x86)/Maxima-5.30.0/bin/maxima.bat"))
      (when darwinp
        (setq maxima-command "/opt/local/bin/maxima")))

;; Org Mode                                                             <<org>>
;; ========

(setq org-startup-truncated nil)                                  ;; Wrap lines
(setq org-startup-folded nil)            ;; Don't open org files in folded mode
(setq system-time-locale "C")    ;; Make sure that timestamps appear in English

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cL" 'org-insert-link-global)  ;; Allow for links to be used
(global-set-key "\C-co" 'org-open-at-point-global)               ;; outside Org

;; Visit files in same window
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file)
                                   (wl . wl-other-frame))))

;; Fontification
;; -------------

(require 'htmlize)
(setq org-src-fontify-natively t)
(setq org-html-htmlize-output-type 'css)
(org-babel-do-load-languages 'org-babel-load-languages '((C . t)
                                                         (python . t)))

;; Location of files
;; -----------------

(setq org-agenda-files sb-org-agenda-files)

(when (boundp 'sb-path-to-blog)
  (defvar sb-path-to-blog-base-directory (concat sb-path-to-blog "org"))
  (defvar sb-path-to-blog-publishing-directory (concat sb-path-to-blog "html"))

  ;; From http://lists.gnu.org/archive/html/emacs-orgmode/2008-11/msg00571.html
  ;;
  ;; Hi Richard,
  ;;
  ;; no, variables are not interpolated into quoted lists,
  ;; any list preceded by "'" is quoted.
  ;;
  ;; If you can guarantee that the value of the variables is define at the
  ;; time the
  ;;
  ;;   (setq org-publish-projects-alist ...
  ;;
  ;; is executed, then you can use backquote syntax: Quote the main list with
  ;; the backquote, and then preceed any variable inside you would like to
  ;; have evaluated with a comma so
  ;;
  ;; (setq org-publish-projects-alist
  ;;        `( .............
  ;;            ,rgr-souerce
  ;;            ....))
  ;;
  ;; Note that this works only once, so if you later change the value, this
  ;; list will not be changed.
  ::
  ;; If you wanted dynamic behavior, then we would have to patch
  ;; org-publish.el.
  ;;
  ;; HTH
  ;;
  ;; - Carsten
  ;;
  (setq org-publish-project-alist
        `(("blog-orgfiles"
           :base-directory ,sb-path-to-blog-base-directory
           :publishing-directory ,sb-path-to-blog-publishing-directory
           :base-extension "org"
           :exclude "header.org"
           :recursive t
           :publishing-function org-html-publish-to-html
           :section-numbers nil
           :with-toc nil)
          ("blog-images"
           :base-directory ,sb-path-to-blog-base-directory
           :publishing-directory ,sb-path-to-blog-publishing-directory
           :base-extension "jpg\\|gif\\|png"
           :recursive t
           :publishing-function org-publish-attachment)
          ("blog-other"
           :base-directory ,sb-path-to-blog-base-directory
           :publishing-directory ,sb-path-to-blog-publishing-directory
           :base-extension "css"
           :recursive t
           :publishing-function org-publish-attachment)
          ("blog"
           :components ("blog-orgfiles" "blog-images" "blog-other")))))

;; Python/Cython                                                     <<python>>
;; =============

(when window-system
  ;; python-mode settings
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  ;; cython-mode
  (require 'cython-mode)
  ;; pydoc-info is an Emacs package for searching and browsing the new Python
  ;;  documentation in the Info browser.
  (require 'pydoc-info)
)


(add-hook 'python-mode-hook 'set-newline-and-indent)



;; Under Linux, starting emacs from the desktop environment results in the
;; PATH variable being different from what it would be if started from a shell.

;; (let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
;;   (setenv "PATH" path)
;;   (setq exec-path
;;         (append
;;          (split-string-and-unquote path ":")
;;          exec-path)))

;;
;; Location of additional info manuals
;; -----------------------------------
;;

(when (boundp 'sb-path-to-info)
 (add-to-list 'Info-directory-list sb-path-to-info))
