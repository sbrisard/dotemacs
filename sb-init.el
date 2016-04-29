;; -*- coding: utf-8 -*-

;; Emacs Lisp Package Archive (ELPA)
;; =================================

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

(require 'f)

;; Customizable variables
;; ======================

(defgroup sb nil "My customization group" :tag "SB")
(defcustom sb-path-to-cloud "~/Google Drive"
  "TODO Documentation"
  :type 'string :group 'sb :tag "Path to cloud")
(defcustom sb-path-to-local-documents "~/Documents"
  "TODO Documentation"
  :type 'string :group 'sb :tag "Path to local documents")
(defcustom sb-path-to-maxima-mode ""
  "TODO Documentation"
  :type 'string :group 'sb :tag "Path to maxima-mode files")

;; Variables and functions defined in this file
;; ============================================

;; Predicates to check whether emacs is run from Windows, Linux or MacOS.
(defvar windowsp (string-equal "windows-nt" (symbol-name system-type)))
(defvar darwinp (string-equal "darwin" (symbol-name system-type)))
(defvar linuxp (string-equal "gnu/linux" (symbol-name system-type)))

(defun sb-bind-newline-and-indent-to-RET ()
  "Bind newline-and-indent to RET. This function can be used as a hook."
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun sb-load-file-when-exists (filename)
  "Load specified file if it exists. Do nothing otherwise."
  (when (file-exists-p filename) (load-file filename)))

;; Toggle window dedication
;; http://stackoverflow.com/questions/5151620/how-do-i-make-this-emacs-frame-keep-its-buffer-and-not-get-resized
(defun sb-toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Miscellaneous
;; =============

(prefer-coding-system 'utf-8)

(setq column-number-mode t)
(setq confirm-nonexistent-file-or-buffer t)
(setq-default default-directory "~/" )
(setq default-process-coding-system 'utf-8)
(setq dnd-open-file-other-window nil)
(setq find-file-run-dired nil)
(setq inhibit-startup-screen t)
(setq initial-frame-alist '((top . 0) (left . 0)))
(setq make-backup-files nil)
(setq ps-paper-type (quote a4))
(setq safe-local-variable-values (quote ((buffer-auto-save-file-name)
                                         (org-confirm-babel-evaluate))))
(setq visible-bell t)

;;(setq-default buffer-file-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)

(global-auto-revert-mode)
(global-hl-line-mode)
(menu-bar-mode)
;(scroll-bar-mode -1)
(show-paren-mode)
(tool-bar-mode -1)

(global-unset-key (kbd "<C-next>"))
(global-unset-key (kbd "<C-prior>"))

;; Under Mac OS X, right-alt must be mapped to Alt Gr.
(when darwinp
  (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'meta))

;; On Windows 7, HOME is set to "C:/Users/brisard/AppData/Roaming/", which
;; allows correct location of ".emacs.d", but leads to incorrect value of "~".
;;(setenv "HOME" "C:/Users/brisard/")

(when window-system
  (load-theme 'zenburn t)
  (split-window-right))

;; Ido
;; ===

(ido-mode t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order (quote (".org" ".tex" ".bib" ".html" ".py")))
(setq ido-ignore-extensions nil)
(setq ido-ignore-files
      (quote
       ("\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".*~" ".*\\.aux\'" ".*\\.bbl\'" ".*\\.blg\'" ".*\\.bst\'" ".*\\.log\'" ".*\\.out\'" ".*\\.pdf\'" ".*\\.spl\'" ".*\\.synctex\\.gz\'" ".*\\.toc\'" ".*\\.o\'" ".*\\.so\'" ".*\\.a\'" ".*\\.elc\'" ".*\\.pyc\'" ".*\\.pyo\'" "\\.git/")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Trailing whitespaces
;; ====================

;; In selected modes, show trailing whitespaces and empty lines at the end of
;; the buffer. This is defined as a mode hook (which requires a function).
(add-hook 'python-mode-hook (lambda() (setq show-trailing-whitespace t)))

;; In all modes, empty lines at the end of the buffer are shown, and trailing
;; white spaces are remoaved when buffer is saved.
(setq-default indicate-empty-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Easy PG (GnuPG interface for Emacs)
;; ===================================

(require 'epa-file)
(epa-file-enable)

;; Whitespace mode (minor mode to visualize blanks)
;; ================================================

;; Here are the UTF-8 values used below
;;   - U+0009 TAB
;;   - U+000A LINE FEED
;;   - U+0020 SPACE
;;   - U+0024 DOLLAR SIGN             $
;;   - U+002E FULL STOP               .
;;   - U+003E GREATER THAN SIGN       >
;;   - U+00B6 PILCROW SIGN            ¶
;;   - U+00B7 MIDDLE DOT              ·
;;   - U+2192 RIGHTWARDS ARROW        →
;;   - U+21E5 RIGHTWARDS ARROW TO BAR ⇥
;;   - U+23CE RETURN SYMBOL          ⏎

(require 'whitespace)

(setq whitespace-line-column 80)

(setq whitespace-display-mappings
      '((space-mark #x0020  [#x00B7] [#x002E])
        (newline-mark #x00A [#x00B6 #x000A] [#x0024 #x000A])
        (tab-mark #x009 [#x2192 #x009] [#x003E #x009])
        ))

(setq whitespace-style (quote (face lines-tail)))

;; elpy
;; ====

(elpy-enable)
(setq elpy-modules (quote (elpy-module-eldoc
                           elpy-module-flymake
                           elpy-module-sane-defaults)))
(setq elpy-test-runner (quote elpy-test-pytest-runner))
(add-hook 'elpy-mode-hook 'whitespace-mode)

;; Org Mode
;; ========

;; This should not be necessary
;;(add-to-list 'load-path (concat user-emacs-directory "/elisp/org-mode/lisp"))

;; For fontification
(require 'htmlize)

;; Allow for links to be used outside Org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c L") 'org-insert-link-global)
(global-set-key (kbd "C-c o") 'org-open-at-point-global)

(org-babel-do-load-languages 'org-babel-load-languages '((C . t)
                                                         (python . t)
                                                         (maxima . t)))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(setq org-agenda-file-regexp "\\`[^.].*\\.\\(org\\|txt\\)\\'")
(setq org-agenda-files
      (let ((root (f-join sb-path-to-cloud "notes")))
        (append (list root user-emacs-directory)
                (f-directories root (lambda (dir) (not (s-contains? "archives" dir)))))))
;; Restore window configuration upon exiting agenda.
(setq org-agenda-restore-windows-after-quit t)
;; Show agenda in the current window, keeping all other windows.
(setq org-agenda-window-setup 'current-window)
(setq org-archive-location (f-join sb-path-to-cloud "notes" "archives" "%s_archive::"))
;; Potentially dangerous
(setq org-confirm-babel-evaluate nil)
(setq org-export-preserve-breaks nil)
(setq org-export-time-stamp-file t)
(setq org-export-with-archived-trees 'headline)
(setq org-export-with-author t)
(setq org-export-with-clocks nil)
(setq org-export-with-creator 'comment)
(setq org-export-with-date t)
(setq org-export-with-drawers '(not "LOGBOOK"))
(setq org-export-with-email nil)
(setq org-export-with-emphasize t)
(setq org-export-with-entities t)
(setq org-export-with-fixed-width t)
(setq org-export-with-footnotes t)
(setq org-export-with-inlinetasks t)
(setq org-export-with-planning nil)
(setq org-export-with-priority nil)
(setq org-export-with-section-numbers nil)
(setq org-export-with-smart-quotes nil)
(setq org-export-with-special-strings t)
(setq org-export-with-statistics-cookies t)
(setq org-export-with-sub-superscripts t)
(setq org-export-with-tables t)
(setq org-export-with-tags t)
(setq org-export-with-tasks t)
(setq org-export-with-timestamps t)
(setq org-export-with-toc nil)
(setq org-export-with-todo-keywords t)
(setq org-html-htmlize-output-type 'css)
;; Visit files in same window
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file-other-window)
                                   (wl . wl-other-frame))))
(setq org-log-into-drawer t)
(setq org-startup-truncated nil) ;; Wrap lines
(setq org-startup-folded nil) ;; Don't open org files in folded mode
(setq org-src-fontify-natively t)
(setq org-table-copy-increment nil)
(setq org-todo-keywords '((sequence "A_FAIRE" "EN_ATTENTE" "UN_JOUR" "|"
                                    "FAIT")
                          (sequence "TODO" "|" "DONE")))
(setq system-time-locale "C") ;; Make sure that timestamps appear in English

;; Maxima
;; ======

(add-to-list 'load-path sb-path-to-maxima-mode)
(autoload 'maxima-mode "maxima" "Major mode for writing Maxima programs" t)
(autoload 'maxima "maxima" "Run Maxima interactively" t)
(setq auto-mode-alist (cons '("\\.ma[cx]" . maxima-mode) auto-mode-alist))

;; Magit
;; =====

;; (setq magit-use-overlays nil)
;; (setq magit-last-seen-setup-instructions "1.4.0")

;; Ispell
;; ======

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
                 ("\\\\cite[tp]"        ispell-tex-arg-end)
                 ))
       (cadr ispell-tex-skip-alists)))

;; AUCTeX and RefTeX
;; =================

(require 'tex)

(setq LaTeX-electric-left-right-brace t)
(setq TeX-auto-save nil)
(setq TeX-electric-math (quote ("\\(" . "\\)")))
(setq-default TeX-master nil)
(setq TeX-parse-self t)

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
       (add-to-list 'TeX-expand-list '("%(sumatra)" (lambda () (format "\"C:/opt/SumatraPDF-3.0/SumatraPDF.exe\""))))
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

(require 'reftex)

(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-insert-label-flags (quote ("s" "st")))
(setq reftex-load-hook (quote (imenu-add-menubar-index)))
(setq reftex-mode-hook (quote (imenu-add-menubar-index)))
(setq reftex-plug-into-AUCTeX t)

;; Custom function to insert BibTeX reference
;; ------------------------------------------

(defvar sb-path-to-biblio (f-join sb-path-to-local-documents "biblio"))

(defun sb-bibref-path (key)
  (f-join sb-path-to-biblio (s-left 1 key) key (concat key ".bib")))

(defun sb-insert-bibref (key)
  (interactive "sBibTeX key: \n")
  (insert-file-contents (sb-bibref-path (downcase key))))

;; Ob-ipython
;; ==========

(require 'ob-ipython)
(setq ob-ipython-kernel-extra-args (quote ("--profile=ob-ipython")))

;; ccmode
;; ======

(setq-default c-basic-offset 4)

;; key-chord
;; =========

(key-chord-mode 1)


;; ace-window
;; ==========

(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)
(set-face-attribute 'aw-leading-char-face nil :foreground nil :inherit 'compilation-mode-line-fail)

(org-babel-load-file (concat user-emacs-directory "sb-config.org"))

;; Blog-specific customizations
;; ============================

(sb-load-file-when-exists (f-join sb-path-to-local-documents "blog/sb-blog.el"))
