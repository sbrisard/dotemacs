;; -*- coding: utf-8 -*-

;; +-------------------------------------------------+
;; | Additional customizations in window-system-mode |
;; +-------------------------------------------------+

;; Contents
;; ========

;; NOTA: if `init-window-system.el` has been loaded, follow the links below with
;;       C-c o. Otherwise, use M-x org-open-at-point-global.
;;
;; [[vars & funs][Variables and functions defined in this file]]
;; [[appearance][Visual appearance in window-system mode]]
;; [[ispell][Ispell]]
;; [[auto-complete][Auto-complete]]
;; [[magit][Magit]]
;; [[latex][LaTeX]]
;; [[maxima][Maxima]]
;; [[org][Org Mode]]
;; [[python][Python/Cython]]

;; Variables and functions defined in this file                 <<vars & funs>>
;; ============================================

(defvar sb-path-to-google-drive "~/Google Drive/")
(defvar sb-path-to-local-documents "~/Documents/")

;; Toggle window dedication
;; http://stackoverflow.com/questions/5151620/how-do-i-make-this-emacs-frame-keep-its-buffer-and-not-get-resized
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))


;; Visual appearance                                             <<appearance>>
;; =================

;; Geometry
;; --------

(cond (windowsp
       (setq default-frame-alist '((top . 0) (left . 514)
                                   (width . 163) (height . 44))))
      (darwinp
       (setq default-frame-alist '((top . 0) (left . 504)
                                   (width . 163) (height . 57))))
      (linuxp
       (setq default-frame-alist '((top . 0) (left . 501)
                                   (width . 163) (height . 41)))))

(setq initial-frame-alist '((top . 0) (left . 0)))

;; Windows
;; -------

;(split-window-below -7)
(split-window-right)
(windmove-default-keybindings)

;; Color theme
;; -----------

;; M-x customize-themes displays a selectable list of custom themes.

;; Do not use variable height fonts. These variables must be set prior to
;; loading the theme itself, for them to take effect.
;(setq solarized-height-minus-1 1.0)
;(setq solarized-height-plus-1 1.0)
;(setq solarized-height-plus-2 1.0)
;(setq solarized-height-plus-3 1.0)
;(setq solarized-height-plus-4 1.0)

;(load-theme 'solarized-light t)
(load-theme 'zenburn t)

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

(cond (windowsp (defvar sb-monospaced-font "Envy Code R-9"))
      (darwinp (defvar sb-monospaced-font "Envy Code R-11"))
      (linuxp (defvar sb-monospaced-font "EnvyCodeR-9")))

(defvar sb-variable-pitch-font sb-monospaced-font)

(set-face-font 'default sb-monospaced-font)
(set-face-attribute 'variable-pitch nil :font sb-monospaced-font)

;; Auto-complete                                              <<auto-complete>>
;; =============

;(require 'auto-complete)
;(ac-config-default)
;(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

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

(setq magit-use-overlays nil)

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

(cond (windowsp
       (add-to-list 'load-path "C:/Program Files (x86)/Maxima-5.30.0/share/maxima/5.30.0/emacs")
       (setq maxima-command "C:/Program Files (x86)/Maxima-5.30.0/bin/maxima.bat"))
      (darwinp
       (add-to-list 'load-path "/opt/local/share/maxima/5.33.0/emacs")
       (setq maxima-command "/opt/local/bin/maxima")))

(autoload 'maxima-mode "maxima" "Major mode for writing Maxima programs" t)
(autoload 'maxima "maxima" "Run Maxima interactively" t)
(setq auto-mode-alist (cons '("\\.ma[cx]" . maxima-mode) auto-mode-alist))

;; Org Mode                                                             <<org>>
;; ========

(setq org-startup-truncated nil)                                  ;; Wrap lines
(setq org-startup-folded nil)            ;; Don't open org files in folded mode
(setq system-time-locale "C")    ;; Make sure that timestamps appear in English

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c L") 'org-insert-link-global) ; Allow for links to be
(global-set-key (kbd "C-c o") 'org-open-at-point-global)    ; used outside Org

;; Visit files in same window
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
                                   (vm-imap . vm-visit-imap-folder-other-frame)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file)
                                   (wl . wl-other-frame))))

;; Show agenda in the current window, keeping all other windows.
(setq org-agenda-window-setup 'current-window)

;; Restore window configuration upon exiting agenda.
(setq org-agenda-restore-windows-after-quit t)

;; Fontification
;; -------------

(require 'htmlize)
(setq org-src-fontify-natively t)
(setq org-html-htmlize-output-type 'css)
(org-babel-do-load-languages 'org-babel-load-languages '((C . t)
                                                         (python . t)))

;; Location of files
;; -----------------

(setq org-agenda-files (mapcar (lambda(s) (concat sb-path-to-google-drive s))
                               '("notes/" "notes/biblio/")))

(defvar sb-path-to-blog (concat sb-path-to-local-documents "blog/"))
(defvar sb-path-to-blog-base-directory (concat sb-path-to-blog "org/"))
(defvar sb-path-to-blog-publishing-directory (concat sb-path-to-blog "html/"))

;; From http://lists.gnu.org/archive/html/emacs-orgmode/2008-11/msg00571.html
;;
;; Hi Richard,
;;
;; no, variables are not interpolated into quoted lists, any list preceded by
;; "'" is quoted.
;;
;; If you can guarantee that the value of the variables is defined at the time
;; the
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
;; Note that this works only once, so if you later change the value, this list
;; will not be changed.
::
;; If you wanted dynamic behavior, then we would have to patch org-publish.el.
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
         :components ("blog-orgfiles" "blog-images" "blog-other"))))

;; Python/Cython                                                     <<python>>
;; =============

;; (when window-system
;;   ;; python-mode settings
;;   (setq
;;    python-shell-interpreter "ipython"
;;    python-shell-interpreter-args ""
;;    python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;    python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;    python-shell-completion-setup-code
;;    "from IPython.core.completerlib import module_completion"
;;    python-shell-completion-module-string-code
;;    "';'.join(module_completion('''%s'''))\n"
;;    python-shell-completion-string-code
;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;;   ;; cython-mode
;;   (require 'cython-mode)
;;   ;; pydoc-info is an Emacs package for searching and browsing the new Python
;;   ;;  documentation in the Info browser.
;;   ;(require 'pydoc-info)
;; )


(autoload 'python-mode "python-mode" "Python" t)
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'python-mode-hook 'whitespace-mode)

;; Jedi -- Python auto-completion for Emacs
;; ----------------------------------------

;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)
;; (add-hook 'python-mode-hook 'jedi:setup)


;; TODO Clean-up below

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

(defvar sb-path-to-info "/Users/Shared/info/")
(when (boundp 'sb-path-to-info)
 (add-to-list 'Info-directory-list sb-path-to-info))
