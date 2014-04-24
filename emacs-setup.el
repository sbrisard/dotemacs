
;; Contents
;; ========
;;
;; If the present file has been loaded, follow the links below with C-c o.
;; Otherwise, use M-x org-open-at-point-global.
;;
;; [[vars][Variables defined in this file]]
;; [[funs][Functions defined in this file]]
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

;; Functions defined in this file                                      <<funs>>
;; ==============================

(defun set-newline-and-indent ()
  "Bind newline-and-indent to RET. This function can be used as a hook."
  (local-set-key (kbd "RET") 'newline-and-indent))

;; Local directory for emacs extensions.
(defvar path-to-site-lisp (concat sb-path-to-emacs-setup "site-lisp/"))
(add-to-list 'load-path path-to-site-lisp)
(let ((default-directory path-to-site-lisp))
  (normal-top-level-add-subdirs-to-load-path))

;; Under Mac OS X, right-alt must be mapped to Alt Gr.
(when darwinp
  (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'meta))

;; On Windows 7, HOME is set to "C:/Users/brisard/AppData/Roaming/", which
;; allows correct location of ".emacs.d", but leads to incorrect value of "~".
;;(setenv "HOME" "C:/Users/brisard/")

;; Set startup directory to home.
(setq default-directory "~/" )

;; Platform specific variables

;; Default geometry
(setq default-frame-alist `((top . 0)
                            (left . ,sb-default-frame-left)
                            (width . ,sb-default-frame-width)
                            (height . ,sb-default-frame-height)))

(setq initial-frame-alist '((top . 0) (left . 0)))

;; Disable system beep
(setq visible-bell t)

;; Disable menu bar
(menu-bar-mode -1)

(setq column-number-mode t)

(set-scroll-bar-mode nil)

 (setq make-backup-files nil)

;; Trailing whitespaces
;; --------------------
;;
;; In selected modes, show trailing white spaces and empty lines at the end of
;; the buffer.This is defined as a mode hook (which requires a function).
(add-hook 'python-mode-hook (lambda() (setq show-trailing-whitespace t)))

;; In all modes, empty lines at the end of the buffer are shown, and trailing
;; white spaces are remoaved when buffer is saved.
(setq-default indicate-empty-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Default font. Under Windows, to get the right font name, follow this
;; procedure
;;   1. switch to the "*scratch*" buffer
;;   2. type (w32-select-font) followed by C-j (or M-x eval-print-last-sexp)
;;   3. select the desired font from the menu dialog that ensues
;;   4. copy the string that is displayed (something like:
;;      "-outline-Lucida Sans Typewriter-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1")
;;      and paste it into the .emacs.d/init.el file. The line to add is
;;          (set-face-font 'default "fontname")
;;      where fontname is the copied string.

;; Under Linux, starting emacs from the desktop environment results in the
;; PATH variable being different from what it would be if started from a shell.

;; (let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
;;   (setenv "PATH" path)
;;   (setq exec-path
;;         (append
;;          (split-string-and-unquote path ":")
;;          exec-path)))

;; Select default encoding for all new buffers
(setq buffer-file-coding-system 'iso-latin-1-unix)

(setq inhibit-startup-screen t)

;; PROGRAMS THAT ARE INSTALLED UNDER WINDOWS ONLY

(if windowsp
    (progn
      ;; This allows emacs to find the TeXLive executables. Altering the
      ;; exec-path variable apparently does not work!
      (setenv "PATH" (concat "C:/texlive/2013/bin/win32;" (getenv "PATH")))
      ))

;; Choix du dictionnaire français pour la vérification de l'orthographe
(setq ispell-dictionary "francais")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(font-use-system-font t)
 '(ps-paper-type (quote a4))
 '(reftex-insert-label-flags (quote ("s" "st")))
 '(safe-local-variable-values (quote ((eval setenv "TEXINPUTS" "./sty:"))))
 '(show-paren-mode t))

;; Remove completion buffer when done
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Empèche Ispell de vérifier le contenu de certaines commandes
(setq ispell-tex-skip-alists
      (list
       (append (car ispell-tex-skip-alists)
               '(("\\\\cite"            ispell-tex-arg-end)
                 ("\\\\nocite"          ispell-tex-arg-end)
                 ("\\\\includegraphics" ispell-tex-arg-end)
                 ("\\\\author"          ispell-tex-arg-end)
                 ("\\\\ref"             ispell-tex-arg-end)
                 ("\\\\eqref"             ispell-tex-arg-end)
                 ("\\\\label"           ispell-tex-arg-end)
                 ))
       (cadr ispell-tex-skip-alists)))

;; Empèche Ispell de vérifier le contenu des citation natbib
(eval-after-load "ispell"
  '(let ((list (car ispell-tex-skip-alists)))
     (add-to-list 'list '("\\\\cite[tp]" ispell-tex-arg-end))
     (setcar ispell-tex-skip-alists list))
)

;; Do not insert tabs when indenting code
(setq-default indent-tabs-mode nil)

;; Systematically ask for confirmation when a nonexistent filename is entered
(setq confirm-nonexistent-file-or-buffer t)

;; Globally enable highlighting of the line containing point
(global-hl-line-mode)

;; Do not create new window for dropped files.
(setq dnd-open-file-other-window nil)

;; Adds a 'Filesets' menu to the menu bar.
(filesets-init)

;; Load these configurations in window mode only, in order to speed up startup
;; -nw mode.
(when window-system
  (tool-bar-mode -1)
  (load-file (concat sb-path-to-emacs-setup "elpa-setup.el"))
  (load-file (concat sb-path-to-emacs-setup "fonts-setup.el"))
  (load-file (concat sb-path-to-emacs-setup "color-theme-setup.el"))
  (load-file (concat sb-path-to-emacs-setup "auctex-setup.el"))
  (load-file (concat sb-path-to-emacs-setup "reftex-setup.el"))
  (load-file (concat sb-path-to-emacs-setup "maxima-mode-setup.el"))
  ;;(load-file (concat sb-path-to-emacs-setup "org-mode-setup.el"))
)

;;
;; Location of additional info manuals
;; -----------------------------------
;;

(when (boundp 'sb-path-to-info)
 (add-to-list 'Info-directory-list sb-path-to-info))

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
