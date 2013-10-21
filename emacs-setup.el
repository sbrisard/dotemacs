;; In init.el, a variable should be defined
;; sb-path-to-emacs-setup
;; which points to the path to this file.

;; Define a useful predicate to check whether emacs is run from windows or
;; linux. This is useful to share the same init.el file between several systems.
(defvar windowsp (string-equal "windows-nt" (symbol-name system-type)))
(defvar darwinp (string-equal "darwin" (symbol-name system-type)))

;; Define path to emacs-config directory
;;(cond (windowsp (defvar sb-path-to-emacs-setup "/Users/brisard/Documents/emacs.d/"))
;;      (darwinp (defvar sb-path-to-emacs-setup "/Users/sb/Documents/emacs-setup/"))
;;      (t (defvar sb-path-to-emacs-setup "/media/sf_documents-brisard/emacs.d/")))

;; Local directory for emacs extensions.
(defvar path-to-site-lisp (concat sb-path-to-emacs-setup "site-lisp/"))
(add-to-list 'load-path path-to-site-lisp)
(let ((default-directory path-to-site-lisp))
  (normal-top-level-add-subdirs-to-load-path))

;; Under Mac OS X, right-alt must be mapped to Alt Gr.
(when darwinp
  (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'meta))

(load-file (concat sb-path-to-emacs-setup "elpa-setup.el"))

;; On Windows 7, HOME is set to "C:/Users/brisard/AppData/Roaming/", which
;; allows correct location of ".emacs.d", but leads to incorrect value of "~".
;;(setenv "HOME" "C:/Users/brisard/")

;; Set startup directory to home.
(setq default-directory "~/" )

;; Default geometry
(setq default-frame-alist
      '((top . 0) (left . 658)
        (width . 80)))
;;        (foreground-color . "black")
;;        (background-color . "lightyellow")))

(if windowsp
    (add-to-list 'default-frame-alist '(height . 48))
  (add-to-list 'default-frame-alist '(height . 41))  
)

(setq initial-frame-alist '((top . 0) (left . 0)))

;; Disable system beep
(setq visible-bell t)

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
(when (display-graphic-p)
  (if windowsp
      (set-face-font 'default "Consolas-9")
    (set-face-font 'default "Inconsolata-10")))


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

;; Personnal customizations
(global-set-key [(control next)] 'next-buffer)
(global-set-key [(control prior)] 'previous-buffer)
(setq inhibit-splash-screen t)
(tool-bar-mode -1)

;; PROGRAMS THAT ARE INSTALLED UNDER WINDOWS ONLY

(if windowsp
    (progn
      ;; This allows emacs to find the TeXLive executables. Altering the
      ;; exec-path variable apparently does not work!
      (setenv "PATH" (concat "C:/texlive/2013/bin/win32;" (getenv "PATH")))     
      ))

;; Choix du dictionnaire fran�ais pour la v�rification de l'orthographe
(setq ispell-dictionary "francais")
 	
; Une macro pour l'insertion de tags dans un fichier XML
(defun nxml-insert-tag(tag)
  "inserts opening and closing XML tags, with specified tag-name, and places the cursor inbetween"
  (interactive "sTag name: ")
  ; (message "tag to insert %s" tag)
  (insert "<"tag"></"tag">")
  (search-backward "<")
  (indent-according-to-mode)
  )

(add-hook 'nxml-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-t") 'nxml-insert-tag)
             ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex")
 '(TeX-PDF-mode t)
 '(TeX-command "tex")
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list (quote (("SumatraPDF" ("\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n") " %o")))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "SumatraPDF") (output-html "xdg-open"))))
 '(column-number-mode t)
 '(display-buffer-reuse-frames t)
; '(font-use-system-font t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(nxml-attribute-indent 2)
 '(nxml-child-indent 2 t)
 '(ps-paper-type (quote a4))
 '(reftex-insert-label-flags (quote ("s" "st")))
 '(safe-local-variable-values (quote ((eval setenv "TEXINPUTS" "./sty:"))))
 '(scroll-bar-mode nil)
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


;; Emp�che Ispell de v�rifier le contenu de certaines commandes 
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
 
;; Emp�che Ispell de v�rifier le contenu des citation natbib
(eval-after-load "ispell"
  '(let ((list (car ispell-tex-skip-alists)))
     (add-to-list 'list '("\\\\cite[tp]" ispell-tex-arg-end))
     (setcar ispell-tex-skip-alists list))
)

;; Do not insert tabs when indenting code
(setq-default indent-tabs-mode nil)

;; Globally enable highlighting of the line containing point
(global-hl-line-mode)

;; htmlize is required to fontify source in html output of org files
(require 'htmlize)

(load-file (concat sb-path-to-emacs-setup "color-theme-setup.el"))
(load-file (concat sb-path-to-emacs-setup "reftex-setup.el"))
(load-file (concat sb-path-to-emacs-setup "maxima-mode-setup.el"))
(load-file (concat sb-path-to-emacs-setup "org-mode-setup.el"))

;; Python-related settings

(load-file (concat sb-path-to-emacs-setup "python-mode-setup.el"))
(load-file (concat sb-path-to-emacs-setup "cython-mode-setup.el"))
(load-file (concat sb-path-to-emacs-setup "jedi-setup.el"))
