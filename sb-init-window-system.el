;; -*- coding: utf-8 -*-

;; Ispell
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

;; LaTeX                                                              <<latex>>
;; =====

;; AUCTeX
;; ------

(require 'tex)

(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq-default TeX-master nil)

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

;; Custom function to insert BibTeX reference
;; ------------------------------------------

(defvar sb-path-to-biblio (f-join sb-path-to-local-documents "biblio"))

(defun sb-bibref-path (key)
  (f-join sb-path-to-biblio (s-left 1 key) key (concat key ".bib")))

(defun sb-insert-bibref (key)
  (interactive "sBibTeX key: \n")
  (insert-file-contents (sb-bibref-path (downcase key))))

;; ob-ipython
;; ==========

(require 'ob-ipython)
(setq ob-ipython-kernel-extra-args (quote ("--profile=ob-ipython")))

;; <<ccmode>>
;; ==========

(setq-default c-basic-offset 4)
