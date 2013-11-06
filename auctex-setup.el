;; +-------------------------+
;; | Configuration of AUCTeX |
;; +-------------------------+

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
