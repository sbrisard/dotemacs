(use-package smartparens
  :init
  (show-paren-mode -1))

(require 'smartparens-config)
(require 'smartparens-latex)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; Deactivate electric mode as it conflicts with AUCTex's
;; This should be investigated
(setq sp-autoinsert-pair nil)

(bind-key "C-M-f" #'sp-forward-sexp)
(bind-key "C-M-b" #'sp-backward-sexp)
(bind-key "C-M-k" #'sp-kill-sexp)
(bind-key "C-M-t" #'sp-transpose-sexp)
(bind-key "C-M-<SPC>" #'sp-mark-sexp)
(bind-key "C-M-n" #'sp-forward-hybrid-sexp)
(bind-key "C-M-p" #'sp-backward-hybrid-sexp)
(bind-key "C-M-u" #'sp-backward-up-sexp)
(bind-key "C-M-d" #'sp-down-sexp)

(defun sb-get-enclosing-sexp-as-string (&optional arg)
  (interactive)
  (let ((enc (sp-get-enclosing-sexp)))
    (buffer-substring (sp-get enc :beg) (sp-get enc :end))))

(defun sb-rewrap-sexp ()
  (interactive)
  (completing-read (sb-get-enclosing-sexp-as-string)
                   (sp--get-pair-list-context 'wrap)))
