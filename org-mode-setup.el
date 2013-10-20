;; +---------------------------+
;; | Configuration of Org-mode |
;; +---------------------------+

;; Wrap lines
(setq org-startup-truncated nil)

;; Don't open org files in folded mode
(setq org-startup-folded nil)

(org-babel-do-load-languages 'org-babel-load-languages '((C . t)
                                                         (python . t)))

;; Fontify code in code blocks
(setq org-src-fontify-natively t)
