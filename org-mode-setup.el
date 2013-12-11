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

;; Visit files in same window
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame) (vm-imap . vm-visit-imap-folder-other-frame) (gnus . org-gnus-no-new-news) (file . find-file) (wl . wl-other-frame))))

;; Use CSS to htmlize source blocks
(setq org-html-htmlize-output-type 'css)
