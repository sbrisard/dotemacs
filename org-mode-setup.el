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

;; org-publish configuration
(defvar sb-path-to-blog-base-directory (concat sb-path-to-blog "org"))
(defvar sb-path-to-blog-publishing-directory (concat sb-path-to-blog "html"))

;; From http://lists.gnu.org/archive/html/emacs-orgmode/2008-11/msg00571.html
;;
;; Hi Richard,
;;
;; no, variables are not interpolated into quoted lists,
;; any list preceded by "'" is quoted.
;;
;; If you can guarantee that the value of the variables is define at the time
;; the
;;
;;   (setq org-publish-projects-alist ...
;;
;; is executed, then you can use backquote syntax: Quote the main list with the
;; backquote, and then preceed any variable inside you would like to have
;;evaluated with a comma so
;;
;; (setq org-publish-projects-alist
;;        `( .............
;;            ,rgr-souerce
;;            ....))
;;
;; Note that this works only once, so if you later change the value, this
;; list will not be changed.
::
;; If you wanted dynamic behavior, then we would have to patch org- publish.el.
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

;; This allows proper handling of links to "dedicated targets" accross multiple
;; files.
;; Let's say file1.og contains
;;
;; #+BEGIN_EXAMPLE
;;    <<target>> This is a dedicated target.
;; #+END_EXAMPLE
;;
;; And file2.org links to this dedicated target
;;
;; #+BEGIN_EXAMPLE
;;     [[file:./file1.org::target][link]]
;; #+END_EXAMPLE
;;
;; If the following variable is not set, then the generated link will point to
;; ./file1.html, instead of ./file1.html#target
(setq org-link-search-must-match-exact-headline nil)
