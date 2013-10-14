;; +---------------------------------+
;; | Configuration of maxima-mode.el |
;; +---------------------------------+

(autoload 'maxima-mode "maxima" "Major mode for writing Maxima programs" t)
(autoload 'maxima "maxima" "Run Maxima interactively" t)
;(setq load-path (cons "C:/Program Files (x86)/Maxima-5.30.0/share/maxima/5.30.0/emacs" load-path))
;(setq load-path (cons "/opt/local/share/maxima/5.28.0/emacs" load-path))
(setq auto-mode-alist (cons '("\\.ma[cx]" . maxima-mode) auto-mode-alist))

;;(setq maxima-command "C:/Program Files (x86)/Maxima-5.30.0/bin/maxima.bat")

