;;; -*- lexical-binding: t -*-
(load-file "~/.emacs.d/sb-init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-auto-help nil)
 '(completion-styles '(flex basic partial-completion))
 '(fido-mode t)
 '(fido-vertical-mode nil)
 '(math-preview-margin '(2 . 2))
 '(math-preview-svg-postprocess-functions nil)
 '(math-preview-tex-macros
   '(("ddx" "\\frac{d#2}{d#1}" 2 "t") ("E" . "\\mathcal{E}")
     ("D" . "\\mathrm{d}") ("order" "#2^{(#1)}" 2 "1")
     ("reals" . "\\mathbb{R}")))
 '(org-startup-folded 'showeverything)
 '(org-startup-truncated nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((TeX-master . \../livre_plaques.tex) (buffer-auto-save-file-name)
     (org-confirm-babel-evaluate))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((default :inherit flymake-note))))
