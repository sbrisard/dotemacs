;; Contents
;; ========

;; NOTA: if `init-window-system.el` has been loaded, follow the links below with
;;       C-c o. Otherwise, use M-x org-open-at-point-global.

;; [[vars & funs][Variables and functions defined in this file]]
;; [[misc][Miscellaneous]]
;; [[trailing][Trailing whitespaces]]
;; [[elpa][Emacs Lisp Package Archive (ELPA)]]
;; [[window][Additional customizations in window-system mode]]

;; Variables and functions defined in this file                 <<vars & funs>>
;; ============================================

;; Predicates to check whether emacs is run from Windows, Linux or MacOS.
(defvar windowsp (string-equal "windows-nt" (symbol-name system-type)))
(defvar darwinp (string-equal "darwin" (symbol-name system-type)))
(defvar linuxp (string-equal "gnu/linux" (symbol-name system-type)))

(defun set-newline-and-indent ()
  "Bind newline-and-indent to RET. This function can be used as a hook."
  (local-set-key (kbd "RET") 'newline-and-indent))

;; Miscellaneous                                                       <<misc>>
;; =============

(setq-default buffer-file-coding-system 'iso-latin-1-unix)
(setq-default indent-tabs-mode nil)

(setq column-number-mode t)
(setq confirm-nonexistent-file-or-buffer t)
(setq default-directory "~/" )
(setq dnd-open-file-other-window nil)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq ps-paper-type (quote a4))
(setq visible-bell t)

(filesets-init)
(global-hl-line-mode)
(menu-bar-mode)
(scroll-bar-mode -1)
(show-paren-mode)
(tool-bar-mode -1)

(global-unset-key (kbd "<C-next>"))
(global-unset-key (kbd "<C-prior>"))

;; Under Mac OS X, right-alt must be mapped to Alt Gr.
(when darwinp
  (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'meta))

;; On Windows 7, HOME is set to "C:/Users/brisard/AppData/Roaming/", which
;; allows correct location of ".emacs.d", but leads to incorrect value of "~".
;;(setenv "HOME" "C:/Users/brisard/")

;; Trailing whitespaces                                            <<trailing>>
;; ====================

;; In selected modes, show trailing whitespaces and empty lines at the end of
;; the buffer. This is defined as a mode hook (which requires a function).
(add-hook 'python-mode-hook (lambda() (setq show-trailing-whitespace t)))

;; In all modes, empty lines at the end of the buffer are shown, and trailing
;; white spaces are remoaved when buffer is saved.
(setq-default indicate-empty-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Emacs Lisp Package Archive (ELPA)                                   <<elpa>>
;; =================================

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; Additional customizations in window-system mode                   <<window>>
;; ===============================================

(when window-system (load-file "~/.emacs.d/init-window-system.el"))
