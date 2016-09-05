;; -*- coding: utf-8 -*-

;; Emacs Lisp Package Archive (ELPA)
;; =================================

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

(require 'f)

;; Customizable variables
;; ======================

(defgroup sb nil "My customization group" :tag "SB")
(defcustom sb-path-to-local-documents "~/Documents"
  "TODO Documentation"
  :type 'string :group 'sb :tag "Path to local documents")

;; Variables and functions defined in this file
;; ============================================

;; Predicates to check whether emacs is run from Windows, Linux or MacOS.
(defvar windowsp (string-equal "windows-nt" (symbol-name system-type)))
(defvar darwinp (string-equal "darwin" (symbol-name system-type)))
(defvar linuxp (string-equal "gnu/linux" (symbol-name system-type)))

;; Miscellaneous
;; =============

(prefer-coding-system 'utf-8)

(setq column-number-mode t)
(setq confirm-nonexistent-file-or-buffer t)
(setq default-process-coding-system 'utf-8)
(setq dnd-open-file-other-window nil)
(setq find-file-run-dired nil)
(setq inhibit-startup-screen t)
(setq initial-frame-alist '((top . 0) (left . 0)))
(setq make-backup-files nil)
(setq ps-paper-type (quote a4))
(setq safe-local-variable-values (quote ((buffer-auto-save-file-name)
                                         (org-confirm-babel-evaluate))))
(setq visible-bell t)

;;(setq-default buffer-file-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)

(global-auto-revert-mode)
(global-hl-line-mode)
(menu-bar-mode)
;(scroll-bar-mode -1)
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

(when window-system
  (split-window-right))


(org-babel-load-file (concat user-emacs-directory "sb-config.org"))
