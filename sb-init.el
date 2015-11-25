;; -*- coding: utf-8 -*-

;; Emacs Lisp Package Archive (ELPA)
;; =================================

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)

(require 'f)

;; Customizable variables
;; ======================

(defgroup sb nil "My customization group" :tag "SB")
(defcustom sb-path-to-cloud "~/Google Drive"
  "TODO Documentation"
  :type 'string :group 'sb :tag "Path to cloud")
(defcustom sb-path-to-local-documents "~/Documents"
  "TODO Documentation"
  :type 'string :group 'sb :tag "Path to local documents")
(defcustom sb-path-to-maxima-mode ""
  "TODO Documentation"
  :type 'string :group 'sb :tag "Path to maxima-mode files")

;; Variables and functions defined in this file
;; ============================================

;; Predicates to check whether emacs is run from Windows, Linux or MacOS.
(defvar windowsp (string-equal "windows-nt" (symbol-name system-type)))
(defvar darwinp (string-equal "darwin" (symbol-name system-type)))
(defvar linuxp (string-equal "gnu/linux" (symbol-name system-type)))

(defun set-newline-and-indent ()
  "Bind newline-and-indent to RET. This function can be used as a hook."
  (local-set-key (kbd "RET") 'newline-and-indent))

;; Miscellaneous
;; =============

(prefer-coding-system 'utf-8)
(setq default-process-coding-system 'utf-8)
;;(setq-default buffer-file-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)

(setq column-number-mode t)
(setq confirm-nonexistent-file-or-buffer t)
(setq default-directory "~/" )
(setq dnd-open-file-other-window nil)
(setq find-file-run-dired nil)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq ps-paper-type (quote a4))
(setq safe-local-variable-values (quote ((buffer-auto-save-file-name)
                                         (org-confirm-babel-evaluate))))
(setq visible-bell t)

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

;; Trailing whitespaces
;; ====================

;; In selected modes, show trailing whitespaces and empty lines at the end of
;; the buffer. This is defined as a mode hook (which requires a function).
(add-hook 'python-mode-hook (lambda() (setq show-trailing-whitespace t)))

;; In all modes, empty lines at the end of the buffer are shown, and trailing
;; white spaces are remoaved when buffer is saved.
(setq-default indicate-empty-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Easy PG (GnuPG interface for Emacs)
;; ===================================

(require 'epa-file)
(epa-file-enable)

;; Whitespace mode (minor mode to visualize blanks)
;; ================================================

;; Here are the UTF-8 values used below
;;   - U+0009 TAB
;;   - U+000A LINE FEED
;;   - U+0020 SPACE
;;   - U+0024 DOLLAR SIGN             $
;;   - U+002E FULL STOP               .
;;   - U+003E GREATER THAN SIGN       >
;;   - U+00B6 PILCROW SIGN            ¶
;;   - U+00B7 MIDDLE DOT              ·
;;   - U+2192 RIGHTWARDS ARROW        →
;;   - U+21E5 RIGHTWARDS ARROW TO BAR ⇥
;;   - U+23CE RETURN SYMBOL          ⏎

(require 'whitespace)

(setq whitespace-line-column 80)

(setq whitespace-display-mappings
      '((space-mark #x0020  [#x00B7] [#x002E])
        (newline-mark #x00A [#x00B6 #x000A] [#x0024 #x000A])
        (tab-mark #x009 [#x2192 #x009] [#x003E #x009])
        ))

(setq whitespace-style (quote (face lines-tail)))

;; Additional customizations in window-system mode
;; ===============================================

(when window-system (load-file "~/.emacs.d/sb-init-window-system.el"))

;; Blog-specific customizations
;; ============================

(defun sb-load-file-when-exists (filename)
  (when (file-exists-p filename) (load-file filename)))
(sb-load-file-when-exists (f-join sb-path-to-local-documents "blog/sb-blog.el"))
