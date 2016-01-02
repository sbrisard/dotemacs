;; -*- coding: utf-8 -*-

;; LaTeX                                                              <<latex>>
;; =====


;; Ob-ipython
;; ==========

(require 'ob-ipython)
(setq ob-ipython-kernel-extra-args (quote ("--profile=ob-ipython")))

;; <<ccmode>>
;; ==========

(setq-default c-basic-offset 4)
