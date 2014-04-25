;; +-------------------------+
;; | Configuration of RefTeX |
;; +-------------------------+

(require 'reftex)

;; Turn on RefTeX in emacs latex mode
(add-hook 'latex-mode-hook 'turn-on-reftex)

;; Turn on RefTeX in AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Add the buffer's index to the menu bar in latex mode
(setq reftex-load-hook (quote (imenu-add-menubar-index)))
(setq reftex-mode-hook (quote (imenu-add-menubar-index)))

;; Turn on the RefTeX-AUCTeX interface. When on, both packages interact
;; closely.
(setq reftex-plug-into-AUCTeX t)

(setq reftex-insert-label-flags (quote ("s" "st")))
