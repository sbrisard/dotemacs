;; +------------------------+
;; | Customization of fonts |
;; +------------------------+

(cond (windowsp (defvar sb-monospaced-font "Consolas-8"))
      (darwinp (defvar sb-monospaced-font "Inconsolata-10"))
      (linuxp (defvar sb-monospaced-font "Inconsolata-8")))

(defvar sb-variable-pitch-font sb-monospaced-font)

(when (display-graphic-p) (set-face-font 'default sb-monospaced-font))

;; Do not use variable-pitch fonts
(set-face-attribute 'variable-pitch nil :font sb-monospaced-font)
