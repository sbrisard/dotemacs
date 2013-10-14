;; +---------------------------------+
;; | Configuration of python-mode.el |
;; +---------------------------------+

(require 'python-mode)

;; Unless other libraries depend on python.el, unloading 'python is
;; recommended, as it seems to destroy python-mode user defined abbreviations.
(when (featurep 'python) (unload-feature 'python t))
