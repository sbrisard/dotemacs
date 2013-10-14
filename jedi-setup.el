;; +--------------------------------------------------------------+
;; | Configuration of Jedi.el -- Python auto-completion for Emacs |
;; +--------------------------------------------------------------+

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)
