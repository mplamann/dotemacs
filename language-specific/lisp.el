(load-file "~/.emacs.d/lisp/geiser/elisp/geiser.el")
(require 'quack)
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'quack-mode-hook            (lambda () (paredit-mode +1)))
(add-hook 'geiser-repl-mode-hook      (lambda () (paredit-mode +1)))
(setq scheme-program-name "racket")

(provide 'lisp-config)
