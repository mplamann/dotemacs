(load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-to-list 'completion-ignored-extensions ".hi")

(provide 'haskell)
