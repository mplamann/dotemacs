(load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-to-list 'completion-ignored-extensions ".hi")

(fset 'ghci-hide-mtl
   [?: ?s ?e ?t ?  ?h ?i ?d ?e ?- ?p ?a ?c ?k ?a ?g ?e ?\M-b ?\M-b ?- ?\C-e ?  ?m ?t ?l return])

;;; Structured-haskell-mode
;(add-to-list 'load-path "~/.emacs.d/lisp/structured-haskell-mode/elisp")
;(require 'shm)
;(add-hook 'haskell-mode-hook 'structured-haskell-mode)

(provide 'haskell)
