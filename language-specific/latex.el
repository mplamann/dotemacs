(fset 'latex-nextline
   [return ?& ?\\ ?c ?o ?n ?h ?  backspace backspace ?g ?  ?\C-c ?i])

(fset 'latex-code
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([92 99 111 100 101 123 125 left] 0 "%d")) arg)))

(add-hook 'LaTeX-mode-hook (lambda ()
                             (local-set-key (kbd "C-c i") 'latex-listing)
                             (local-set-key (kbd "C-j") 'latex-nextline)))

(fset 'latex-text
   [?& ?\\ ?t ?e ?x ?t ?\{ ?\[ ?\] ?\} ?\\ ?\\ left left left left])

(add-hook 'LaTeX-mode-hook (lambda ()
                             (local-set-key (kbd "C-c t") 'latex-text)))

(defun compile-latex-hook ()
  (shell-command-to-string
   (concat "pdflatex -halt-on-error " (buffer-file-name) " &")))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'compile-latex-hook 't 't)))

(fset 'latex-listing
   [?\\ ?l ?s ?t ?i ?n ?l ?i ?n ?e ?| ?| left])

;;; This is a good idea.
;; (defun type-c () (interactive) (insert "C"))
;; (global-set-key (kbd "C-t") 'type-c)
