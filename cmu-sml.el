;; these emacs settings for SML mode are added by a script written for
;; 15-150 in Sp 2012.

;; they are largely portable, except that they refer to a copy of the
;; sml-mode elisp files in the 150 AFS volume. so if you ever want to use
;; this off of AFS, you should change where that points to and install them
;; locally.

;; this points to AFS
(add-to-list 'load-path
             "~/.emacs.d/lisp/sml-mode")
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)

;; this points to where SML happens to live on unix.andrew.cmu.edu
(setq sml-program-name "/usr/local/bin/sml")

(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))

(defun my-sml-mode-hook () "Local defaults for SML mode"
  (setq sml-indent-level 2)             ; conserve on horizontal space
  (setq words-include-escape t)         ; \ loses word break status
  (setq indent-tabs-mode nil))          ; never ever indent with tabs
(add-hook 'sml-mode-hook 'my-sml-mode-hook)

;; General non-SML-specific settings that are appropriate follow - feel
;; free to comment these out if you don't like them.

;; Removes trailing whitespace from your lines before saving
;; files. Trailing whitespace is the devil.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Makes sure your files end in a newline before saving them. Terminating
;; newlines are a Good Idea.
(setq require-final-newline t)

(provide 'cmu-sml)
