(require 'eclim)
(require 'eclimd)
(global-eclim-mode)

(defun eclim-package-and-main ()
  (let ((package-name (eclim--java-current-package))
        (class-name   "Main"))
    (if package-name (concat package-name "." class-name)
      class-name)))

(defun eclim-run-main ()
  "Run the Main class."
  (interactive)
  (if (not (string= major-mode "java-mode"))
      (message "Sorry cannot run current buffer.")
    (compile (concat eclim-executable " -command java -p "  eclim--project-name
                     " -c " (eclim-package-and-main)))))

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(eval-after-load 'java-mode '(progn
  (define-key java-mode-map (kbd "C-c c") 'eclim-run-main)))

(global-set-key (kbd "<f8>") 'eclim-run-main)

(provide 'java-config)
