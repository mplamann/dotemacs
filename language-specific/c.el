(setq c-default-style "gnu"
      c-basic-offset 2)
(c-set-offset 'substatement-open 0)

(add-to-list 'auto-mode-alist '("\.c0$" . c-mode))
(add-to-list 'auto-mode-alist '("\.ino$" . c-mode))

(provide 'c-config)
