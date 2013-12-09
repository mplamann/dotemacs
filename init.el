;; Packaging
(require 'cl)
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("elpa" . "http://elpa.gnu.org/packages/") t)
(package-initialize)
(defvar prelude-packages
  '(haskell-mode python quack paredit workgroups crosshairs hl-line+ col-highlight slime auctex cmake-mode rinari python-mode zenburn-theme company auto-complete eimp web-mode ruby-mode autopair yasnippet-bundle))
(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))
(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done/")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(when (string/ends-with system-name "andrew.cmu.edu")
  (load-file "~/.emacs.d/c0.el"))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Requires
(require 'git)
(require 'crosshairs)
(require 'wc-mode)
(require 'cmake-mode)
(require 'mingus)
(require 'yasnippet-bundle)

;; General emacs settings

(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq large-file-warning-threshold 200000000) ; Warn for files > 200 MB
(setq doc-view-continuous t)

(iswitchb-mode 1) ;; improved buffer switching
(menu-bar-mode 0)
(desktop-save-mode 1) ;; persistent sessions
;(undo-tree-mode 0)
;(global-undo-tree-mode 0)

(setq scroll-step            1
      scroll-conservatively  10000)
(setq c-default-style "gnu"
      c-basic-offset 2)
(c-set-offset 'substatement-open 0)

(subword-mode 1)
(load-theme 'zenburn t)
(setq-default indent-tabs-mode nil) ; Don't use tabs for indentation

(load "~/.emacs.d/elpa/python-mode-6.0.10/python-mode.el")

(add-to-list 'auto-mode-alist '("\.c0$" . c-mode))
(add-to-list 'auto-mode-alist '("\.ino$" . c-mode))
(autopair-global-mode)

;; Haskell mode
(load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook (lambda () (paredit-mode +1)))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(require 'haskell-ac) ;; Improved autocompletion for Haskell

(add-hook 'haskell-mode-hook (setq ac-sources
                                   (append '(ac-source-yasnippet
                                             ac-source-abbrev
                                             ac-source-words-in-buffer
                                             my/ac-source-haskell)
                                           ac-sources)))

(add-to-list 'completion-ignored-extensions ".hi")

;; Web development
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.qml\\'" . javascript-mode))

;; SICP Support (Racket)
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

;; SLIME for Common Lisp
(require 'slime-autoloads)
(if (eq system-type 'windows-nt)
    (setq inferior-lisp-program "sbcl.exe")
  (setq inferior-lisp-program "clisp"))
(add-hook 'lisp-mode-hook
	  (lambda () (local-set-key (kbd "C-c c") 'slime-compile-and-load-file)))
(add-hook 'lisp-mode-hook
	  (lambda () (slime-mode +1)))

;; NOTE! If Slime does not work, giving some cl error, do this:
;; Open up slime.el and change the line at the bottom from
;; lexical-binding: t
;; to
;; lexical-binding: nil


;; Keyboard shortcuts
(global-set-key (kbd "RET") 'newline-and-indent)
(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(add-hook 'feature-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-x h") 'help-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c g") 'gdb-many-windows)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-u") 'undo)
(global-set-key (kbd "C-c l") 'hl-line-mode)
(global-set-key (kbd "C-x C-b") 'iswitchb-buffer)
(global-set-key (kbd "<C-return>") 'dabbrev-expand)

(global-set-key (kbd "C-c b")  'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p")    'windmove-up)
(global-set-key (kbd "C-c n")  'windmove-down)
(setq windmove-wrap-around t)

(fset 'both-prev-page
   "\C-[v\C-xo\C-[v")

(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

(global-set-key (kbd "C-M-p") 'both-prev-page)
(global-set-key (kbd "C-M-v") 'both-next-page)

;; Windows-specific
(setq w32-lwindow-modifier 'meta)
(setq w32-pass-lwindow-to-system nil)
;; Mac Compatibility (Terminal is a pain)
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)
(add-hook 'paredit-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-[ 5 d") 'paredit-forward-barf-sexp)
	    (local-set-key (kbd "M-[ 5 c") 'paredit-forward-slurp-sexp)))

(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash")
	(explicit-bash-args '("--login" "-i")))
    (call-interactively 'shell)))

;; Workgroups
(require 'workgroups)
(setq wg-prefix-key (kbd "C-z"))
(workgroups-mode 1)
(wg-load "~/.emacs.d/workgroups")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((emacs-lisp-docstring-fill-column . 75)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
