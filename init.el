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
  '(haskell-mode
    python
    paredit
    crosshairs
    hl-line+
    col-highlight
    slime
    auctex
    cmake-mode
    rinari
    python-mode
    zenburn-theme
    eimp
    web-mode
    ruby-mode
    yasnippet-bundle
    elscreen))
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

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/")

;; Requires
(require 'git)
(require 'wc-mode)
(require 'yasnippet-bundle)
(require 'cmu-sml)

;; General emacs settings

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq make-backup-files nil)
(setq large-file-warning-threshold 200000000) ; Warn for files > 200 MB
(setq doc-view-continuous t)
(subword-mode 1)
(setq-default indent-tabs-mode nil) ; Don't use tabs for indentation
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(iswitchb-mode 1) ;; improved buffer switching
(menu-bar-mode 0)
;(desktop-save-mode 1) ;; persistent sessions
(tool-bar-mode 0)
(scroll-bar-mode -1)

(setq scroll-step            1
      scroll-conservatively  10000)

;; Best theme I've found so far
(load-theme 'zenburn t)

(defun load-config-file (file)
  (load-file (concat "~/.emacs.d/language-specific/" file ".el")))

(loop for language in '("c" 
                        "python"
                        "haskell"
                        "web"
                        "lisp"
                        "latex")
      do (load-config-file language))

(add-to-list 'auto-mode-alist '("\\.qml\\'" . javascript-mode))

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
(global-set-key (kbd "C-c s") 'eshell)

;; I've messed up C-x o enough times.
(global-set-key (kbd "C-c o") 'other-window)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-c C-o") 'other-window)

(global-set-key (kbd "C-c b")  'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p")    'windmove-up)
(global-set-key (kbd "C-c n")  'windmove-down)
(setq windmove-wrap-around t)

(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; Mac Compatibility (Terminal is a pain)
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)
(add-hook 'paredit-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-[ 5 d") 'paredit-forward-barf-sexp)
	    (local-set-key (kbd "M-[ 5 c") 'paredit-forward-slurp-sexp)))

