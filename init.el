(require 'cl)
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(defvar prelude-packages
  '(haskell-mode python quack paredit workgroups crosshairs hl-line+ col-highlight))
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

(require 'ahk-mode)

;(setq asm-comment-char ?\#) ;; This is MIPS assembly, uses # for comments
(setq auto-mode-alist (cons '("\\.asmnes$" . asm-mode) auto-mode-alist))

(setq inhibit-startup-message t)
(setq make-backup-files nil)

(setq text-mode-hook
      '(lambda ()
	 (flyspell-mode 1)))

(iswitchb-mode 1) ;; improved buffer switching
(menu-bar-mode 0)
(desktop-save-mode 1) ;; persistent sessions
(global-hl-line-mode 1) ;; highlight current line
(require 'crosshairs)

;; Haskell mode
(load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

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
(add-hook 'c-mode-common-hook
	  (lambda () (subword-mode 1)))
;; NOTE TO SELF: C-c C-a, bound to geiser-mode-switch-to-repl-and-enter, runs a file easily


;; Keyboard shortcuts
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-x h") 'help-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

(global-set-key (kbd "C-c b")  'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p")    'windmove-up)
(global-set-key (kbd "C-c n")  'windmove-down)
(setq windmove-wrap-around t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Workgroups
(require 'workgroups)
(setq wg-prefix-key (kbd "C-z"))
(workgroups-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XCode integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First of all, associate file extensions
(setq auto-mode-alist
      (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm$" . objc-mode) auto-mode-alist))
(defun bh-choose-header-mode ()
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (progn
	;; OK, we got a .h file, if a .m file exists we'll assume it's
					; an objective c file. Otherwise, we'll look for a .cpp file.
	(let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
	      (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
	  (if (file-exists-p dot-m-file)
	      (progn
		(objc-mode))
	    (if (file-exists-p dot-cpp-file)
		(c++-mode)
	      )
	    )
	  )
	)
    )
  )
(add-hook 'find-file-hook 'bh-choose-header-mode)
(defun bh-compile ()
  (interactive)
  (let ((df (directory-files "."))
	(has-proj-file nil)
	)
    (while (and df (not has-proj-file))
      (let ((fn (car df)))
	(if (> (length fn) 10)
	    (if (string-equal (substring fn -10) ".xcodeproj")
		(setq has-proj-file t)
	      )
	  )
	)
      (setq df (cdr df))
      )
    (if has-proj-file
	(compile "xcodebuild -configuration Debug")
      (compile "make")
      )
    )
  )
