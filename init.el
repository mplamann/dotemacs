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
    elscreen
    projectile
    smex
    idris-mode
    rust-mode
    evil
    evil-numbers
    evil-matchit
    evil-surround
    key-chord
    emacs-eclim
    dtrt-indent
    helm
    helm-hoogle
    sml-mode
    guide-key
    yasnippet
    ghc

    outorg
    outshine
    navi-mode
    ))
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
;; (add-to-list 'load-path "~/.emacs.d/")

;; Requires
(require 'git)
(require 'wc-mode)
(require 'cmu-sml)
(require 'toggle-case)
(require 'rtf-mode)
(require 'helm-config)

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

; (iswitchb-mode 1) ;; improved buffer switching
(menu-bar-mode 0)
;(desktop-save-mode 1) ;; persistent sessions
(tool-bar-mode 0)
(scroll-bar-mode -1)
(smex-initialize)
(column-number-mode 1)
(global-undo-tree-mode)
(dtrt-indent-mode 1)
(helm-mode 1)
;; (evil-mode 1)
;; (global-evil-matchit-mode 1)
(key-chord-mode 1)

(setq scroll-step            1
      scroll-conservatively  10000)

(projectile-global-mode)

;; Just helpful
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; Best theme I've found so far
(load-theme 'zenburn t)

(defun load-config-file (file)
  (load-file (concat "~/.emacs.d/language-specific/" file ".el")))

(loop for language in '("c"
                        "python"
                        "haskell-lang-specific"
                        "web"
                        "lisp"
                        "latex"
                        "idris"
                        "java")
      do (load-config-file language))

(add-to-list 'auto-mode-alist '("\\.qml\\'" . javascript-mode))

;; Keyboard shortcuts

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "C-h") 'delete-backward-char)
(define-key my-keys-minor-mode-map (kbd "M-h") 'backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "C-c c") 'compile)
(define-key my-keys-minor-mode-map (kbd "C-x h") 'help-command)
(define-key my-keys-minor-mode-map (kbd "C-x C-m") 'execute-extended-command)
; (define-key my-keys-minor-mode-map (kbd "C-c C-m") 'execute-extended-command)
(define-key my-keys-minor-mode-map (kbd "C-c o") 'ff-find-other-file)
(define-key my-keys-minor-mode-map (kbd "C-c r") 'revert-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c l") 'hl-line-mode)
(define-key my-keys-minor-mode-map (kbd "C-x C-b") 'iswitchb-buffer)
(define-key my-keys-minor-mode-map (kbd "<C-return>") 'dabbrev-expand)
(define-key my-keys-minor-mode-map (kbd "C-.") 'toggle-case)
(define-key my-keys-minor-mode-map (kbd "C-+") 'evil-numbers/inc-at-pt)
(define-key my-keys-minor-mode-map (kbd "<C-kp-add>") 'evil-numbers/inc-at-pt)
(define-key my-keys-minor-mode-map (kbd "C--") 'evil-numbers/dec-at-pt)
(define-key my-keys-minor-mode-map (kbd "<C-kp-subtract>") 'evil-numbers/dec-at-pt)
(define-key my-keys-minor-mode-map (kbd "C-c h") 'helm-hoogle)

(define-key my-keys-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key my-keys-minor-mode-map (kbd "M-X") 'smex-major-mode-commands)

(define-key my-keys-minor-mode-map (kbd "C-c o") 'other-window)
(define-key my-keys-minor-mode-map (kbd "C-x C-o") 'other-window)
(define-key my-keys-minor-mode-map (kbd "C-c C-o") 'other-window)
(define-key my-keys-minor-mode-map (kbd "M-P") 'scroll-down-line)
(define-key my-keys-minor-mode-map (kbd "M-N") 'scroll-up-line)

(setq key-chord-two-keys-delay .025
      key-chord-one-key-delay .020)

(key-chord-define my-keys-minor-mode-map "lj" 'windmove-left)
(key-chord-define my-keys-minor-mode-map "lk" 'windmove-right)
(key-chord-define my-keys-minor-mode-map "ln" 'windmove-down)
(key-chord-define my-keys-minor-mode-map "lm" 'windmove-up)
(key-chord-define my-keys-minor-mode-map "ax" 'switch-to-buffer)


;; (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
;; (define-key evil-insert-state-map "\C-y" nil)
;; (define-key evil-insert-state-map "\C-n" nil)
;; (define-key evil-insert-state-map "\C-e" nil)
;; (define-key evil-insert-state-map (kbd "C-e") nil)
;; (define-key evil-insert-state-map "\C-p" nil)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(my-keys-minor-mode 1)

(global-set-key (kbd "RET") 'newline-and-indent)
(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(add-hook 'feature-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; Mac Compatibility (Terminal is a pain)
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)
(add-hook 'paredit-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-[ 5 d") 'paredit-forward-barf-sexp)
	    (local-set-key (kbd "M-[ 5 c") 'paredit-forward-slurp-sexp)))

;; Windows stuff
(defun android-start ()
  (interactive)
  (when (eq system-type 'windows-nt)
    (require 'cygwin-mount)
    (setq cygwin-mount-cygwin-bin-directory "C:/cygwin64/bin")
    (cygwin-mount-activate)
    (add-hook 'comint-output-filter-functions
              'shell-strip-ctrl-m nil t)
    (add-hook 'comint-output-filter-functions
              'comint-watch-for-password-prompt nil t)
    (setq explicit-shell-file-name "C:/cygwin64/bin/bash.exe")
    (setq shell-file-name explicit-shell-file-name))

  (when (eq system-type 'windows-nt)
    (progn (setq android-ndk-root-path "C:/Users/mitchell/AppData/Local/Android/android-ndk-r9d")
           (setq android-sdk-root-path "C:/Users/mitchell/AppData/Local/Android/android-sdk"))
    (add-to-list 'load-path "C:/Users/mitchell/AppData/Local/Android/android-emacs-toolkit")
    (require 'androidmk-mode)
    (add-hook 'androidmk-mode-hook
              (lambda ()
                (progn (local-set-key [M-f5] 'androidndk-build)
                       (local-set-key [M-S-f5] 'androidndk-rebuild)
                       (local-set-key [C-f5] 'androidsdk-build)
                       (local-set-key [C-S-f5] 'androidsdk-rebuild)))))
  (setq android-default-package "com.infreefall"))

;; outshine
(defvar outline-minor-mode-prefix "\C-z")
(require 'outshine)
(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'python-mode-hook 'outline-minor-mode)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c p" "C-x h"))
(guide-key-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
