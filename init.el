;(setq asm-comment-char ?\#) ;; This is MIPS assembly, uses # for comments
(setq auto-mode-alist (cons '("\\.asmnes$" . asm-mode) auto-mode-alist))

(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)