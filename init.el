;; .emacs

; To set this up,
; mkdir -p ~/.cache/emacs{,backups,auto-save-list}

;; set up load path
(add-to-list 'load-path "~/.emacs.d")

;; prefer UTF-8 coding system
(setq prefer-coding-system 'utf-8)

;; always end a file with a newline
(setq require-final-newline t)

;; default to unified diffs
(setq diff-switches "-u")

;; disable tabs by default, use linux style and 4 indent width
;; the following line doesn't seem to be needed with emacs 24 and cc-guess
(setq-default indent-tabs-mode nil)
(setq c-default-style "linux"
      c-basic-offset 4)
;; not needed with emacs 24 and cc-guess (needed to guess between tabs and spaces)
;; use dtrt to automatically use the correct tab/indenting style
;;(require 'dtrt-indent)
;;(dtrt-indent-mode 1)

;; make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; make all backups in a single directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.cache/emacs/backups"))))

;; record auto-save-list in the cache directory
(setq auto-save-list-file-prefix
      (expand-file-name "~/.cache/emacs/auto-save-list/"))

;; make the font a bit bigger
(set-face-attribute 'default nil :height 110)

;; Highlight the parenthesis at point and its match.
;; http://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)

;; don't show so many messages on startup
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; disable UI elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; remember the previous place in visited buffers
(setq save-place-file "~/.cache/emacs/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package

;; enable column number minor mode
(column-number-mode t)

;; highlight TODO?
;;(font-lock-add-keywords
;;   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
;;          1 font-lock-warning-face t)))

(setq compile-command "make -j 8")

(if (window-system) (set-frame-size (selected-frame) 100 45))

;; http://stackoverflow.com/questions/64360/how-to-copy-text-from-emacs-to-another-application-on-linux
;; not needed for emacs 24
;; (setq x-select-enable-clipboard t)

(setq sentence-end-double-space nil)

(require 'bindings)
(require 'other)
