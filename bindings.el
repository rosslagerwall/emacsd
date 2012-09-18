;; switch buffer with ` (set this in shell scripts as well)
(global-set-key (kbd "`") 'ido-switch-buffer)

;; find file with F12
(global-set-key (kbd "<f12>") 'ido-find-file)

;; compile with F10
(global-set-key (kbd "<f10>") 'compile)

;; enter is newline & indent for c mode
;;(add-hook 'c-mode-common-hook '(lambda ()
;;      (local-set-key (kbd "RET") 'newline-and-indent)))

;; enter is newline
;;(add-hook 'c-mode-common-hook '(lambda ()
;;      (local-set-key (kbd "C-j") 'newline)))

;; enter is newline & indent for python mode
;;(add-hook 'python-mode-hook '(lambda ()
;;      (local-set-key (kbd "RET") 'newline-and-indent)))

;; enter is newline
;;(add-hook 'python-mode-hook '(lambda ()
;;      (local-set-key (kbd "C-j") 'newline)))

;; move to mark with F9
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "<f9>") 'jump-to-mark)

;; move to previous buffer with F11
(global-set-key (kbd "<f11>") 'switch-to-previous-buffer)
    (defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer)))

;; switch between .h & .cpp with M-RET
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "M-RET") 'ff-find-other-file)))

;; Save with s-s
(global-set-key (kbd "s-s") 'save-buffer)

;; Other window with F8
(global-set-key (kbd "<f8>") 'other-window)

;; toggle indentation level
;; http://stackoverflow.com/questions/530461/emacs-lisp-function-to-toggle-variable-tab-width-between-4-8
(global-set-key (kbd "<f5>") 'tf-toggle-tab-width-setting)
(defun tf-toggle-tab-width-setting ()
    "Toggle setting tab widths between 4 and 8"
    (interactive)
    (setq c-basic-offset (if (= c-basic-offset 8) 4 8))
    (message "set c-basic-offset to %d." c-basic-offset)
    (redraw-display))

;; toggle tabs or spaces
;; http://stackoverflow.com/questions/530461/emacs-lisp-function-to-toggle-variable-tab-width-between-4-8
(global-set-key (kbd "<f6>") 'tf-toggle-tab-mode-setting)
(defun tf-toggle-tab-mode-setting ()
    "Toggle setting tab or spaces"
    (interactive)
    (setq indent-tabs-mode (if (eq indent-tabs-mode t) nil t))
    (message "Indenting using %s." (if (eq indent-tabs-mode t) "tabs" "spaces"))
    (redraw-display))

;; toggle tab indentation level
;; http://stackoverflow.com/questions/530461/emacs-lisp-function-to-toggle-variable-tab-width-between-4-8
(global-set-key (kbd "<f4>") 'tf-toggle-actual-tab-width-setting)
(defun tf-toggle-actual-tab-width-setting ()
    "Toggle setting actual tab widths between 4 and 8"
    (interactive)
    (setq tab-width (if (= tab-width 8) 4 8))
    (message "set tab-width to %d." tab-width)
    (redraw-display))

(provide 'bindings)
