;; marker at 80 chars
;;(require 'column-marker)
;;(column-marker-1 80)
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq-default fill-column 80)
(setq fci-rule-column 80)

(require 'visible-mark)
(add-hook 'after-change-major-mode-hook 'visible-mark-mode)

;; ido makes competing buffers and finding files easier
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
(require 'ido) 
(setq ido-save-directory-list-file "~/.cache/emacs/ido.last")
;; more flexible matching
(setq  ido-enable-flex-matching t)
(setq ido-file-extensions-order '(".c" ".cpp" ".py" ".java"))
(setq 
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
    "^\*compilation" "^\*GTAGS" "^session\.*" "^\*"))
(setq  ido-case-fold  t)                 ; be case-insensitive
(setq ido-max-work-file-list      50)   ; remember many
(setq ido-enable-last-directory-history t) ; remember last used dirs
(setq  ido-max-work-directory-list 50)   ; should be enough
(setq ido-work-directory-list '("~/" "~/src"))
(ido-mode 'both) ;; for buffers and files

;;  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
;; ido-use-url-at-point nil         ; don't use url at point (annoying)


(require 'recentf)
(setq recentf-save-file (recentf-expand-file-name "~/.cache/emacs/recentf"))
 
;; bind recent file open to F7
(global-set-key (kbd "<f7>") 'ido-recentf-open)
 
;; enable recent files mode.
(recentf-mode t)
 
; 50 files ought to be enough.
(setq recentf-max-saved-items 50)
 
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

; https://raw.github.com/rafl/git-commit-mode/master/git-commit.el
; When committing, C-c C-c saves and exits.
; C-x k y y, C-c C-x or C-c C-c with an empty message discards the buffer and
; does not commit anything
(require 'git-commit)
; don't remember where you are in the git commit buffer
(add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))
; (add-hook 'git-commit-mode-hook 'turn-on-flyspell)

(add-hook 'git-commit-mode-hook
  (lambda ()
    (setq fci-rule-column 72)
    (setq fill-column 72)
))

(defun kill-buffer-unconditionally ()
  (interactive nil)
  (set-buffer-modified-p nil)
  (make-local-variable 'kill-buffer-query-functions)
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
  (kill-buffer))

(add-hook 'git-commit-mode-hook
  (lambda() 
    (local-set-key  (kbd "C-c C-x") 'kill-buffer-unconditionally)))


;; Mutt support.
;; Put emacs in mail mode when run from mutt.
(setq auto-mode-alist
  (append
    '(("/tmp/mutt.*" . mail-mode))
      auto-mode-alist))

; Set the fill column to 72 when in mail mode.
(add-hook 'mail-mode-hook
  (lambda ()
    (setq fci-rule-column 72)
    (setq fill-column 72)
))

; Turn on auto-fill when sending mail.
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

; Force emacs to think that the buffer is modified.
; This allows one to send an empty email without mutt thinking it is unchanged
; or without having to type a letter then delete it.
(add-hook 'mail-mode-hook
  (lambda ()
    (set-buffer-modified-p t)
))

; Add bindings similar to git commit mode.
; C-c C-c saves and exits.
; C-c C-x discards and exits.
; C-x C-# exits with a save prompt. (the usual)
; C-x C-k exits with a save prompt and a "still has clients" prompt.
(add-hook 'mail-mode-hook
  (lambda() 
    (local-set-key  (kbd "C-c C-x") 'kill-buffer-unconditionally)))

(add-hook
   'mail-mode-hook
   (lambda ()
     (define-key mail-mode-map [(control c) (control c)]
       (lambda ()
         (interactive)
         (save-buffer)
         (server-edit)))))

(provide 'other)
