;;; svn-commit.el --- Major mode for editing svn commit messages

;; This is a hacked version of git-commit-model.el by Ross Lagerwall.
;; Here follows the original copyright message:
;; This software is Copyright (c) 2010 by Florian Ragwitz.
;;
;; This is free software, licensed under:
;;   The GNU General Public License, Version 2, June 1991

(defgroup svn-commit '((jit-lock custom-group))
  "Mode for editing svn commit messages"
  :group 'faces)

(defgroup svn-commit-faces nil
  "Faces for highlighting svn commit messages"
  :prefix "svn-commit-"
  :group 'svn-commit)

(defface svn-commit-summary-face
  '((default (:weight bold))
    (((class grayscale) (background light))
     (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light))
     (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight the summary in svn commit messages"
  :group 'svn-commit-faces)

(defface svn-commit-overlong-summary-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 88) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 16) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 16) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "Face used to highlight overlong parts of svn commit message summaries"
  :group 'svn-commit-faces)

(defface svn-commit-nonempty-second-line-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 88) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 16) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 16) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "Face used to highlight text on the second line of svn commit messages"
  :group 'svn-commit-faces)

(defface svn-commit-text-face
  '((t (:inherit default)))
  "Face used to highlight text in svn commit messages"
  :group 'svn-commit-faces)

(defface svn-commit-comment-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "chocolate1"))
    (((class color) (min-colors 16) (background light))
     (:foreground "red"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "red1"))
    (((class color) (min-colors 8) (background light))
     (:foreground "red"))
    (((class color) (min-colors 8) (background dark)))
    (t (:weight bold :slant italic)))
  "Face used to highlight comments in svn commit messages"
  :group 'svn-commit-faces)

(defface svn-commit-comment-file-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Face used to hightlight file names in the default comments in
svn commit messages"
  :group 'svn-commit-faces)

(defface svn-commit-comment-action-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "VioletRed2"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light))
     (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight what has happened to files in the
default comments in svn commit messages"
  :group 'svn-commit-faces)

(defvar svn-commit-font-lock-keywords
   '(("\\(D\\)\\(.*\\)"
     (1 'svn-commit-comment-action-face)
     (2 'svn-commit-comment-file-face))
    ("\\(A\\)\\(.*\\)"
     (1 'svn-commit-comment-action-face)
     (2 'svn-commit-comment-file-face))
    ("\\(M\\)\\(.*\\)"
     (1 'svn-commit-comment-action-face)
     (2 'svn-commit-comment-file-face))
    ("\\`\\(?:\\(?:[[:space:]]*\\|#.*\\)\n\\)*\\(.\\{,50\\}\\)\\(.*?\\)\\(?:\n\\(.*\\)\\)?$"
     (1 'svn-commit-summary-face)
     (2 'svn-commit-overlong-summary-face)
     (3 'svn-commit-nonempty-second-line-face))
    ("^--This line, and those below, will be ignored--$" . 'svn-commit-comment-face)
    (".*" . 'svn-commit-text-face)))


(defvar svn-commit-mode-hook nil
  "List of functions to be called when activating `svn-commit-mode'.")

(defun svn-commit--save-and-exit ()
  (save-buffer)
  (kill-buffer))

(defcustom svn-commit-commit-function
  #'svn-commit--save-and-exit
  "Function to actually perform a commit.
Used by `svn-commit-commit'."
  :group 'svn-commit
  :type '(radio (function-item :doc "Call `save-buffers-kill-terminal'."
                               svn-commit--save-and-exit)
                (function)))

(defun svn-commit-commit ()
  "Finish editing the commit message and commit.
By default this only calls `save-buffer', as there is no general
way to actually trigger svn to commit whatever the commit message
was intended for.

After calling `save-buffer', the hooks in
`svn-commit-commit-hook' will be run.  If you have configured svn
in a way that simply invokes Emacs for editing the commit
message, you might want to this:

  (add-hook 'svn-commit-commit-hook
          (lambda () (save-buffers-kill-terminal)))"
  (interactive)
  (save-buffer)
  (save-buffers-kill-terminal))

(defvar svn-commit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'svn-commit-commit)
    map))


;;;###autoload
(defun svn-commit-mode ()
  "Major mode for editing svn commit messages.
This mode helps with editing svn commit messages both by
by highlighting the basic structure of and errors in svn
commit messages.

Commands:\\<svn-commit-map>
\\[svn-commit-commit]   `svn-commit-commit'  Finish editing and commit

Turning on svn commit calls the hooks in `svn-commit-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map svn-commit-map)
  (setq font-lock-multiline t)
  (setq font-lock-defaults '(svn-commit-font-lock-keywords t))
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start-skip "^#\s"
        comment-start "# "
        comment-end "")
  (setq major-mode 'svn-commit-mode)
  (when (fboundp 'toggle-save-place)
    (toggle-save-place 0))
  (run-mode-hooks 'svn-commit-mode-hook)
  (setq mode-name "Svn-Commit"))

;;;###autoload
(when (boundp 'session-mode-disable-list)
  (add-to-list 'session-mode-disable-list 'svn-commit-mode))

;;;###autoload
(setq auto-mode-alist
  (append
    '(("svn-commit.*tmp" . svn-commit-mode))
      auto-mode-alist))

(provide 'svn-commit)

;;; svn-commit.el ends here
