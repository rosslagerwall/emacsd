;;; hg-commit.el --- Major mode for editing hg commit messages

;; This is a hacked version of git-commit-model.el by Ross Lagerwall.
;; Here follows the original copyright message:
;; This software is Copyright (c) 2010 by Florian Ragwitz.
;;
;; This is free software, licensed under:
;;   The GNU General Public License, Version 2, June 1991

(defgroup hg-commit '((jit-lock custom-group))
  "Mode for editing hg commit messages"
  :group 'faces)

(defgroup hg-commit-faces nil
  "Faces for highlighting hg commit messages"
  :prefix "hg-commit-"
  :group 'hg-commit)

(defface hg-commit-summary-face
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
  "Face used to highlight the summary in hg commit messages"
  :group 'hg-commit-faces)

(defface hg-commit-overlong-summary-face
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
  "Face used to highlight overlong parts of hg commit message summaries"
  :group 'hg-commit-faces)

(defface hg-commit-nonempty-second-line-face
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
  "Face used to highlight text on the second line of hg commit messages"
  :group 'hg-commit-faces)

(defface hg-commit-text-face
  '((t (:inherit default)))
  "Face used to highlight text in hg commit messages"
  :group 'hg-commit-faces)

(defface hg-commit-comment-face
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
  "Face used to highlight comments in hg commit messages"
  :group 'hg-commit-faces)

(defface hg-commit-branch-face
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
  "Face used to highlight the branch name in comments in hg commit messages"
  :group 'hg-commit-faces)

(defface hg-commit-comment-file-face
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
hg commit messages"
  :group 'hg-commit-faces)

(defface hg-commit-comment-action-face
  '((t (:inherit hg-commit-branch-face)))
  "Face used to highlight what has happened to files in the
default comments in hg commit messages"
  :group 'hg-commit-faces)

(defvar hg-commit-font-lock-keywords
   '(("^\\(HG: branch '\\)\\(.*\\)\\('\\)$"
     (1 'hg-commit-comment-face)
     (2 'hg-commit-branch-face)
     (3 'hg-commit-comment-face))
    ("^\\(HG: \\)\\(added\\)\\(.*\\)$"
     (1 'hg-commit-comment-face)
     (2 'hg-commit-comment-action-face)
     (3 'hg-commit-comment-file-face))
    ("^\\(HG: \\)\\(removed\\)\\(.*\\)$"
     (1 'hg-commit-comment-face)
     (2 'hg-commit-comment-action-face)
     (3 'hg-commit-comment-file-face))
    ("^\\(HG: \\)\\(changed\\)\\(.*\\)$"
     (1 'hg-commit-comment-face)
     (2 'hg-commit-comment-action-face)
     (3 'hg-commit-comment-file-face))
    ("^HG:.*$" . 'hg-commit-comment-face)
    ("\\`\\(?:\\(?:[[:space:]]*\\|#.*\\)\n\\)*\\(.\\{,50\\}\\)\\(.*?\\)\\(?:\n\\(.*\\)\\)?$"
     (1 'hg-commit-summary-face)
     (2 'hg-commit-overlong-summary-face)
     (3 'hg-commit-nonempty-second-line-face))
    (".*" . 'hg-commit-text-face)))

(defvar hg-commit-mode-hook nil
  "List of functions to be called when activating `hg-commit-mode'.")

(defun hg-commit--save-and-exit ()
  (save-buffer)
  (kill-buffer))

(defcustom hg-commit-commit-function
  #'hg-commit--save-and-exit
  "Function to actually perform a commit.
Used by `hg-commit-commit'."
  :group 'hg-commit
  :type '(radio (function-item :doc "Call `save-buffers-kill-terminal'."
                               hg-commit--save-and-exit)
                (function)))

(defun hg-commit-commit ()
  "Finish editing the commit message and commit.
By default this only calls `save-buffer', as there is no general
way to actually trigger hg to commit whatever the commit message
was intended for.

After calling `save-buffer', the hooks in
`hg-commit-commit-hook' will be run.  If you have configured hg
in a way that simply invokes Emacs for editing the commit
message, you might want to this:

  (add-hook 'hg-commit-commit-hook
          (lambda () (save-buffers-kill-terminal)))"
  (interactive)
  (save-buffer)
  (save-buffers-kill-terminal))

(defvar hg-commit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'hg-commit-commit)
    map))


;;;###autoload
(defun hg-commit-mode ()
  "Major mode for editing hg commit messages.
This mode helps with editing hg commit messages both by
by highlighting the basic structure of and errors in hg
commit messages.

Commands:\\<hg-commit-map>
\\[hg-commit-commit]   `hg-commit-commit'  Finish editing and commit

Turning on hg commit calls the hooks in `hg-commit-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map hg-commit-map)
  (setq font-lock-multiline t)
  (setq font-lock-defaults '(hg-commit-font-lock-keywords t))
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start-skip "^#\s"
        comment-start "# "
        comment-end "")
  (setq major-mode 'hg-commit-mode)
  (when (fboundp 'toggle-save-place)
    (toggle-save-place 0))
  (run-mode-hooks 'hg-commit-mode-hook)
  (setq mode-name "Hg-Commit"))

;;;###autoload
(when (boundp 'session-mode-disable-list)
  (add-to-list 'session-mode-disable-list 'hg-commit-mode))

;;;###autoload
(setq auto-mode-alist
  (append
    '(("/tmp/hg-editor-.*\.txt" . hg-commit-mode))
      auto-mode-alist))

(provide 'hg-commit)

;;; hg-commit.el ends here
