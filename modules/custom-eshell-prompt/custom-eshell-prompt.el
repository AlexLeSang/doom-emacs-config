(require 'eshell-prompt-extras)

(setq eshell-prompt-extra-section-delim " ")
(setq eshell-prompt-extra-section-timestamp-format "%Y-%m-%d %H:%M:%S %Z")

(defun epe-status-formatter-custom (timestamp duration)
  "Return the status display for `epe-status'.
TIMESTAMP is the value returned by `current-time' and DURATION is the floating
time the command took to complete in seconds."
  (format "#End %s, duration %.3fs\n"
          ;; (format-time-string "%F %T" timestamp)
          (format-time-string eshell-prompt-extra-section-timestamp-format timestamp)
          duration))

(defun epe-theme-multiline-with-status-custom ()
  "A simple eshell-prompt theme with information on its own line
and status display on command termination."
  ;; If the prompt spans over multiple lines, the regexp should match
  ;; last line only.
  ;; (setq eshell-prompt-regexp "^> ")
  (setq eshell-prompt-regexp "^λ ")
  ;; (setq eshell-prompt-regexp "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} .{3} \[.*\]\n^>")
  ;; (setq eshell-prompt-regexp "^.*\]\n> ")
  ;; (setq eshell-prompt-regexp "^.* .* .*\]\n> ")
  ;; (setq eshell-prompt-regexp "^.* .* .*\]\n>")
  (concat
   (epe-colorize-with-face (concat (format-time-string eshell-prompt-extra-section-timestamp-format (current-time)) eshell-prompt-extra-section-delim) '(:weight bold))
   (epe-colorize-with-face (epe-status 'epe-status-formatter-custom) 'epe-status-face)
   (when (epe-remote-p)
     (epe-colorize-with-face
      (concat "(" (epe-remote-user) "@" (epe-remote-host) ")")
      'epe-remote-face))
   (when (and epe-show-python-info (bound-and-true-p venv-current-name))
     (epe-colorize-with-face (concat "(" venv-current-name ") ") 'epe-venv-face))
   (let ((f (cond ((eq epe-path-style 'fish) 'epe-fish-path)
                  ((eq epe-path-style 'single) 'epe-abbrev-dir-name)
                  ((eq epe-path-style 'full) 'abbreviate-file-name))))
     (pcase (epe-extract-git-component (funcall f (eshell/pwd)))
       (`(,prefix nil)
        (format
         (propertize "[%s]" 'face '(:weight bold))
         (propertize prefix 'face 'epe-dir-face)))
       (`(,prefix ,git-component)
        (format
         (epe-colorize-with-face "[%s%s@%s]" '(:weight bold))
         (epe-colorize-with-face prefix 'epe-dir-face)
         (if (string-empty-p git-component)
             ""
           (concat "/"
                   (epe-colorize-with-face git-component 'epe-git-dir-face)))
         (epe-colorize-with-face
          (concat (or (epe-git-branch)
                      (epe-git-tag))
                  (epe-git-dirty)
                  (epe-git-untracked)
                  (let ((unpushed (epe-git-unpushed-number)))
                    (unless (= unpushed 0)
                      (concat ":" (number-to-string unpushed)))))
          'epe-git-face)))))
   (epe-colorize-with-face "\nλ" '(:weight bold))
   " "))


(provide 'custom-eshell-prompt)
