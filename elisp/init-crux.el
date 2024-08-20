;;; init-crux.el --- Crux Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package crux
  :defer 5
  :commands crux-start-or-switch-to
  :config
  (defmacro crux-with-region-or-sexp-or-line (func)
    "When called with no active region, call FUNC on current sexp."
    `(defadvice ,func (before with-region-or-sexp-or-line activate compile)
       (interactive
        (cond
         (mark-active (list (region-beginning) (region-end)))
         ((in-string-p) (flatten-list (bounds-of-thing-at-point 'string)))
         ((thing-at-point 'list) (flatten-list (bounds-of-thing-at-point 'list)))
         (t (list (line-beginning-position) (line-beginning-position 2)))))))

  (crux-with-region-or-sexp-or-line sp-kill-region)
  (crux-with-region-or-buffer shell-command-on-region)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer)
  :bind
  ("C-S-<backspace>" . crux-kill-line-backwards)
  ([remap kill-whole-line] . crux-kill-whole-line)
  ("C-k" . crux-smart-kill-line)
  ("C-a" . crux-move-beginning-of-line)
  ([remap open-line] . crux-smart-open-line)
  ;; TODO don't need all of these
  ;;("C-^" . crux-top-join-line)
  ;;("C-<return>" . crux-smart-open-line)
  ;;("S-<return>" . crux-smart-open-line)
  ;;("C-S-<return>" . crux-smart-open-line-above)
  ;;("M-o" . crux-smart-open-line-above)
  ("C-c C-o" . crux-duplicate-current-line-or-region)
  ("C-c M-o" . crux-duplicate-and-comment-current-line-or-region)
  ("C-c F" . crux-recentf-find-file)
  ;;("C-x 4 s" . crux-swap-windows)
  ;;("C-x M-o" . crux-other-window-or-switch-buffer)
  ;;("C-c C-M-x" . crux-eval-and-replace)
  ("C-c K" . crux-kill-other-buffers)
  ;;("C-c C-r" . crux-rename-buffer-and-file)
  ;;("C-c D" . crux-delete-file-and-buffer)
  ;;("C-c w" . crux-cleanup-buffer-or-region)
  ;;("C-c M-o" . crux-open-with)
  ;;("C-c C-u" . crux-view-url)
  ;;("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
  ;;("C-c C-M-j" . crux-switch-to-previous-buffer)
  ;;("C-c C-!" . crux-reopen-as-root))
   )

(provide 'init-crux)
;;; init-crux.el ends here
