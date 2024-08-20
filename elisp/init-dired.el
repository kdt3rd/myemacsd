;;; init-dired.el --- Initialize misc editor modes -*- lexical-binding: t -*-

(use-feature dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :custom
  (dired-use-ls-dired t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (wdired-use-dired-vertical-movement 'sometimes))

(use-feature dired-x)

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(provide 'init-dired)
;;; init-dired.el ends here
