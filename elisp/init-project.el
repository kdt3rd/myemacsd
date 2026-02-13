;;; init-project.el --- project related utilities -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; projectile has quick navigation within a project
;; https://github.com/bbatsov/projectile
(use-package projectile
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :after ivy
  :custom
  (projectile-completion-system 'ivy)
  ;(projectile-project-search-path '("~/Development"))
  :config
  (projectile-mode 1)
  (projectile-global-mode)
  ;(setq projectile-switch-project-action 'projectile-dired)
  ;(setq projectile-sort-order 'recentf)
  ;;(setq projectile-indexing-method 'native)
  (setq projectile-mode-line-prefix "P"
        projectile-dynamic-mode-line t
        ;projectile-completion-system 'ivy
        projectile-sort-order 'default
        projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-file-exists-remote-cache-expire (* 10 60)
        projectile-file-exists-local-cache-expire (* 10 60))
  ;(setq projectile-require-project-root nil)
  ;(projectile-mode +1)
  ;(add-to-list 'projectile-globally-ignored-directories "node_modules")
  )

;;(use-package counsel-projectile
;;  :straight t
;;  :after (projectile)
;;  :bind
;;  (("C-S-p" . counsel-projectile-find-file)
;;   ("C-S-h" . counsel-projectile-rg))
;;  :config
;;  (add-to-list 'ivy-initial-inputs-alist '(counsel-projectile-switch-project . ""))
;;  (setq projectile-switch-project-action 'counsel-projectile-find-file)
;;  :hook (after-init . counsel-projectile-mode)
;;  )

(provide 'init-project)
;;; init-project.el ends here
