
(eval-when-compile
  (require 'init-const))

;; camelcase symbols as separate words...
(use-package subword
  :config (global-subword-mode 1))
(setq compilation-scroll-output t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :bind
    ("M-/" . company-complete-common))
(add-hook 'after-init-hook 'global-company-mode)

;;;; see memacs-d init-company.el
;;;; they enable company- tabnine, lsp, and box
;;(use-package company
;;  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
;;  :bind
;;  (:map company-active-map
;;        ([tab] . smarter-yas-expand-next-field-complete)
;;        ("TAB" . smarter-yas-expand-next-field-complete))
;;  :custom
;;  (company-minimum-prefix-length 1)
;;  (company-tooltip-align-annotations t)
;;  (company-begin-commands '(self-insert-command))
;;  (company-require-match 'never)
;;  ;; Don't use company in the following modes
;;  (company-global-modes '(not shell-mode eaf-mode))
;;  ;; Trigger completion nearly immediately.
;;  (company-idle-delay 0.1)
;;  ;; Number the candidates (use M-1, M-2 etc to select completions).
;;  (company-show-numbers t)
;;  :config
;;  (unless *clangd* (delete 'company-clang company-backends))
;;  (global-company-mode 1)
;;  (defun smarter-yas-expand-next-field-complete ()
;;    "Try to `yas-expand' and `yas-next-field' at current cursor position.
;;
;;If failed try to complete the common part with `company-complete-common'"
;;    (interactive)
;;    (if yas-minor-mode
;;        (let ((old-point (point))
;;              (old-tick (buffer-chars-modified-tick)))
;;          (yas-expand)
;;          (when (and (eq old-point (point))
;;                     (eq old-tick (buffer-chars-modified-tick)))
;;            (ignore-errors (yas-next-field))
;;            (when (and (eq old-point (point))
;;                       (eq old-tick (buffer-chars-modified-tick)))
;;              (company-complete-common))))
;;      (company-complete-common))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-a" . sp-backward-down-sexp)
        ("C-M-e" . sp-up-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-k" . sp-change-enclosing)
        ("M-k" . sp-kill-sexp)
        ("C-." . bounce-sexp)
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" . sp-splice-sexp-killing-around)
        ("C-]" . sp-select-next-thing-exchange))
  :custom
  (sp-escape-quotes-after-insert nil)
  ;;; don't use default key bindings as I prefer
  ;;; control- arrows to move between words
  ;(sp-base-key-bindings 'sp)
  ;(sp-override-key-bindings
  ; '(
  ;   ("C-." . bounce-sexp)
  ;   ("C-<left>" . nil)
  ;   ("C-<right>" . nil)
  ;   ("C-M-<left>" . nil)
  ;   ("C-M-<right>" . nil)
  ;   ))
  :config
  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "[" nil :actions nil)
  (require 'smartparens-c)
  ;;; see above
  ;;(sp-use-smartparens-bindings)

  ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
  ;; https://github.com/Fuco1/smartparens/issues/963
  (unless (version< emacs-version "27")
    (dolist (fun '(c-electric-paren c-electric-brace))
      (add-to-list 'sp--special-self-insert-commands fun)))
  (defun bounce-sexp ()
    "Will bounce between matching parens just like % in vi"
    (interactive)
    (let ((prev-char (char-to-string (preceding-char)))
		  (next-char (char-to-string (following-char))))
	  (cond ((string-match "[[{(<]" next-char) (sp-forward-sexp 1))
		    ((string-match "[\]})>]" prev-char) (sp-backward-sexp 1))
		    (t (error "%s" "Not on a paren, brace, or bracket")))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package iedit)
;;(use-package undo-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is from llvm folks directly, there is also
;; https://github.com/SavchenkoValeriy/emacs-clang-format-plus
;; which applies only to modified regions, but does crappy stuff like runs on
;; save...
(use-package clang-format
  :bind
  (("C-S-i" . clang-format-buffer)
   ("C-S-r" . clang-format-region))
  :config
  (setq clang-format-style "file")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit https://magit.vc
(use-package magit
  :if *git*
  :bind
  ("C-x g" . magit-status)
  :config
  (setq git-commit-summary-max-length 50)
                                        ;(with-eval-after-load 'magit-remote
                                        ;  (magit-define-popup-action 'magit-push-popup ?P
                                        ;                             'magit-push-implicitly--desc
                                        ;                             'magit-push-implicitly ?p t))
  )

;; for github PRs management
;; TODO: look this stuff up
;;(use-package ghub)
;;(use-package forge)

;;(use-package git-timemachine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; projectile has quick navigation within a project
;; https://github.com/bbatsov/projectile
(use-package projectile
  ;:bind
                                        ;("C-c p" . projectile-command-map)
                                        ;("C-x p" . projectile-add-known-project)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  :config
  ;;(projectile-mode 1)
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-require-project-root nil)
  ;(add-to-list 'projectile-globally-ignored-directories "node_modules")
  )

(use-package counsel-projectile
  :after (projectile)
  :bind
  (("C-S-p" . counsel-projectile-find-file)
   ("C-S-h" . counsel-projectile-ag))
  :config
  (counsel-projectile-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (use-package treemacs
;;;   :init
;;;   (with-eval-after-load 'winum
;;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;;   :custom
;;;   (treemacs-collapse-dirs 3)
;;;   (treemacs-deferred-git-apply-delay 0.5)
;;;   (treemacs-display-in-side-window t)
;;;   (treemacs-file-event-delay 5000)
;;;   (treemacs-file-follow-delay 0.2)
;;;   (treemacs-follow-after-init t)
;;;   (treemacs-follow-recenter-distance 0.1)
;;;   (treemacs-git-command-pipe "")
;;;   (treemacs-goto-tag-strategy 'refetch-index)
;;;   (treemacs-indentation 2)
;;;   (treemacs-indentation-string " ")
;;;   (treemacs-is-never-other-window nil)
;;;   (treemacs-max-git-entries 5000)
;;;   (treemacs-no-png-images nil)
;;;   (treemacs-no-delete-other-windows t)
;;;   (treemacs-project-follow-cleanup nil)
;;;   (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
;;;   (treemacs-recenter-after-file-follow nil)
;;;   (treemacs-recenter-after-tag-follow nil)
;;;   (treemacs-show-cursor nil)
;;;   (treemacs-show-hidden-files t)
;;;   (treemacs-silent-filewatch nil)
;;;   (treemacs-silent-refresh nil)
;;;   (treemacs-sorting 'alphabetic-desc)
;;;   (treemacs-space-between-root-nodes t)
;;;   (treemacs-tag-follow-cleanup t)
;;;   (treemacs-tag-follow-delay 1.5)
;;;   (treemacs-width 35)
;;;   :config
;;;   ;; The default width and height of the icons is 22 pixels. If you are
;;;   ;; using a Hi-DPI display, uncomment this to double the icon size.
;;;   ;;(treemacs-resize-icons 44)
;;;   (treemacs-follow-mode t)
;;;   (treemacs-filewatch-mode t)
;;;   (treemacs-fringe-indicator-mode t)
;;;   :bind
;;;   (("M-0"       . treemacs-select-window)
;;;    ("C-x t 1"   . treemacs-delete-other-windows)
;;;    ("C-x t t"   . treemacs)
;;;    ("C-x t B"   . treemacs-bookmark)
;;;    ("C-x t C-t" . treemacs-find-file)
;;;    ("C-x t M-t" . treemacs-find-tag))
;;;   (:map treemacs-mode-map ("C-p" . treemacs-previous-line)))
;;; 
;;; (use-package treemacs-magit
;;;   :defer t
;;;   :after (treemacs magit))
;;; 
;;; (use-package treemacs-projectile
;;;   :defer t
;;;   :after (treemacs projectile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :config
  (use-package yasnippet-snippets :after yasnippet)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  (setq yas/indent-line nil)
  ;;:hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
  ;;:bind
  ;;((:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
  ;; (:map yas-keymap
  ;;       (("TAB" . smarter-yas-expand-next-field)
  ;;        ([(tab)] . smarter-yas-expand-next-field))))
  ;;:config
  ;;(yas-reload-all)
  ;;(defun smarter-yas-expand-next-field ()
  ;;  "Try to `yas-expand' then `yas-next-field' at current cursor position."
  ;;  (interactive)
  ;;  (let ((old-point (point))
  ;;        (old-tick (buffer-chars-modified-tick)))
  ;;    (yas-expand)
  ;;    (when (and (eq old-point (point))
  ;;               (eq old-tick (buffer-chars-modified-tick)))
  ;;      (ignore-errors (yas-next-field)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dumb-jump
  :bind
  (:map prog-mode-map
        (("C-c C-o" . dumb-jump-go-other-window)
         ([f12] . dumb-jump-go)
         ([S-f12] . dumb-jump-back)
         ("C-c C-i" . dumb-jump-go-prompt)))
  :config
  (setq dumb-jump-aggressive nil)
  :custom (dumb-jump-selector 'ivy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package let-alist)
(use-package flycheck
  :config
  (add-hook 'markdown-mode-hook #'flycheck-mode)
  (add-hook 'gfm-mode-hook #'flycheck-mode)
  (add-hook 'text-mode-hook #'flycheck-mode)
  (add-hook 'org-mode-hook #'flycheck-mode)
  )
;;  :defer t
;;  :hook (prog-mode . flycheck-mode)
;;  :custom
;;  (flycheck-emacs-lisp-load-path 'inherit)
;;  :config
;;  (flycheck-add-mode 'javascript-eslint 'js-mode)
;;  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))

(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'gfm-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(use-package irony
;  :defer t
;  :hook ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-programming-tools)
