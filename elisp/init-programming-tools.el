;;; -*- lexical-binding: t; -*-


(eval-when-compile
  (require 'init-const))

;; camelcase symbols as separate words...
(use-package subword
  :straight t
  :config (global-subword-mode 1))

(setq compilation-scroll-output t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package company
;;  :straight t
;;  :custom
;;  (company-global-modes '(not shell-mode shell-script-mode))
;;  (company-idle-delay 0)
;;  (company-selection-wrap-around t)
;;  (company-minimum-prefix-length 1)
;;  :config
;;  (delete 'company-files company-backends)
;;  :bind
;;  ("M-/" . company-complete-common)
;;  )
;;(add-hook 'after-init-hook (lambda () (global-company-mode) (company-tng-mode)))

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

;(use-package irony
;  :defer t
;  :hook ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;tofix;(use-package smartparens
;tofix;  :hook (prog-mode . smartparens-mode)
;tofix;  :bind
;tofix;  (:map smartparens-mode-map
;tofix;        ("C-M-f" . sp-forward-sexp)
;tofix;        ("C-M-b" . sp-backward-sexp)
;tofix;        ("C-M-a" . sp-backward-down-sexp)
;tofix;        ("C-M-e" . sp-up-sexp)
;tofix;        ("C-M-w" . sp-copy-sexp)
;tofix;        ("C-M-k" . sp-change-enclosing)
;tofix;        ("M-k" . sp-kill-sexp)
;tofix;        ("C-." . bounce-sexp)
;tofix;        ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
;tofix;        ("C-S-<backspace>" . sp-splice-sexp-killing-around)
;tofix;        ("C-]" . sp-select-next-thing-exchange))
;tofix;  ;:custom
;tofix;  ;(sp-escape-quotes-after-insert nil)
;tofix;  ;;; don't use default key bindings as I prefer
;tofix;  ;;; control- arrows to move between words
;tofix;  ;(sp-base-key-bindings 'sp)
;tofix;  ;(sp-override-key-bindings
;tofix;  ; '(
;tofix;  ;   ("C-." . bounce-sexp)
;tofix;  ;   ("C-<left>" . nil)
;tofix;  ;   ("C-<right>" . nil)
;tofix;  ;   ("C-M-<left>" . nil)
;tofix;  ;   ("C-M-<right>" . nil)
;tofix;  ;   ))
;tofix;  :config
;tofix;  ;; Stop pairing single quotes in elisp
;tofix;  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
;tofix;  (sp-local-pair 'org-mode "[" nil :actions nil)
;tofix;  (require 'smartparens-c)
;tofix;  ;;; see above
;tofix;  ;;(sp-use-smartparens-bindings)
;tofix;
;tofix;  ;; This now appears fixed...
;tofix;  ;;;; Smartparens is broken in `cc-mode' as of Emacs 27. See
;tofix;  ;;;; https://github.com/Fuco1/smartparens/issues/963
;tofix;  ;;(unless (version< emacs-version "27")
;tofix;  ;;  (dolist (fun '(c-electric-paren c-electric-brace))
;tofix;  ;;    (add-to-list 'sp--special-self-insert-commands fun)))
;tofix;  (defun bounce-sexp ()
;tofix;    "Will bounce between matching parens just like % in vi"
;tofix;    (interactive)
;tofix;    (let ((prev-char (char-to-string (preceding-char)))
;tofix;		  (next-char (char-to-string (following-char))))
;tofix;	  (cond ((string-match "[[{(<]" next-char) (sp-forward-sexp 1))
;tofix;		    ((string-match "[\]})>]" prev-char) (sp-backward-sexp 1))
;tofix;		    (t (error "%s" "Not on a paren, brace, or bracket")))))
;tofix;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package iedit
  ;; Type \"C-;\" to select current symbol and all matches; Then edit at multiple
  ;; points.
  :straight t)
;;(use-package undo-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is from llvm folks directly, there is also
;; https://github.com/SavchenkoValeriy/emacs-clang-format-plus
;; which applies only to modified regions, but does crappy stuff like runs on
;; save...
(use-package clang-format
  :straight t
  :bind
  (("C-S-i" . clang-format-buffer)
   ("C-S-r" . clang-format-region))
  :config
  (setq clang-format-style "file")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;tofix;; magit https://magit.vc
;tofix;(use-package magit
;tofix;  :if *git*
;tofix;  :bind
;tofix;  ("C-x g" . magit-status)
;tofix;  :config
;tofix;  (setq git-commit-summary-max-length 50)
;tofix;                                        ;(with-eval-after-load 'magit-remote
;tofix;                                        ;  (magit-define-popup-action 'magit-push-popup ?P
;tofix;                                        ;                             'magit-push-implicitly--desc
;tofix;                                        ;                             'magit-push-implicitly ?p t))
;tofix;  )
(use-package magit
  ;; A fantastic UI for git commands; the interactive rebase is an absolute
  ;; wonder tool (see
  ;; https://takeonrules.com/2023/01/12/using-the-git-interactive-staging-as-a-moment-to-facilitate-synthesis/).
  ;; Also the progenitor of `transient'
  :straight (:host github :repo "magit/magit")
  :commands (magit-process-git)
  ;; My "~/bin/editor" script was causing problems in that it was asking to wait.
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Adding format to git-commit-fill-column of 72 as best practice.
  (setq git-commit-fill-column 72)
  ;; Keeping the summary terse helps with legibility when you run a
  ;; report with only summary.
  (setq git-commit-summary-max-length 50)
  ;; Set the tabular display columns for the `magit-list-repositories'
  (setq magit-repolist-columns
    '(("Name"    25 magit-repolist-column-ident ())
       ("Version" 25 magit-repolist-column-version ())
       ("δ"        1 magit-repolist-column-flag ())
       ("⇣"        3 magit-repolist-column-unpulled-from-upstream
         ((:right-align t)
           (:help-echo "Upstream changes not in branch")))
       ("⇡"        3 magit-repolist-column-unpushed-to-upstream
         ((:right-align t)
           (:help-echo "Local changes not in upstream")))
       ("Branch"  25 magit-repolist-column-branch ())
       ("Path"    99 magit-repolist-column-path ())))
  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  ;; (defadvice magit-status (around magit-fullscreen activate)
  ;;   (window-configuration-to-register :magit-fullscreen)
  ;;   ad-do-it
  ;;   (delete-other-windows))
  ;; (defadvice magit-mode-quit-window (after magit-restore-screen activate)
  ;;   (jump-to-register :magit-fullscreen))
  ;; (use-package libgit :straight t)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  :bind (("C-c m" . magit-status)
          ("C-x g m" . magit-status)
          ("C-x g f" . magit-file-dispatch)
          ("C-x g d" . magit-dispatch))
  )

;; for github PRs management
;; TODO: look this stuff up
;;(use-package ghub)
;;(use-package forge)

;;(use-package git-timemachine)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;tofix;(use-package dumb-jump
;tofix;  :bind
;tofix;  (:map prog-mode-map
;tofix;        (("C-c C-o" . dumb-jump-go-other-window)
;tofix;         ([f12] . dumb-jump-go)
;tofix;         ([S-f12] . dumb-jump-back)
;tofix;         ("C-c C-i" . dumb-jump-go-prompt)))
;tofix;  :config
;tofix;  (setq dumb-jump-aggressive nil)
;tofix;  :custom (dumb-jump-selector 'ivy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package let-alist :straight t)
;(use-package flycheck
;  :config
;  (add-hook 'markdown-mode-hook #'flycheck-mode)
;  (add-hook 'gfm-mode-hook #'flycheck-mode)
;  (add-hook 'text-mode-hook #'flycheck-mode)
;  (add-hook 'org-mode-hook #'flycheck-mode)
;  )
;;  :defer t
;;  :hook (prog-mode . flycheck-mode)
;;  :custom
;;  (flycheck-emacs-lisp-load-path 'inherit)
;;  :config
;;  (flycheck-add-mode 'javascript-eslint 'js-mode)
;;  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))

;(use-package flyspell
;  :config
;  (add-hook 'text-mode-hook 'turn-on-auto-fill)
;  (add-hook 'gfm-mode-hook 'flyspell-mode)
;  (add-hook 'org-mode-hook 'flyspell-mode)
;
;  (add-hook 'git-commit-mode-hook 'flyspell-mode)
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-programming-tools)
