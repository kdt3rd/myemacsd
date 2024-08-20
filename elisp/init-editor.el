;;; init-editor.el --- editor wide init -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature midnight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature compile
  :custom
  (compilation-ask-about-save nil)
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error))

;; http://stackoverflow.com/a/3072831/355252
(use-feature ansi-color
  :hook
  (compilation-filter . (lambda ()
                          (when (eq major-mode 'compilation-mode)
                            (let ((inhibit-read-only t))
                              (ansi-color-apply-on-region (point-min) (point-max)))))))

(use-package winnow
  :hook (compilation-mode . winnow-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package simple-modeline
  :hook (elpaca-after-init . simple-modeline-mode)
  :custom
  (simple-modeline-segments
   '((simple-modeline-segment-modified
      simple-modeline-segment-buffer-name
      simple-modeline-segment-position)
     (simple-modeline-segment-minor-modes
      simple-modeline-segment-vc
      simple-modeline-segment-misc-info
      simple-modeline-segment-process
      simple-modeline-segment-major-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature ispell
  :config
  (setq
   ispell-program-name "aspell"
   ispell-dictionary "en"
   ispell-extra-args '("--camel-case") ;; TeX mode "-t"
   ispell-silently-savep t
   ispell-alternate-dictionary "/usr/share/dict/american-english")
  ;;(defun my:org-ispell ()
  ;;  "Configure `ispell-skip-region-alist' for `org-mode'."
  ;;  (make-local-variable 'ispell-skip-region-alist)
  ;;  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  ;;  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  ;;  (add-to-list 'ispell-skip-region-alist '("=" "="))
  ;;  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
  )

(use-package flycheck
  :custom
  (flycheck-emacs-lisp-load-path 'inherit "necessary with alternatives to package.el")
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-cppcheck))
  (global-flycheck-mode)
  )

(use-package flycheck-aspell
  :hook
  (elpaca-after-init . (lambda ()
                         (require 'flycheck-aspell)
                         (flycheck-aspell-define-checker "org"
                                                         "Org" ("--add-filter" "url")
                                                         (org-mode))
                         (add-to-list 'flycheck-checkers 'org-aspell-dynamic)
                         ;; this doesn't seem to work...
                         ;;(add-to-list 'flycheck-checkers 'c-aspell-dynamic)
                       ))
  )

(use-package flycheck-indicator
  :hook (flycheck-mode . flycheck-indicator-mode)
  :custom
  (flycheck-indicator-icon-error 9632)
  (flycheck-indicator-icon-info 9679)
  (flycheck-indicator-icon-warning 9650)
  (flycheck-indicator-status-icons
   '((running . "◉")
     (errored . "◙")
     (finished . "●")
     (interrupted . "◘")
     (suspicious . "◘")
     (no-checker . "○")
     (not-checked . "○"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If you add this make sure to add snails as a submodule
;;; https://github.com/manateelazycat/snails.git
;;;
;;; so something like git submodule add https://github.com/manateelazycat/snails.git site-elisp/snails
;;;
;;; (use-package snails
;;;   :load-path (lambda () (expand-file-name "site-elisp/snails/" user-emacs-directory))
;;;   :if *sys/gui*
;;;   :custom-face
;;;   (snails-content-buffer-face ((t (:background "#111" :height 110))))
;;;   (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
;;;   (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
;;;   :config
;;;   (use-package exec-path-from-shell
;;;     :if (featurep 'cocoa) :defer t)
;;; 
;;;   ;; Functions for specific backends
;;;   (defun snails-current-project ()
;;;     (interactive)
;;;     (snails '(snails-backend-projectile snails-backend-rg snails-backend-fd)))
;;;   (defun snails-active-recent-buffers ()
;;;     (interactive)
;;;     (snails '(snails-backend-buffer snails-backend-recentf)))
;;;   (defun snails-everywhere ()
;;;     (interactive)
;;;     (snails '(snails-backend-everything snails-backend-mdfind)))
;;;   :bind
;;;   (("M-s s" . snails)
;;;    ("M-s g" . snails-current-project)
;;;    ("M-s b" . snails-active-recent-buffers)
;;;    ("M-s e" . snails-everywhere)))

(provide 'init-editor)
;;; init-editor.el ends here
