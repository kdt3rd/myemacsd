(eval-when-compile
  (require 'init-global-config)
  (require 'init-const))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
;;  :diminish
;;  :init
;;  (use-package amx :defer t)
;;  (ivy-mode 1)
;;  :bind
;;  (("C-s" . swiper-isearch)
;;   ("C-S-h" . counsel-ag)
;;   ("C-x b" . counsel-buffer-or-recentf)
;;   ("C-x C-b" . counsel-ibuffer)
;;   (:map ivy-minibuffer-map
;;         ("C-r" . ivy-previous-line-or-history)
;;         ("M-RET" . ivy-immediate-done))
;;   (:map counsel-find-file-map
;;         ("C-~" . counsel-goto-local-home)))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-use-selectable-prompt t)
;;  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "(%d/%d) ")
  (ivy-re-builders-alist '((swiper . ivy--regex-plus)
                           (t . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1)
  )
;;  (ivy-wrap t)
;;  :config
;;  (defun counsel-goto-local-home ()
;;      "$HOME"
;;      (interactive)
;;    (ivy--cd "~/")))
;;
;;(use-package swiper)

(use-package counsel
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-s" . 'swiper)
  ("C-S-h" . 'counsel-ag)
  :config
  (counsel-mode 1)
  (use-package flx)
  (use-package smex)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package color-rg
;;  :load-path (lambda () (expand-file-name "site-elisp/color-rg" user-emacs-directory))
;;  :if *rg*
;;  :bind ("C-M-s" . color-rg-search-input))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-search)

