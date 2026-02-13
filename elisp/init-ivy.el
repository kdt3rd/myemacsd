;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'init-global-config)
  (require 'init-const))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package ivy
  :straight t
  :config
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 4))))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap nil)
  ;(setq ivy-re-builders-alist
  ;      '((counsel-M-x . ivy--regex-fuzzy)
  ;        (ivy-switch-buffer . ivy--regex-fuzzy)
  ;        (ivy-switch-buffer-other-window . ivy--regex-fuzzy)
  ;        (counsel-rg . ivy--regex-or-literal)
  ;        (t . ivy--regex-plus)))
  (setq ivy-display-style 'fancy)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-fixed-height-minibuffer nil)
  (setq ivy-initial-inputs-alist nil)
  ;(setq ivy-initial-inputs-alist
  ;      '((counsel-M-x . "^")
  ;        (ivy-switch-buffer . "^")
  ;        (ivy-switch-buffer-other-window . "^")
  ;        (counsel-describe-function . "^")
  ;        (counsel-describe-variable . "^")
  ;        (t . "")))
  ;(ivy-set-occur 'counsel-fzf 'counsel-fzf-occur)
  ;(ivy-set-occur 'counsel-rg 'counsel-ag-occur)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (ivy-set-occur 'swiper-multi 'counsel-ag-occur)
  (ivy-mode t)
  ;:hook ((after-init . ivy-mode)
  ;       (ivy-occur-mode . hl-line-mode))

  ;:custom
  ;(ivy-use-virtual-buffers t)
  ;(ivy-height 10)
  ;(ivy-on-del-error-function nil)
  ;(ivy-use-selectable-prompt t)
;;  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  ;(ivy-re-builders-alist '((swiper . ivy--regex-plus)
  ;                         (t . ivy--regex-fuzzy)))
  ;:config
  ;(ivy-mode 1)

  ;:bind (("<s-up>" . ivy-push-view)
  ;       ("<s-down>" . ivy-switch-view)
  ;       ("C-S-r" . ivy-resume)
  ;       :map ivy-occur-mode-map
  ;       ("f" . forward-char)
  ;       ("b" . backward-char)
  ;       ("n" . ivy-occur-next-line)
  ;       ("p" . ivy-occur-previous-line)
  ;       ("<C-return>" . ivy-occur-press))
  )
;;  (ivy-wrap t)
;;  :config
;;  (defun counsel-goto-local-home ()
;;      "$HOME"
;;      (interactive)
;;    (ivy--cd "~/")))
;;

(use-package swiper
  :straight t
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t)
  (setq swiper-include-line-number-in-search t)
  (ivy-set-occur 'swiper 'swiper-occur)
  (ivy-set-occur 'swiper-isearch 'swiper-occur)
  )

(use-package ivy-rich
  :straight t
  :after ivy
  :config
  (setq ivy-rich-path-style 'abbreviate)
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  :hook (after-init . ivy-rich-mode)
  )

(use-package ivy-posframe
  :straight t
  :after ivy
  :delight
  :config
  (setq ivy-posframe-width 850)
  (setq ivy-posframe-parameters
        '((left-fringe . 2)
          (right-fringe . 2)
          (internal-border-width . 2)))
  (setq ivy-posframe-height-alist
        '((swiper . 15)
          (swiper-isearch . 15)
          (t . 10)))
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . ivy-display-function-fallback)
          (swiper-isearch . ivy-display-function-fallback)
          (t . ivy-posframe-display-at-window-center)
          ;(t . ivy-posframe-display-at-frame-center)
          )
        )
  :hook (after-init . ivy-posframe-mode)
  )

(use-package prescient
  :straight t
  :config
  (setq prescient-history-length 200)
  (setq prescient-save-file "~/.emacs.d/prescient-items")
  (setq prescient-filter-method '(literal regexp))
  (prescient-persist-mode t))

(use-package ivy-prescient
  :straight t
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not counsel-grep
               counsel-rg
               counsel-switch-buffer
               ivy-switch-buffer
               swiper
               swiper-multi))
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-enable-sorting t)
  (ivy-prescient-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-ivy)
