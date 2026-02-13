;;; init-lsp.el --- Initialize misc editor modes -*- lexical-binding: t -*-

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :after (flycheck treesit)
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil) ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  :init
  (setq lsp-use-plists t)
  )

(use-package lsp-completion
  :after lsp-mode
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands
  (lsp-ui-mode
   lsp-ui-doc-show
   lsp-ui-doc-glance)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if *sys/gui*
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

;;(use-package lsp-ui
;;  :after lsp-mode
;;  :ensure t
;;  :commands
;;  (lsp-ui-doc-show
;;   lsp-ui-doc-glance)
;;  :bind (:map lsp-mode-map
;;              ("C-c C-d" . 'lsp-ui-doc-glance))
;;  :after (lsp-mode)
;;  :config (setq lsp-ui-doc-enable t
;;                ;;evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
;;                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
;;                lsp-ui-doc-include-signature t       ; Show signature
;;                lsp-ui-doc-position 'at-point))

;;(use-package lsp-mode
;;  :defer t
;;  :commands lsp
;;  :custom
;;  (lsp-auto-guess-root nil)
;;  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
;;  (lsp-file-watch-threshold 2000)
;;  (read-process-output-max (* 1024 1024))
;;  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
;;  :hook ((java-mode python-mode go-mode
;;          js-mode js2-mode typescript-mode web-mode
;;          c-mode c++-mode objc-mode) . lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package consult-lsp
  :after (lsp-mode consult)
  :bind (:map lsp-mode-map
         ([remap xref-find-apropos] . consult-lsp-symbols)))

;;(use-package company-lsp
;;  :defer t
;;  :custom (company-lsp-cache-candidates 'auto))
;;
;;(use-package lsp-ivy
;;  :defer t)

(use-package lsp-eslint
  :demand t
  :after lsp-mode)

(use-package lsp-tailwindcss
  :ensure (:type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is from memacs-d, but look at
;; other gdb based support / inspection windows
;;; (use-package dap-mode
;;;   :diminish
;;;   :bind
;;;   (:map dap-mode-map
;;;         (("<f12>" . dap-debug)
;;;          ("<f8>" . dap-continue)
;;;          ("<f9>" . dap-next)
;;;          ("<M-f11>" . dap-step-in)
;;;          ("C-M-<f11>" . dap-step-out)
;;;          ("<f7>" . dap-breakpoint-toggle)))
;;;   :hook ((after-init . dap-mode)
;;;          (dap-mode . dap-ui-mode)
;;;          (python-mode . (lambda () (require 'dap-python)))
;;;          (ruby-mode . (lambda () (require 'dap-ruby)))
;;;          (go-mode . (lambda () (require 'dap-go)))
;;;          (java-mode . (lambda () (require 'dap-java)))
;;;          ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
;;;          (php-mode . (lambda () (require 'dap-php)))
;;;          (elixir-mode . (lambda () (require 'dap-elixir)))
;;;          ((js-mode js2-mode typescript-mode) . (lambda () (require 'dap-chrome)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-lsp)
;;; init-lsp.el ends here
