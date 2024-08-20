;;; init-smartparens.el --- Initialize misc editor modes -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package paredit
;;  :commands (paredit-semicolon
;;             paredit-comment-dwim
;;             paredit-close-round
;;             paredit-close-square
;;             paredit-close-curly))

(use-package smartparens
  :diminish
  :ensure (:type git :host github :repo "Fuco1/smartparens")
  ;;:hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks

  ;;:custom
  ;;(sp-base-key-bindings 'paredit)
  ;;(sp-autoskip-closing-pair 'always)
  ;;(sp-hybrid-kill-entire-symbol t)
  ;;(sp-hybrid-kill-excessive-whitespace nil)

  ;;:hook (elpaca-after-init . (lambda ()
  ;;                             (smartparens-global-strict-mode)
  ;;                             (show-smartparens-global-mode)
  ;;                             (setq sp-paredit-bindings (delete '("M-?" . sp-convolute-sexp) sp-paredit-bindings))
  ;;                             (require 'smartparens-config)
  ;;                             (sp-use-paredit-bindings)))

  :config
  ;;(sp-pair "\"" "\"" :wrap "M-\"")

  (require 'smartparens-config)
  ;; load default config
  (defun bounce-sexp (&optional _)
    "Will bounce between matching parens just like % in vi"
    (interactive "^p")
    ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
    (cond ((looking-at "[[({]") (forward-sexp))
          ((looking-back "[])}]" 1) (backward-sexp))
          ;; now, try to succeed from inside of a bracket
          ((looking-at "[])}]") (forward-char) (backward-sexp))
          ((looking-back "[[({]" 1) (backward-char) (forward-sexp))
          (t (sp-forward-sexp))))
  (add-hook 'eval-expression-minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  (sp-use-smartparens-bindings)
  (define-key smartparens-mode-map (kbd "C-.") 'bounce-sexp)
  (sp-with-modes '(c++-mode c-mode)
                 (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
                 (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                      ("* ||\n[i]" "RET"))))
  (add-hook 'prog-mode 'smartparens-mode)
  )

(provide 'init-smartparens)
;;; init-smartparens.el ends here
