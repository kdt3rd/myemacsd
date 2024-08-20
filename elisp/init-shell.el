;;; init-shell.el --- Initialize shell modes -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature eshell
  :bind ("C-x m " . eshell)
  :hook
  (eshell-pre-command . eshell-save-some-history)
  (eshell-mode-hook . (lambda () (setenv "TERM" "xterm-256color")))
  :custom
  (eshell-directory-name (expand-file-name "eshell" save-dir))
  :config
  (setenv "PAGER" "cat"))

(use-package eshell-z
  :hook (eshell-mode . (lambda () (require 'eshell-z))))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package xterm-color
  :after esh-mode
  :hook
  (eshell-before-prompt . (lambda ()
                            (setq xterm-color-preserve-properties t)))
  :config
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)
;;  (setq comint-output-filter-functions
;;        (remove 'ansi-color-process-output comint-output-filter-functions))
;;  (add-hook 'shell-mode-hook
;;            (lambda ()
;;              ;; Disable font-locking in this buffer to improve performance
;;              (font-lock-mode -1)
;;              ;; Prevent font-locking from being re-enabled in this buffer
;;              (make-local-variable 'font-lock-function)
;;              (setq font-lock-function (lambda (_) nil))
;;              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (setenv "TERM" "xterm-256color"))

;;(use-package eterm-256color
;;  :straight t
;;  :config
;;  (add-hook 'term-mode-hook #'eterm-256color-mode)
;;  )

;;(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
;;  (if (memq (process-status proc) '(signal exit))
;;      (let ((buffer (process-buffer proc)))
;;        ad-do-it
;;        (kill-buffer buffer))
;;    ad-do-it))
;;(ad-activate 'term-sentinel)

;; terminal modes...
;;(use-package multi-term)
;;(global-set-key (kbd "C-c t") 'multi-term)
;;(setq multi-term-program-switches "--login")
;;  (defun my:term-paste (&optional string)
;;    (interactive)
;;    (process-send-string
;;     (get-buffer-process (current-buffer))
;;     (if string string (current-kill 0))))
;;
;;  (add-hook 'term-mode-hook
;;            (lambda ()
;;              (goto-address-mode)
;;              (define-key term-raw-map (kbd "C-y") 'my:term-paste)
;;              (define-key term-raw-map (kbd "<mouse-2>") 'my:term-paste)
;;              (define-key term-raw-map (kbd "M-o") 'other-window)
;;              (setq yas-dont-activate t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-shell)
;;; init-shell.el ends here
