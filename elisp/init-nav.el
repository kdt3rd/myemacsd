;;; init-nav.el --- Bits for navigation -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package avy
;;  :custom
;;  (avy-all-windows nil)
;;  (avy-all-windows-alt t)
;;  (avy-timeout-seconds 0.3)
;;  :config
;;  ;; https://karthinks.com/software/avy-can-do-anything/#avy-plus-embark-any-action-anywhere
;;  (defun avy-action-embark (pt)
;;    (unwind-protect
;;        (save-excursion
;;          (goto-char pt)
;;          (embark-act))
;;      (select-window
;;       (cdr (ring-ref avy-ring 0))))
;;    t)
;;  (add-to-list 'avy-dispatch-alist '(111 . avy-action-embark))
;;  (defun avy-copy-as-kill ()
;;    (interactive)
;;    (avy-goto-char-timer)
;;    (let ((beg (point)))
;;      (avy-goto-char-timer)
;;      (copy-region-as-kill beg (point))))
;;  (defun avy-kill-in-line ()
;;    (interactive)
;;    (avy-goto-char-timer)
;;    (let ((beg (point)))
;;      (call-interactively 'avy-goto-char-in-line)
;;      (copy-region-as-kill beg (point))))
;;  :bind
;;  ("C-'" . avy-goto-char-timer)
;;  ("C-;" . avy-goto-char-in-line)
;;  ("C-c C-'" . avy-copy-as-kill)
;;  ("C-c C-;" . avy-copy-as-kill-in-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package dumb-jump
;;  :defer 5
;;  :custom (dumb-jump-force-searcher 'rg)
;;  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'init-nav)
;;; init-nav.el ends here
