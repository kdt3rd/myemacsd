
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ein (jupyter) settings
(use-package ein
  :ensure t
  :bind (("C-c I l" . ein:notebooklist-login)
         ("C-c I o" . ein:notebooklist-open))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-ein)
;; init-ein.el ends here

