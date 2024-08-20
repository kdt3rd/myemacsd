;;; init-misc.el --- Initialize misc editor modes -*- lexical-binding: t -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cmake-font-lock
  :commands cmake-font-lock-activate)
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :config
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature css-mode
  :custom
  (css-indent-offset 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package csv-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package json-mode
  :mode ("\\.json\\'")
  ;;:bind (:map json-mode-map
  ;;            ("\C-m" . newline-and-indent))
  )

(use-package json-reformat
  :after json-mode
  :config
  (setq json-reformat:indent-width 2))

(use-package jq-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; lua-mode site-lisp configuration
(use-package lua-mode
  :mode ("\\.lua\\'" "construct\\'")
  :config
  (setq lua-indent-level 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature makefile-mode
  :hook
  (makefile-mode . (lambda () (setq indent-tabs-mode t)))
  :mode ("Makefile\\'" "GNUMakefile\\'")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ninja-mode
  :mode (".ninja\\'")
  )
;;;; HRM, ninja-mode is just in the ninja repository, not a separate one
;;;; so periodically one must check github.com/martine/ninja.git and see
;;;; if there is an update to the file and put it in site-elisp manually
;;(autoload 'ninja-mode "ninja-mode" "Major mode for ninja build" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; can't always have org mode, assume github markdown
(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :config
  ;; use pandoc to format???
  ;;(setq markdown-command "pandoc --standalone --mathjax --from=markdown")
  (font-lock-add-keywords 'markdown-mode
                          '(("{{[^}]+}}" . 'font-lock-function-name-face)))
  :hook
  (markdown-mode . (lambda ()
                     (auto-fill-mode t)
                     ;;(flyspell-mode t)
                     ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature octave-mode
  :mode ("\\.m\\'")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature perl-mode
  :bind (:map perl-mode-map
              ("\C-m" . reindent-then-newline-and-indent))
  :mode ("\\.pl\\'" "\\.pm\\'")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature po-mode
  :mode ("\\.po\\'")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python-mode
  ;:load-path (lambda () (expand-file-name "site-elisp/python-mode" user-emacs-directory))
  :mode (("\\.py\\'" . python-mode))
  :bind (:map python-mode-map
              ("\C-backspace" . backward-kill-word))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature text-mode
  :mode ("\\.txt\\'")
  :hook
  (text-mode . (lambda () (auto-fill-mode t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :custom
  (web-mode-enable-auto-pairing nil)
  :config
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>"))
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.css?\\'" . web-mode)
         ("\\.scss?\\'" . web-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :mode ("\\.yaml\\'")
  :hook
  (yaml-mode . (lambda ()
                 (whitespace-mode t)
                 (subword-mode t))))

(provide 'init-misc)
;;; init-misc.el ends here
