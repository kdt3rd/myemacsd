;;; init-highlight.el --- Various Highlight Settings -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(let ( ;( my-extra-types
;;		; '( "\\([UI]i*\\)nt\\(8\\|16\\|32\\|64\\)" "Index"
;;		;	"Float\\(32\\|64\\)" "sstring" "RE_\\(\\w*\\)Coord" ) )
;;	   ( my-mode-additions
;;		 '( ("\\<\\(TODO\\):" . font-lock-warning-face)
;;			("\\<\\(PROGRAMMING_ERROR\\|ASSERT\\w*\\|DEBUG_ABORT\\w*\\|BADPLACE\\w*\\)\\>" . font-lock-warning-face)
;;			("\\<\\(PRECONDITION\\|POSTCONDITION\\|CHECK_INVARIANT\\|REQUIRE\\|ENSURE\\|STATIC_CHECK\\)\\>" . font-lock-constant-face)
;;			) )
;;	   )
;;;  (setq c-font-lock-extra-types 
;;;		(append c-font-lock-extra-types my-extra-types))
;;;  (setq c++-font-lock-extra-types 
;;;		(append c++-font-lock-extra-types my-extra-types ) )
;;
;;  (font-lock-add-keywords 'c-mode my-mode-additions)
;;  (font-lock-add-keywords 'c++-mode my-mode-additions)
;;  (font-lock-add-keywords 'objc-mode my-mode-additions)
;;)

(use-package hl-todo
  :bind
  (:map hl-todo-mode-map
        ("C-c c t p" . hl-todo-previous)
        ("C-c c t n" . hl-todo-next)
        ("C-c c t o" . hl-todo-occur)
        ("C-c c t r" . hl-todo-rgrep)
        ("C-c c t i" . hl-todo-insert))
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . "red3")
     ("FIXME"  . "red3")
     ("DEBUG"  . "#A020F0")
     ("GOTCHA" . "#FF4500")
     ("HACK" . "#FF4500")
     ("STUB"   . "#1E90FF")
     ("FAIL"   . "red3")
     ("NOTE"   . "DarkOrange2")
     ("DEPRECATED" . "yellow")))
  :hook (elpaca-after-init . global-hl-todo-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature whitespace
  :diminish
  :custom
  (whitespace-line-column 120)
  (whitespace-style '(face tabs empty trailing lines-tail))
  :config
  (defun turn-off-whitespace-mode ()
    (whitespace-mode -1))
  (defvar-local whitespace-disabled-modes '(cider-repl-mode ielm-mode vterm-mode eshell-mode shell-mode term-mode ansi-term-mode))
  (dolist (mode whitespace-disabled-modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'turn-off-whitespace-mode))
  :hook
  ((text-mode prog-mode) . (lambda () (whitespace-mode +1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature paren
  :config
  (show-paren-mode t)
  (setq show-paren-delay 0))

(use-package highlight-parentheses
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda () (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))

(use-package rainbow-delimiters
  :hook
  ((text-mode prog-mode ielm-mode) . #'rainbow-delimiters-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package highlight-indent-guides
  :diminish
  :custom (highlight-indent-guides-method 'character)
  :config
  ;; TODO can we do the same with highlight-indent-guides-auto-* ?
  (setq highlight-indent-guides-auto-enabled nil)
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-highlight)

