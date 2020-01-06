
(global-eldoc-mode -1)
(require 'cc-mode)
;(require 'cmake-mode)
;(require 'paren)
;(require 'find-file)
;(autoload 'javascript-mode "javascript" "major mode for editing js / json files" t)

;;; lua-mode site-lisp configuration
(use-package lua-mode
  :load-path (lambda () (expand-file-name "site-elisp/lua-mode" user-emacs-directory))
  :mode (("\\.lua\\'" . lua-mode)
      ("construct\\'" . lua-mode))
  :config
  (setq indent-tabs-mode t)
  (setq lua-indent-level 4)
  :custom
  (lua-default-application "/usr/bin/lua")
  )

(use-package yaml-mode
  :load-path (lambda () (expand-file-name "site-elisp/yaml-mode" user-emacs-directory))
  :mode (("\\.yaml\\'" . yaml-mode)
      ("\\.yml\\'" . yaml-mode))
  :bind (:map yaml-mode-map
              ("\\C-m" . newline-and-indent))
  :config
  (defun my-yaml-mode-hook ()
    (auto-fill-mode 0))
  (add-hook 'yaml-mode-hook 'my-yaml-mode-hook)
  )

(use-package json-mode
  :mode (("\\.json\\'" . json-mode))
  :bind (:map json-mode-map
              ("\\C-m" . newline-and-indent))
  )
  
;; Set up for po mode
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; gnuplot mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; Set up for ninja mode
(autoload 'ninja-mode "ninja-mode" "Major mode for ninja build" t)

;; Set up for ninja mode
(autoload 'markdown-mode "markdown-mode" "Major mode for markdown files" t)

(eval-when-compile
  (require 'lua-mode)
  (require 'web-mode)
  (require 'python-mode)
  (require 'perl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (require 'dabbrev)
;;; (load "cpp-skeletons")

;;; ;; control automatic var dabbrev stuff
;;; (setq dabbrev-always-check-other-buffers t)
;;; (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-home-c-style
  '((c-tab-always-indent . nil)
    (c-comment-only-line-offset . 0)
	(c-syntactic-indentation-in-macros . t)
	;; Make it so a newline isn't inserted ever - only reindent
	(c-hanging-semi&comma-criteria . ((lambda () 'stop)))
;	(c-hanging-semi&comma-criteria . (cons 'c-semi&comma-no-newlines-for-oneline-inliners
;										   (cons 'c-semi&comma-no-newlines-before-nonblanks
;												 c-hanging-semi&comma-criteria)))
	(c-hanging-braces-alist . ((block-close . c-snug-do-while)
							   (namespace-open)
							   (class-close)
							   (inline-open)
							   (inline-close)
							   ))
	(c-auto-newline . t)
    (c-offsets-alist . ((arglist-close . c-lineup-arglist )
                        (substatement-open . 0)
                        (case-label . +)
                        (extern-lang-open . 0)
						(inline-open . 0)
                        (inextern-lang . 0)
						(innamespace . 0)
						(cpp-macro . 0)
						(cpp-macro-cont . 0)
						(member-init-intro . +)
                        (block-open . 0)))
    )
  "My C Programming Style")

(c-add-style "Home" my-home-c-style nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-c-mode-common-hook()
  (load "indent")
  (c-set-style "Home")
;  (if (or (string-equal "C" (file-name-extension (buffer-file-name)))
;	  (string-match "src/houdini" (buffer-file-name)))
;      (setq tab-width 8))
  (setq indent-tabs-mode nil)
  (c-toggle-auto-hungry-state 1)
  (setq c-electric-pound-behavior (quote (alignleft)))
  (show-paren-mode t)
  (infer-tabs-style)

;  (set (make-local-variable 'compile-command)
;	   (let ((tmpdir (find-source-root)))
;		 (if tmpdir 
;			 (concat "cd " tmpdir "; make")
;			 (concat "make " (file-name-sans-extension (buffer-file-name))))))

  ;(define-key c-mode-base-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)

  (define-key c-mode-base-map [tab] 'indent-or-expand)

;  (define-key c++-mode-map [tab] 'indent-or-expand)
  (setq c-auto-align-backslashes t)
;;; Set the various swap files
  (setq ff-other-file-alist
		'(("\\.cpp$" (".h"))
		  ("\\.h$" (".cpp" ".cc" ".c" ".C"))
		  ("\\.c$" (".h"))
		  ("\\.C$" (".h"))
		  ("\\.cc$" (".h"))
		  ("\\.tcc$" (".h"))))
  (let ((inc-dir-list '("."
						"/usr/include"
						"/usr/include/c++/*"
						"/usr/local/include/*"
						)))
;	(if (getenv "SRCDIR") 
;		(append inc-dir-list '("$SRCDIR" 
;							   "$SRCDIR/Core"
;							   "$SRCDIR/UserInterface"))
;	  (append inc-dir-list '("~/Development/Zion" 
;							 "~/Development/Zion/Core"
;							 "~/Development/Zion/UserInterface"))
;	  )
;	(append inc-dir-list '("~/Development/Source" 
;						   "~/Development/Source/lib"))
	(setq ff-search-directories inc-dir-list)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-perl-mode-hook()
  (define-key perl-mode-map "\C-m" 'reindent-then-newline-and-indent)
)

(defun my-text-mode-hook()
  (auto-fill-mode 1)
)

(defun my-python-mode-hook()
  (setq indent-tabs-mode nil)
  (define-key python-mode-map (kbd "<C-backspace>") 'backward-kill-word)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook 'my-text-mode-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'perl-mode-hook 'my-perl-mode-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Go into various modes depending on file extention
(setq auto-mode-alist
      (append '(("\\.C$"   . c++-mode)
                ("\\.C\\.[Oo][Ll][Dd]$"  . c++-mode)
                ("\\.[Cc][Cc]$"  . c++-mode)
                ("\\.[Cc][Cc]\\.[Oo][Ll][Dd]$"  . c++-mode)
                ("\\.[Cc][Cc]\\.bak$"  . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.cpp\\.[Oo][Ll][Dd]$"  . c++-mode)
                ("\\.cpp\\.bak$"  . c++-mode)
                ("\\.cxx$" . c++-mode)
                ("\\.tcc$" . c++-mode)
                ("\\.lem$" . c++-mode)
                ("\\.re$" . c++-mode)
                ("\\.cxx$" . c++-mode)
                ("\\.gp$"   . gnuplot-mode)
                ("\\.hxx$" . c++-mode)
                ("\\.h$"   . c++-mode)
                ("\\.hh$"  . c++-mode)
                ("\\.idl$" . c++-mode)
                ("\\.cu$"   . c++-mode)
                ("\\.c$"   . c-mode)
                ("\\.m$"   . octave-mode)
                ("\\.mm$"   . objc-mode)
                ("\\.md$"   . markdown-mode)
                ("\\.md\\.html$"   . markdown-mode)
                ("\\.pl$" . perl-mode)
                ("\\.pm$" . perl-mode)
                ("\\.py$" . python-mode)
                ("\\.ninja$" . ninja-mode)
                ("\\.html$" . web-mode)
                ("\\.js$" . javascript-mode)
                ("\\.json$" . javascript-mode)
                ("Makefile" . makefile-mode)
                ("CMakeLists.txt" . cmake-mode)
                ("\\.cmake$" . cmake-mode)
                ("\\.po\\'\\|\\.po\\." . po-mode)
                ("\\.txt$" . text-mode))
              auto-mode-alist
			  ))

(provide 'init-programming-modes)
