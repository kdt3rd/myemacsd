
(global-eldoc-mode -1)
(require 'cc-mode)
;(require 'cmake-mode)
;(require 'paren)
;(require 'find-file)
;(autoload 'javascript-mode "javascript" "major mode for editing js / json files" t)

(show-paren-mode t)
(setq show-paren-delay 0.0)

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

(use-package python-mode
  :load-path (lambda () (expand-file-name "site-elisp/python-mode" user-emacs-directory))
  :mode (("\\.py\\'" . python-mode))
  :bind (:map python-mode-map
              ("\C-backspace" . backward-kill-word))
  :config
  (setq indent-tabs-mode nil)
)

;;(use-package elpy)
;;(elpy-enable)

;; format according to pep8
;;  (use-package py-autopep8)
;;  (require 'py-autopep8)
;;  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;;
;; jedi + company
;;
;;  (use-package company-jedi)
;;  (add-to-list 'company-backends 'company-jedi)
;;  (add-hook 'python-mode-hook 'jedi:setup)
;;  (setq jedi:complete-on-dot t)

(use-package yaml-mode
  :load-path (lambda () (expand-file-name "site-elisp/yaml-mode" user-emacs-directory))
  :mode (("\\.yaml\\'" . yaml-mode)
      ("\\.yml\\'" . yaml-mode))
  :bind (:map yaml-mode-map
              ("\C-m" . newline-and-indent))
  :config
  (defun my-yaml-mode-hook ()
    (auto-fill-mode 0))
  (add-hook 'yaml-mode-hook 'my-yaml-mode-hook)
  )

(use-package json-mode
  :mode (("\\.json\\'" . json-mode))
  :bind (:map json-mode-map
              ("\C-m" . newline-and-indent))
  )

;; can't always have org mode, assume github markdown
(use-package markdown-mode
  :commands gfm-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  ;; use pandoc to format???
  ;;(setq markdown-command "pandoc --standalone --mathjax --from=markdown")
  (custom-set-faces
   '(markdown-code-face ((t nil))))
  )

;; HRM, ninja-mode is just in the ninja repository, not a separate one
;; so periodically one must check github.com/martine/ninja.git and see
;; if there is an update to the file and put it in site-elisp manually
(autoload 'ninja-mode "ninja-mode" "Major mode for ninja build" t)

;; likewise, cmake-mode.el is in the cmake repo
;; https://gitlab.kitware.com/cmake/cmake.git
;; in the Auxiliary folder
(autoload 'cmake-mode "cmake-mode" "Major mode for editing cmake files" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; more config info on web-mode.org
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustach\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.css?\\'" . web-mode)
         ("\\.scss?\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
    '(("php" . "\\.phtml\\'")
      ("blade" . "\\.blade\\."))))

;; Set up for po mode
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; gnuplot mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

(eval-when-compile
  (require 'text-mode)
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
  (infer-tabs-style)

;  (set (make-local-variable 'compile-command)
;	   (let ((tmpdir (find-source-root)))
;		 (if tmpdir 
;			 (concat "cd " tmpdir "; make")
;			 (concat "make " (file-name-sans-extension (buffer-file-name))))))

  ;(define-key c-mode-base-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)

;  (define-key c-mode-base-map [tab] 'indent-or-expand)

;  (define-key c++-mode-map [tab] 'indent-or-expand)
  (setq c-auto-align-backslashes t)
;;; Set the various swap files
  (setq ff-other-file-alist
		'(("\\.cpp\\'" (".h" ".hpp" ".ipp" ".tpp"))
		  ("\\.h\\'" (".cpp" ".cc" ".c" ".C"))
		  ("\\.hh\\'" (".cpp" ".cc" ".c" ".C"))
		  ("\\.c\\'" (".h"))
		  ("\\.C\\'" (".h"))
		  ("\\.cc\\'" (".h" ".hh"))
          ("\\.ipp\\'" (".hpp" ".cpp" ".tpp"))
          ("\\.hpp\\'" (".ipp" ".cpp" ".tpp"))
          ("\\.tpp\\'" (".hpp" ".cpp" ".ipp"))
          ("\\.ixx\\'" (".hxx" ".cxx"))
          ("\\.hxx\\'" (".ixx" ".cxx"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; turn on autofill when will be editing text
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'gfm-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'perl-mode-hook 'my-perl-mode-hook)

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
                ("\\.md\\.html$"   . markdown-mode)
                ("\\.pl$" . perl-mode)
                ("\\.pm$" . perl-mode)
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
