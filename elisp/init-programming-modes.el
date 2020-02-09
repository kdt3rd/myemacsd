
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

;; org mode settings
(use-package org
  :ensure org-plus-contrib
  :bind
  ("C-c d" . my:dashboard)
  (:map org-mode-map
        ("C-c C-x C-s" . my:mark-done-and-archive))
  :config
  (require 'org-tempo)
  (setq org-ellipsis "⤵") ; instead of ... when data under header
  (setq org-src-fontify-natively t) ; native syntax highlighting within source blocks
  (setq org-src-tab-acts-natively t) ; tab acts as if it were issued in a buffer w/ the language major mode
  (setq org-src-window-setup 'current-window) ; when editing a snippet, use current window instead of separate
  ;; quickly insert a block of the appropriate type
  (add-to-list 'org-structure-template-alist
               '("el" . "src emacs-lisp"))
  ;; task management
  (setq org-directory "~/Documents/org")
  (defun org-file-path (filename)
    "Return the absolute address of an org file, given its relative name."
    (concat (file-name-as-directory org-directory) filename))
  (setq org-index-file (org-file-path "index.org"))
  (setq org-archive-location
        (concat (org-file-path "archive.org") "::* From %s"))
  ;; where to pull tasks / events from...
  (setq org-agenda-files (list org-index-file
                               (org-file-path "recurring-events.org")
                               (org-file-path "work.org")))
  (defun my:mark-done-and-archive ()
    "Mark the state of an org-mode item as DONE and archive it."
    (interactive)
    (org-todo 'done)
    (org-archive-subtree))
  (setq org-log-done 'time) ; record time a todo was archived
  ; make sure dependencies are done before finishing current
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  ; two weeks of agenda
  (setq org-agenda-span 14)
  (setq org-agenda-start-on-weekday nil)
  ; not too many task sources, limit the prefix to keep it cleaner
  (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                   (todo . " %i ")
                                   (tags . " %i ")
                                   (search . " %i ")))
  ;; filter priority items and display
  ;; check out Aaron Bieber's agenda config and Harry Schwartz org mode config
  (require 'org-habit)

  (defun my:org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

  PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

  (defun my:org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
          subtree-end
        nil)))

  (setq org-agenda-custom-commands
        '(("p" "Personal agenda"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if
                                               'todo '("DONE" "PENDING" "BLOCKED")))
                   (org-agenda-overriding-header "Today's high-priority tasks:")))
            (agenda "")
            (todo "TODO"
                  ((org-agenda-skip-function '(or (my:org-skip-subtree-if-priority ?A)
                                                  (my:org-skip-subtree-if-habit)))
                   (org-agenda-overriding-header "Other tasks:")))
            (todo "PENDING"
                  ((org-agenda-skip-function '(my:org-skip-subtree-if-priority ?A))
                   (org-agenda-overriding-header "Waiting to hear about these:")))))))
  (defun my:dashboard ()
    (interactive)
    ;(my:copy-tasks-from-inbox)
    (org-agenda nil "p"))

  ;;;; export modes
  (require 'ox-md)
  (require 'ox-beamer)
  (use-package ox-reveal
    :config
    (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

  ;; enable bable to handle a few langs
  (use-package gnuplot)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;(ruby . t)
     (dot . t)
     (gnuplot . t)))
  ;; skip prompt on evaluating code blocks
  (setq org-confirm-babel-evaluate nil)
  (use-package htmlize)
  ;; misc export options
  (setq org-export-with-smart-quotes t)
  ;; skip footer?
  ;;(setq org-html-postamble nil)

  ;; enable pdfs with syntax highlighting using minted but that involves shelling out
  ;; which we need to enable
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  )

(use-package graphviz-dot-mode
  :config
  (org-babel-do-load-languages
   'org-babel-do-load-languages
   '((dot . t)))
  )

;; TeX mode touch-ups
(setq TeX-parse-self t)
;; does anyone need DVI any more?
(setq TeX-PDF-mode t)
;; math hooks...
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-math-mode)
            (setq TeX-master t)))

;;(setq initial-major-mode 'org-mode)
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
