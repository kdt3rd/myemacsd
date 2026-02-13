;;; -*- lexical-binding: t; -*-

;;; init-cc.el --- Initialize cc editor modes

;;; (load "cpp-skeletons")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clang-format
  :config
  (setq-default clang-format-style "{BasedOnStyle: llvm}")
  ;;(defun my:register-clang-format ()
  ;;  (make-variable-buffer-local 'before-save-hook)
  ;;  (add-hook 'before-save-hook 'clang-format-buffer))
  ;;(add-hook 'c-mode-hook 'my:register-clang-format)
  ;;(add-hook 'c++-mode-hook 'my:register-clang-format)
  ;;(defun my:disable-clang-format ()
  ;;  (interactive)
  ;;  (remove-hook 'before-save-hook 'clang-format-buffer))
  (setq clang-format-executable *clang-format*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my-home-c-style
  '(
    (c-comment-only-line-offset . 0)
    (c-syntactic-indentation-in-macros . t)
    ;; Make it so a newline isn't inserted ever - only reindent
;    (c-hanging-semi&comma-criteria . ((lambda () 'stop)))
;   (c-hanging-semi&comma-criteria . (cons 'c-semi&comma-no-newlines-for-oneline-inliners
;                                          (cons 'c-semi&comma-no-newlines-before-nonblanks
;                                                c-hanging-semi&comma-criteria)))
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

(defconst my-c-or-c++-mode--regexp
  (eval-when-compile
    (let ((id "[a-zA-Z_][a-zA-Z0-9_]*") (ws "[ \t]+") (ws-maybe "[ \t]*")
          (headers '("memory" "cstdint" "string" "string_view" "iostream" "map" "unordered_map"
                     "set" "unordered_set" "vector" "tuple")))
      (concat "^" ws-maybe "\\(?:"
                    "using"     ws "\\(?:namespace" ws
                                     "\\|" id "::"
                                     "\\|" id ws-maybe "=\\)"
              "\\|" "\\(?:inline" ws "\\)?namespace"
                    "\\(?:" ws "\\(?:" id "::\\)*" id "\\)?" ws-maybe "{"
              "\\|" "class"     ws id
                    "\\(?:" ws "final" "\\)?" ws-maybe "[:{;\n]"
              "\\|" "struct"     ws id "\\(?:" ws "final" ws-maybe "[:{\n]"
                                         "\\|" ws-maybe ":\\)"
              "\\|" "template"  ws-maybe "<.*?>"
              "\\|" "#include"  ws-maybe "<" (regexp-opt headers) ">"
              "\\|" "(public|private|protected):"   ws-maybe "\n"
              "\\)")))
  "A regexp applied to C header files to check if they are really C++.")

;;;###autoload
(defun my-c-or-c++-mode ()
  "Analyze buffer and enable either C or C++ mode.

Some people and projects use .h extension for C++ header files
which is also the one used for C header files.  This makes
matching on file name insufficient for detecting major mode that
should be used.

This function attempts to use file contents to determine whether
the code is C or C++ and based on that chooses whether to enable
`c-mode' or `c++-mode'."
  (interactive)
  (let ((mode
	 (if (save-excursion
	       (save-restriction
		 (save-match-data
		   (widen)
		   (goto-char (point-min))
		   (re-search-forward my-c-or-c++-mode--regexp
				      (+ (point) c-guess-region-max) t))))
	     'c++-mode
	   'c-mode)))
    (funcall (if (fboundp 'major-mode-remap)
		 (major-mode-remap mode)
	       mode))))

(use-feature cc-mode
  :config
  (c-add-style "home" my-home-c-style nil)
  (setq c-default-style '((c-mode . "home")
                          (c++-mode . "home")
                          ))
  (defun my-c-indent-complete()
    (interactive)
    (let (( p (point)))
      (c-indent-line-or-region)
      (when (= p (point))
        (call-interactively 'complete-symbol))))

  (defun my-c-mode-common-hook()
    (load "indent")
    (c-toggle-auto-hungry-state 1)
    (setq c-electric-pound-behavior (quote (alignleft)))
    (infer-tabs-style)

    ;;  (set (make-local-variable 'compile-command)
    ;;     (let ((tmpdir (find-source-root)))
    ;;       (if tmpdir 
    ;;           (concat "cd " tmpdir "; make")
    ;;           (concat "make " (file-name-sans-extension (buffer-file-name))))))

    ;;(define-key c-mode-base-map "\C-m" 'reindent-then-newline-and-indent)
    (local-set-key (kbd "<return>") 'c-context-line-break)
    ;;(local-set-key (kbd "C-.") 'bounce-sexp)
    (local-set-key (kbd "<tab>") 'my-c-indent-complete)
    ;;(define-key c++-mode-map (kbd "TAB") 'my-c-indent-complete)
    ;;(define-key c-mode-map (kbd "TAB") 'my-c-indent-complete)

    ;; (define-key c++-mode-map [tab] 'indent-or-expand)
    (setq c-auto-align-backslashes t)
    (setq comment-multi-line t)

    ;;(let ((root (ignore-errors (projectile-project-root))))
    ;;  (when root
    ;;    (let ((ci (make-variable-buffer-local 'flycheck-clang-include-path))
    ;;          (gi (make-variable-buffer-local 'flycheck-gcc-include-path)))
    ;;      (mapcar (lambda (p)
    ;;                (add-to-list ci (expand-file-name p root))
    ;;                (add-to-list gi (expand-file-name p root))
    ;;                )
    ;;              (list
    ;;               "build.release/_deps/deflate-src/lib"
    ;;               "build.release/_deps/deflate-src"
    ;;               "build.release/_deps/imath-build/config"
    ;;               "build.release/_deps/imath-build/config"
    ;;               "build.release/_deps/imath-src/src/Imath"
    ;;               "build.release/cmake"
    ;;               "src/lib/OpenEXRCore"
    ;;               "src/lib/OpenEXR"
    ;;               "src/lib")))))

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
    ;; Set the various swap files
    (let ((inc-dir-list '("."
                          "/usr/include"
                          "/usr/include/c++/*"
                          "/usr/local/include/*"
                          )))
    ;;   (if (getenv "SRCDIR")
    ;;       (append inc-dir-list '("$SRCDIR"
    ;;                              "$SRCDIR/Core"
    ;;                              "$SRCDIR/UserInterface"))
    ;;     (append inc-dir-list '("~/Development/Zion"
    ;;                            "~/Development/Zion/Core"
    ;;                            "~/Development/Zion/UserInterface"))
    ;;     )
    ;;   (append inc-dir-list '("~/Development/Source"
    ;;                          "~/Development/Source/lib"))
      (setq ff-search-directories inc-dir-list)
      ))
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  (add-to-list 'auto-mode-alist '("\\.\\(cpp2\\|h2\\)\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . my-c-or-c++-mode))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))
  :after (flycheck-aspell)
;;  :mode
;;  (("\\.C$"   . c++-mode)
;;   ("\\.C\\.[Oo][Ll][Dd]$"  . c++-mode)
;;   ("\\.[Cc][Cc]$"  . c++-mode)
;;   ("\\.[Cc][Cc]\\.[Oo][Ll][Dd]$"  . c++-mode)
;;   ("\\.[Cc][Cc]\\.bak$"  . c++-mode)
;;   ("\\.cpp$" . c++-mode)
;;   ("\\.cpp2$" . c++-mode)
;;   ("\\.cpp\\.[Oo][Ll][Dd]$"  . c++-mode)
;;   ("\\.cpp\\.bak$"  . c++-mode)
;;   ("\\.cxx$" . c++-mode)
;;   ("\\.tcc$" . c++-mode)
;;   ("\\.lem$" . c++-mode)
;;   ("\\.re$" . c++-mode)
;;   ("\\.cxx$" . c++-mode)
;;   ("\\.hxx$" . c++-mode)
;;   ("\\.h$"   . c++-mode)
;;   ("\\.h2$"   . c++-mode)
;;   ("\\.hpp$"   . c++-mode)
;;   ("\\.hh$"  . c++-mode)
;;   ("\\.idl$" . c++-mode)
;;   ("\\.cu$"   . c++-mode)
;;   ("\\.c$"   . c-mode)
;;   ("\\.mm$"   . objc-mode)
;;   )
  )

(provide 'init-cc)
;;; init-cc.el ends here
