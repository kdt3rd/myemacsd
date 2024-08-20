;;; init-org.el --- Org-Mode Configuration File -*- lexical-binding: t -*-
;;; Commentary:
;; Some parts copied from prelude-org.el
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable bable to handle a few langs
(use-package graphviz-dot-mode)
(use-package gnuplot
  :mode ("\\.gp\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :hook
  (elpaca-after-init . (lambda ()
                         (org-babel-do-load-languages
                          'org-babel-load-languages
                          '((shell . t)
                            (emacs-lisp . t)
                            (gnuplot . t)
                            (dot . t)
                            (C . t)
                            (sql . t)))
                         ))
  (org-mode . (lambda ()
                (turn-on-visual-line-mode)
                (electric-pair-mode -1)))
  :custom
  (org-log-done t)
  (org-special-ctrl-k t)
  (org-special-ctrl-a t)
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "DONE")))
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (define-abbrev-table 'org-mode-abbrev-table
    '(("esq" "#+BEGIN_SRC es :jq .\n\n#+END_SRC" (lambda () (forward-line -1)))))

  (defun my:dashboard ()
    (interactive)
    ;(my:copy-tasks-from-inbox)
    (org-agenda nil "p"))

  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . my:dashboard)
  ;("C-c b" . org-switchb)
  ;; TODO bindings
  ;("C-c r" . org-refile)
  ;("C-c c" . org-capture)
  )

;; Daniel Mendler (https://github.com/minad).
(use-package org-modern
  :ensure (:host github :repo "minad/org-modern")
  :after (org)
  :custom ((org-modern-star '("◉" "○" "◈" "◇" "•"))
            ;; Showing the depth of stars helps with the speed keys as well as
            ;; gives a clearer indicator of the depth of the outline.
            (org-modern-hide-stars nil))
  :config (global-org-modern-mode))

;;;; org mode settings
;;(use-package org
;;  :hook
;;  (org-mode . (lambda ()
;;                (turn-on-visual-line-mode)
;;                (electric-pair-mode -1)))
;;  :bind
;;  ("C-c d" . my:dashboard)
;;  (:map org-mode-map
;;        ("C-c C-x C-s" . my:mark-done-and-archive))
;;  :custom (org-use-speed-commands t)
;;  (org-time-stamp-rounding-minutes '(0 15))
;;  (org-clock-rounding-minutes 15)
;;  (org-src-tab-acts-natively t) ; tab acts as if it were issued in a buffer w/ the language mode
;;  :config
;;  ;; quickly insert a block of the appropriate type
;;  ;(add-to-list 'org-structure-template-alist
;;  ;             '("el" . "src emacs-lisp"))
;;  (defun org-file-path (filename)
;;    "Return the absolute address of an org file, given its relative name."
;;    (concat (file-name-as-directory org-directory) filename))
;;
;;  (defun my:mark-done-and-archive ()
;;    "Mark the state of an org-mode item as DONE and archive it."
;;    (interactive)
;;    (org-todo 'done)
;;    (org-archive-subtree))
;;
;;  (defun my:org-skip-subtree-if-priority (priority)
;;    "Skip an agenda subtree if it has a priority of PRIORITY.
;;
;;  PRIORITY may be one of the characters ?A, ?B, or ?C."
;;    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
;;          (pri-value (* 1000 (- org-lowest-priority priority)))
;;          (pri-current (org-get-priority (thing-at-point 'line t))))
;;      (if (= pri-value pri-current)
;;          subtree-end
;;        nil)))
;;
;;  (defun my:org-skip-subtree-if-habit ()
;;    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
;;    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
;;      (if (string= (org-entry-get nil "STYLE") "habit")
;;          subtree-end
;;        nil)))
;;
;;  (setq org-ellipsis "⤵" ; instead of ... when data under header
;;        org-src-fontify-natively t ; native syntax highlighting within source blocks
;;        org-src-window-setup 'current-window ; when editing a snippet, use current window instead of separate
;;        org-directory "~/Documents/org"
;;        org-index-file (org-file-path "index.org")
;;        org-archive-location (concat (org-file-path "archive.org") "::* From %s")
;;        ;; where to pull tasks / events from...
;;        org-agenda-files (list org-index-file
;;                               (org-file-path "recurring-events.org")
;;                               (org-file-path "work.org"))
;;        org-log-done 'time ; record time a todo was archived
;;        ;; make sure dependencies are done before finishing current
;;        org-enforce-todo-dependencies t
;;        org-enforce-todo-checkbox-dependencies t
;;        ;; two weeks of agenda
;;        org-agenda-span 14
;;        org-agenda-start-on-weekday nil
;;        ;; not too many task sources, limit the prefix to keep it cleaner
;;        org-agenda-prefix-format '((agenda . " %i %?-12t% s")
;;                                   (todo . " %i ")
;;                                   (tags . " %i ")
;;                                   (search . " %i "))
;;        org-confirm-babel-evaluate nil
;;        ;; skip prompt on evaluating code blocks
;;        ;; misc export options
;;        org-export-with-smart-quotes t)
;;  ;; skip footer?
;;  ;;(setq org-html-postamble nil)
;;
;;  (setq org-agenda-custom-commands
;;        '(("p" "Personal agenda"
;;           ((tags "PRIORITY=\"A\""
;;                  ((org-agenda-skip-function '(org-agenda-skip-entry-if
;;                                               'todo '("DONE" "PENDING" "BLOCKED")))
;;                   (org-agenda-overriding-header "Today's high-priority tasks:")))
;;            (agenda "")
;;            (todo "TODO"
;;                  ((org-agenda-skip-function '(or (my:org-skip-subtree-if-priority ?A)
;;                                                  (my:org-skip-subtree-if-habit)))
;;                   (org-agenda-overriding-header "Other tasks:")))
;;            (todo "PENDING"
;;                  ((org-agenda-skip-function '(my:org-skip-subtree-if-priority ?A))
;;                   (org-agenda-overriding-header "Waiting to hear about these:")))))))
;;
;;  ;; enable pdfs with syntax highlighting using minted but that involves shelling out
;;  ;; which we need to enable
;;  (setq org-latex-pdf-process
;;        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;  (add-to-list 'org-latex-packages-alist '("" "minted"))
;;  (setq org-latex-listings 'minted)
;;
;;  (org-babel-do-load-languages 'org-babel-load-languages
;;    (append org-babel-load-languages
;;      '((emacs-lisp . t)
;;         (shell . t)
;;         (plantuml . t)
;;         (c . t))))
;;  :init
;;  (require 'ox)
;;  (add-to-list 'org-latex-classes
;;    '("jf/article"
;;       "\\documentclass[11pt,a4paper]{article}
;;      \\usepackage[utf8]{inputenc}
;;      \\usepackage[T1]{fontenc}
;;      \\usepackage{fixltx2e}
;;      \\usepackage{graphicx}
;;      \\usepackage{longtable}
;;      \\usepackage{float}
;;      \\usepackage{wrapfig}
;;      \\usepackage{rotating}
;;      \\usepackage[normalem]{ulem}
;;      \\usepackage{amsmath}
;;      \\usepackage{textcomp}
;;      \\usepackage{marvosym}
;;      \\usepackage{wasysym}
;;      \\usepackage{amssymb}
;;      \\usepackage{hyperref}
;;      \\usepackage{mathpazo}
;;      \\usepackage{xcolor}
;;      \\usepackage{enumerate}
;;      \\definecolor{bg}{rgb}{0.95,0.95,0.95}
;;      \\tolerance=1000
;;      [NO-DEFAULT-PACKAGES]
;;      [PACKAGES]
;;      [EXTRA]
;;
;;      \\linespread{1.1}
;;      \\hypersetup{pdfborder=0 0 0}"
;;       ("\\section{%s}" . "\\section*{%s}")
;;       ("\\subsection{%s}" . "\\subsection*{%s}")
;;       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;       ("\\paragraph{%s}" . "\\paragraph*{%s}")))
;;  )
;;
;;;; check out Aaron Bieber's agenda config and Harry Schwartz org mode config
;;(use-package org-habit
;;  :straight t
;;  :after (org))
;;(use-package org-tempo
;;  :straight t
;;  :after (org))
;;
;;;; see
;;;; https://takeonrules.com/2022/02/26/note-taking-with-org-roam-and-transclusion/,
;;;; about including text from another file.
;;(use-package org-transclusion
;;  :straight t
;;  :after (org)
;;  :init (setq org-transclusion-exclude-elements '(property-drawer keyword)))
;;
;;(use-package org-appear
;;  :straight (:type git :host github :repo "awth13/org-appear")
;;  :after (org)
;;  :hook (org-mode . org-appear-mode)
;;  )
;;
;;;;(setq initial-major-mode 'org-mode)
;;(use-package org-bullets
;;  :straight t
;;  :after (org)
;;  ;:hook (org-mode . org-bullets-mode)
;;  )
;;
;;
;;(use-package htmlize :straight t)
;;
;;;;; Org Export and Composition Functionality
;;(setq org-export-global-macros (list))
;;
;;(use-package ox
;;  :straight t ;;(ox :type built-in)
;;  :after (org)
;;  :config
;;  (add-to-list 'org-export-global-macros
;;    '("kbd" . "@@html:<kbd>@@$1@@html:</kbd>@@"))
;;  (add-to-list 'org-export-global-macros
;;    '("cite" . "@@html:<cite>@@$1@@html:</cite>@@"))
;;  (add-to-list 'org-export-global-macros
;;    '("dfn" . "@@html:<dfn>@@$1@@html:</dfn>@@"))
;;  (add-to-list 'org-export-global-macros
;;    '("mark" . "@@html:<mark>@@$1@@html:</mark>@@"))
;;  (add-to-list 'org-export-global-macros
;;    '("scene-date" . "#+begin_marginnote\nThe scene occurs on @@html:<span class=\"time\">@@$1@@html:</span>@@.\n#+end_marginnote"))
;;  (add-to-list 'org-export-global-macros
;;    '("mention" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" >}}@@"))
;;  (add-to-list 'org-export-global-macros
;;    '("abbr" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" abbr=\"t\" >}}@@"))
;;  (add-to-list 'org-export-global-macros
;;    '("abbr-plural" . "@@hugo:{{< glossary key=\"@@$1@@hugo:\" abbr=\"t\" plural=\"t\" >}}@@"))
;;  (add-to-list 'org-export-global-macros
;;    '("i" . "@@html:<i class=\"dfn\">@@$1@@html:</i>@@"))
;;  (add-to-list 'org-export-global-macros
;;    '("mechanic" . "@@html:<i class=\"mechanic\">@@$1@@html:</i>@@"))
;;  (add-to-list 'org-export-global-macros
;;    '("linkToSeries" . "@@hugo:{{< linkToSeries \"@@$1@@hugo:\" >}}@@")))
;;
;;(use-package ox-md
;;  :straight t
;;  :after (ox))
;;(use-package ox-beamer
;;  :straight t
;;  :after (ox))
;;(use-package ox-gfm
;;  :straight t
;;  :after (ox))
;;(use-package ox-reveal
;;  :straight t
;;  :after (ox)
;;  :config
;;  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))


;;  (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((emacs-lisp . t)
;;     (dot . t)
;;     (gnuplot . t)))
;;  
;;  ;; TeX mode touch-ups
;;  (setq TeX-parse-self t)
;;  ;; does anyone need DVI any more?
;;  (setq TeX-PDF-mode t)
;;  ;; math hooks...
;;  (add-hook 'LaTeX-mode-hook
;;            (lambda ()
;;              (LaTeX-math-mode)
;;              (setq TeX-master t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-org)
;; init-org.el ends here
