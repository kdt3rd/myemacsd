
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

  ;; skip prompt on evaluating code blocks
  (setq org-confirm-babel-evaluate nil)
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

;; enable bable to handle a few langs
(use-package graphviz-dot-mode
(use-package gnuplot)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (dot . t)
   (gnuplot . t)))

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

(use-package htmlize :defer t)
(use-package ox-gfm :defer t)
(use-package ox-reveal
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-org)
;; init-org.el ends here
