;;; init-search.el --- Bits for searching -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature isearch
  :config
  (defface isearch-prompt
    '((t (:foreground "gold")))
    "Face for isearch minibuffer prompt."
    :group 'isearch)
  ;; https://www.emacs.dyerdwelling.family/emacs/20230503211610-emacs--isearch-occur-advice-window-focus/
  (defun isearch-occur-advice (origin &rest args)
    (isearch-exit)
    (select-window (get-buffer-window "*Occur*"))
    (goto-char (point-min)))
  (advice-add 'isearch-occur :after 'isearch-occur-advice)
  :custom
  (search-whitespace-regexp ".*\\b")
  (isearch-lax-whitespace t)
  (isearch-allow-scroll t)
  (isearch-yank-on-move 'shift)
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format "   (%s/%s)")
  (isearch-message-properties '(read-only t cursor-intangible t face isearch-prompt))
  :bind-keymap ("C-c s" . search-map) ;; M-s clashes with paredit/smartparens bindings
  :bind
  ("C-*" . isearch-forward-symbol-at-point)
  (:map search-map
        ("M-s M-<" . isearch-beginning-of-buffer)
        ("M-s M->" . isearch-end-of-buffer)
        ("C-c s M-<" . isearch-beginning-of-buffer)
        ("C-c s M->" . isearch-end-of-buffer)))

(use-package isearch-dabbrev
  :after isearch
  :bind (:map isearch-mode-map ("M-/" . isearch-dabbrev-expand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package anzu
  :diminish
  :config
  (global-anzu-mode)
  (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold)
  :custom
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-replace-threshold 100)
  (anzu-replace-to-string-separator " => ")
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp)
  (:map isearch-mode-map
        ([remap isearch-query-replace] . anzu-isearch-query-replace)
        ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

(use-package deadgrep
  :config
  (defun deadgrep-symbol-at-point ()
    (interactive)
    (deadgrep (thing-at-point 'symbol)))
  (defun deadgrep-current-directory (search-term)
    (interactive (list (deadgrep--read-search-term)))
    (deadgrep search-term (file-name-directory buffer-file-name)))
  (defvar include-all nil)
  (defun deadgrep--include-all-advice (rg-args)
    (if include-all
        (push "-uuuLz" rg-args)
      rg-args))
  (advice-add 'deadgrep--arguments :filter-return #'deadgrep--include-all-advice)
  (defun deadgrep-all (search-term)
    (interactive (list (deadgrep--read-search-term)))
    (let ((include-all t))
      (deadgrep search-term)))
  :custom (deadgrep-display-buffer-function
            (lambda (buffer) (display-buffer-same-window buffer '())))
  :bind
  ;;("C-c c d" . deadgrep)
  ;;("C-c c M-d" . deadgrep-all)
  ("C-S-z" . deadgrep-symbol-at-point)
  ;;("C-c c C-d" . deadgrep-current-directory)
  (:map search-map
        ("d" . deadgrep)
        ("M-d" . deadgrep-all)
        ("C-d" . deadgrep-current-directory)
        ("D" . deadgrep-symbol-at-point)))

;;(use-package wgrep
;;  ;; “Edit a grep buffer and apply those changes to the file buffer.”  In other
;;  ;; words, after “searching” for something, sending the results to a buffer
;;  ;; (via `embark-export' or such thing), you can edit that search results
;;  ;; buffer and propogate the changes to the locations of the elements that
;;  ;; matched the search.
;;  ;;
;;  ;;   1.  Call `consult-ripgrep' (via ~C-c f~) to search for something.
;;  ;;   2.  Call `embark-export' (via ~C-s-e~) to export to a grep buffer.
;;  ;;   3.  Call `wgrep-change-to-wgrep-mode' (via ~e~ or ~C-c C-p~)
;;  ;;   4.  Edit the grep buffer as you would anywhere else.
;;  ;;   5.  Save (via ~C-x C-s~) or Cancel (via ~C-c C-k~).
;;  :after (embark-consult ripgrep)
;;  :config (setq wgrep-auto-save-buffer t
;;                wgrep-change-readonly-file t)
;;  :bind (
;;         :map wgrep-mode-map
;;         ;; Added keybinding to echo Magit behavior
;;         ("C-c C-c" . (lambda ()
;;                        (interactive)
;;                        (wgrep-save-all-buffers)
;;                        (wgrep-finish-edit)))
;;         :map grep-mode-map
;;         ("e" . wgrep-change-to-wgrep-mode)
;;         :map ripgrep-search-mode-map
;;         ("e" . wgrep-change-to-wgrep-mode)))

(use-package rg
  ;; A highly performant successor to the venerable grep.
  :bind
  ("C-c C-M-S-r" . rg-menu)
  ("C-c C-M-r" . rg)
  ("C-z" . rg-dwim)
  ;;:config (rg-enable-menu)
  ;; :init (setq ripgrep-arguments "--ignore-case")
  (:map search-map ("s" . rg)))

;;(use-package color-rg
;;  :load-path (lambda () (expand-file-name "site-elisp/color-rg" user-emacs-directory))
;;  :if *rg*
;;  :bind ("C-M-s" . color-rg-search-input))

(use-package affe
  :config
  (setq affe-grep-command (replace-regexp-in-string "rg" "rg -Suu" affe-grep-command))
  ;; Configure Orderless
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))
  (defun my/affe-grep-symbol-at-point (&optional dir initial)
    (interactive
     (list prefix-arg (when-let ((s (symbol-at-point)))
                        (symbol-name s))))
    (affe-grep dir initial))
  (defun my/affe-find-symbol-at-point (&optional dir initial)
    (interactive
     (list prefix-arg (when-let ((s (symbol-at-point)))
                        (symbol-name s))))
    (affe-find dir initial))
  :bind
  ("C-#" . affe-grep)
  ("C-c z" . affe-find)
  ("C-c Z" . my/affe-find-symbol-at-point)
  ("C-~" . my/affe-grep-symbol-at-point)
  (:map search-map
        ("#" . affe-grep)
        ("~" . my/affe-grep-symbol-at-point)
        ("a" . affe-find)
        ("A" . my/affe-find-symbol-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-search)

