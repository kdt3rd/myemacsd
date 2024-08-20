;;; init-complete.el --- Bits for completion -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-feature abbrev
;;  :defer 5
;;  :diminish
;;  :hook
;;  ((text-mode prog-mode) . abbrev-mode)
;;  )

(use-feature dabbrev
  :diminish
  :custom
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  ;;(dabbrev-case-replace nil)
  )
;;; ;; control automatic var dabbrev stuff
;;; (setq dabbrev-always-check-other-buffers t)
;;; (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

(use-feature hippie-expand
  :config
  (setq hippie-expand-try-functions-list
        '(;yas-hippie-try-expand
          try-expand-dabbrev
          try-expand-all-abbrevs
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))
  ;; https://www.emacswiki.org/emacs/HippieExpand#h5o-9
  (defadvice he-substitute-string (after he-paredit-fix)
    "Remove extra paren when expanding line in paredit."
    (if (and (or smartparens-mode paredit-mode) (equal (substring str -1) ")"))
        (progn (backward-delete-char 1) (forward-char))))
  :bind
  ("C-M-/" . hippie-expand))

(use-package fancy-dabbrev
  :diminish
  :config
  (global-fancy-dabbrev-mode)
  (defun fancy-dabbrev-popup-advice (_next)
    (local-set-key (kbd "C-M-/") #'fancy-dabbrev-backward))
  (defun fancy-dabbrev-popup-exit-advice ()
    (local-unset-key (kbd "C-M-/")))
  (advice-add #'fancy-dabbrev--expand-again :before #'fancy-dabbrev-popup-advice)
  (advice-add #'fancy-dabbrev--on-exit :after #'fancy-dabbrev-popup-exit-advice)
  :bind ("M-/" . fancy-dabbrev-expand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature subword
  :diminish)

(use-feature xref
  :custom (xref-search-program 'ripgrep)
  :config
  (defun xref-find-references-other-window (identifier)
    "Like `xref-find-references' but switch to the other window"
    (interactive (list (xref--read-identifier "Find references of: ")))
      (xref--find-xrefs identifier 'references identifier 'window))
  (defun xref-find-references-other-frame (identifier)
    "Like `xref-find-references' but switch to the other frame"
    (interactive (list (xref--read-identifier "Find references of: ")))
    (xref--find-xrefs identifier 'references identifier 'frame))
  (define-key ctl-x-4-map (kbd "M-?") 'xref-find-references-other-window)
  (define-key ctl-x-5-map (kbd "M-?") 'xref-find-references-other-window)
  ;; 'xref-prompt-for-identifier begins with not, so adding this prevents
  ;; prompting for an identifier when calling xref-find-references, unless
  ;; there is no value at point that can be used
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references-other-window t)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references-other-frame t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature emacs
  :config
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (format "[%s %s] %s"
                  (propertize "CRM" 'face 'error)
                  (propertize
                   (replace-regexp-in-string
                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                    crm-separator)
                   'face 'success)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq completion-cycle-threshold 3
        tab-always-indent 'complete
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package consult
  ;; Extensions for the numerous `completing-read' functions.  Highly extensible
  ;; and customizable.

  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x B" . consult-buffer-no-preview) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("C-S-s" . consult-line)
         ("M-*" . consult-line-thing-at-point)
         ("C-c f" . consult-recent-file)
         ("C-S-h" . consult-ripgrep)
         ("C-c r" . consult-ripgrep-auto-preview)
         ;; TODO find an alternative to C-c c?
         ;("C-c c r" . consult-ripgrep-auto-preview)
         ;("C-c c s" . consult-ripgrep-case-sensitive)
         ;("C-c c z" . consult-z-ripgrep)
         ("C-c C-*" . consult-ripgrep-thing-at-point)
         ("C-c C-^" . consult-ripgrep-parent)
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         (:map isearch-mode-map
               ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
               ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
               ("M-s l" . consult-line)) ;; needed by consult-line to detect isearch
         (:map search-map
               ("f" . consult-fd)
               ("F" . consult-find)
               ("M-f" . consult-locate)
               ("g" . consult-grep)
               ("G" . consult-git-grep)
               ("r" . consult-ripgrep)
               ("R" . consult-ripgrep) ;; can't use r in isearch-mode, so add R too
               ("M-r" . consult-ripgrep-unrestricted)
               ("*" . consult-ripgrep-thing-at-point)
               ("z" . consult-z-ripgrep)
               ("^" . consult-ripgrep-parent)
               ("l" . consult-line)
               ("L" . consult-line-multi)
               ("m" . consult-multi-occur)
               ("k" . consult-keep-lines)
               ("u" . consult-focus-lines)
               ("e" . consult-isearch))
         (:map vertico-map
               ;; These are used for previewing with some consult commands (see consult-customize call below)
               ("C-S-p" . vertico-previous)
               ("C-S-n" . vertico-next)
               ;; Toggle preview on/off without changing preview-key
               ("M-P" . consult-toggle-preview)))
  :config
  ;(autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (add-to-list 'consult-mode-histories '(cider-repl-mode cider-repl-input-history))

  (defun consult-ripgrep-auto-preview (&optional dir initial)
    (interactive "P")
    (consult-ripgrep dir initial))
  (defun consult-ripgrep-unrestricted (&optional dir initial)
    (interactive "P")
    (let ((consult-ripgrep-args (replace-regexp-in-string "\\." "-uu ." consult-ripgrep-args)))
      (consult-ripgrep dir initial)))
  (defun consult-z-ripgrep (&optional dir initial)
    (interactive "P")
    (let ((consult-ripgrep-args (replace-regexp-in-string "\\." "-z ." consult-ripgrep-args)))
      (consult-ripgrep dir initial)))
  (defun consult-ripgrep-case-sensitive (&optional dir initial)
    (interactive "P")
    (let ((consult-ripgrep-args (replace-regexp-in-string "\\." "-s ." consult-ripgrep-args)))
      (consult-ripgrep dir initial)))
  (defun consult-buffer-no-preview ()
    (interactive)
    (consult-buffer))
  (defun consult-ripgrep-parent (&optional initial)
    (interactive "P")
    (consult-ripgrep (file-name-directory (directory-file-name (consult-project-root-function)))))

  (defalias 'consult-line-thing-at-point 'consult-line)
  (defalias 'consult-ripgrep-thing-at-point 'consult-ripgrep)

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   ;; For these commands we can use C-N/C-P to scroll and preview, or M-. to preview
   consult-git-grep consult-grep
   consult-ripgrep-parent consult-ripgrep consult-ripgrep-case-sensitive
   consult-ripgrep-unrestricted consult-z-ripgrep consult-ripgrep-thing-at-point
   consult-bookmark consult-recent-file consult-xref consult-buffer-no-preview
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key '("M-." :debounce 0.2 "C-S-n" :debounce 0.2 "C-S-p")
   consult-ripgrep-thing-at-point
   :initial (concat "#" (thing-at-point 'symbol))
   consult-line-thing-at-point
   :initial (thing-at-point 'symbol))

  (defvar-local consult-toggle-preview-orig nil)
  (defun consult-toggle-preview ()
    "Command to enable/disable preview."
    (interactive)
    (if consult-toggle-preview-orig
        (setq consult--preview-function consult-toggle-preview-orig
              consult-toggle-preview-orig nil)
      (setq consult-toggle-preview-orig consult--preview-function
            consult--preview-function #'ignore)))

  (setq consult-narrow-key "<")
  (append-to-list* 'consult-buffer-filter
                   "^\\*helpful"
                   "^\\*Warnings\\*"
                   "^\\*cider-test-report\\*"
                   "^\\*cider-error\\*"
                   "^\\*cider-inspect\\*")

  ;;(defvar consult-initial-narrow-config
  ;;  '((consult-buffer . ?x)
  ;;    (consult-buffer-no-preview . ?x)
  ;;    (consult-buffer-other-window . ?x)
  ;;    (consult-project-extra-find . ?f)))
  ;;;; Add initial narrowing hook
  ;;(defun consult-initial-narrow ()
  ;;  (when-let (key (alist-get this-command consult-initial-narrow-config))
  ;;    (setq unread-command-events (append unread-command-events (list key 32)))))
  ;;(add-hook 'minibuffer-setup-hook #'consult-initial-narrow)

  ;; Versions of consult--source-project-buffer and consult--source-project-file for use by consult-project-buffer
  ;; They allow narrowing with b, f and a (instead of p)
  ;; f is the recentf version provided by consult
  ;; a is an "all files" version based on fd (respecting .gitignore, hidden by default)
  (defvar consult--project-source-project-buffer
    (plist-put (plist-put (copy-sequence consult--source-project-buffer)
                          :hidden nil)
               :narrow '(?b . "Buffer")))
  (defvar consult--project-source-project-file-recentf
    (plist-put (plist-put (copy-sequence consult--source-project-recent-file)
                          :hidden nil)
               :narrow '(?f . "File (Recentf)")))
  (defvar consult--project-source-project-file-all
    (plist-put (plist-put (copy-sequence consult--source-project-recent-file)
                          :narrow '(?a . "File (All)"))
               :items '(lambda ()
                         (when (eq 0 (call-process-shell-command "fd"))
                           (when-let (root (consult--project-root))
                             (let ((len (length root))
                                   (inv-root (propertize root 'invisible t)))
                               (mapcar (lambda (x)
                                         (concat inv-root (substring x len)))
                                       (split-string
                                        (shell-command-to-string
                                         (format  "fd --color never -t f -0 . %s" root))
                                        "\0" t))))))))

  (defun consult-project-buffer ()
    (interactive)
    (let ((consult-buffer-sources '(consult--project-source-project-buffer
                                    consult--project-source-project-file-recentf
                                    consult--project-source-project-file-all)))
      (consult-buffer))))

(use-package consult-flycheck)

(use-package consult-dir
  :after (consult)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-git-log-grep
  :bind ("C-c g l" . consult-git-log-grep)
  :custom (consult-git-log-grep-open-function #'magit-show-commit))

(use-package consult-ls-git
  :bind ("C-c g f" . consult-ls-git))

(use-package consult-project-extra)

;;(use-package consult-todo
;;  ;; TODO use consult-todo-project when it works
;;  :after (consult)
;;  :bind ("C-c c t t" . consult-todo))

(use-package consult-projectile
  ;; package provides a function I use everyday: ~M-x consult-projectile~.  When
  ;; I invoke ~consult-projectile~, I have the file completion for the current
  ;; project.  I can also type =b= + =SPACE= to narrow my initial search to open
  ;; buffers in the project.  Or =p= + =space= to narrow to other projects; and
  ;; then select a file within that project.
  :after (projectile consult)
  :commands (consult-projectile)
  :ensure (consult-projectile
              :type git
              :host gitlab
              :repo "OlMon/consult-projectile"
              :branch "master")
  :config
  (setq consult-projectile-sources
    '( ;; key b
       consult-projectile--source-projectile-buffer
       ;; key f
       consult-projectile--source-projectile-file
       ;; key p
       consult-projectile--source-projectile-project
       ;; key d
       consult-projectile--source-projectile-dir
       ;; key m
       consult--source-bookmark
       ;; key r
       consult-projectile--source-projectile-recentf
       ;; key *
       consult--source-modified-buffer))

  (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
    (interactive)
    (let ((default-directory (or dir default-directory)))
      (consult--read #'read-file-name-internal :state (consult--file-preview)
        :prompt prompt
        :initial initial
        :require-match mustmatch
        :predicate pred)))
  :bind
  ("C-S-p" . consult-projectile)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package marginalia
  :hook (elpaca-after-init . marginalia-mode)
  :config
  ;; crux-recentf-find-file
  (add-to-list 'marginalia-prompt-categories '("Choose recent file" . file))
  (setq marginalia-max-relative-age 0) ;; Set absolute value)

(use-package embark
  :bind
  ("C-," . embark-act)
  ("M-," . embark-dwim)
  ("C-c C-o" . embark-export)
  ("C-h b" . embark-bindings)
  ("C-h B" . describe-bindings)
  (:map minibuffer-local-map
        ("M-." . embark-preview)
        ("C-," . embark-become))
  (:map embark-become-file+buffer-map
        ("e" . consult-project-extra-find)
        ("E" . project-switch-consult-project-extra-find))
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  :config
  (defun embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  (setq embark-action-indicator
        (lambda (map &optional _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  ;; demand, combined with after means that this will load after embark and consult
  ;; See https://github.com/oantolin/embark/commit/47daded610b245caf01a97d74c940aff91fe14e2#r46010972
  :demand t
  :bind
  (:map embark-consult-async-search-map
        ("^" . consult-ripgrep-parent)
        ("R" . consult-ripgrep-unrestricted))
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package orderless
  :defer 2
  :bind (:map minibuffer-local-map
              ("C-l" . my/orderless-match-components-literally))
  :custom
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion orderless)))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-strict-initialism))
  (orderless-style-dispatchers '(+orderless-dispatch))
  :config
  (defun my/orderless-match-components-literally ()
    "Components match literally for the rest of the session."
    (interactive)
    (setq-local orderless-matching-styles '(orderless-literal)
                orderless-style-dispatchers nil))

  (defun orderless-strict-initialism (component &optional leading)
    "Match a component as a strict leading initialism.
This means the characters in COMPONENT must occur in the
candidate, in that order, at the beginning of words, with
no words in between. If LEADING is non-nil, anchor to the
first word."
    (orderless--separated-by '(seq (zero-or-more word) (zero-or-more punct))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when leading '(seq buffer-start))))

  (defun orderless-strict-leading-initialism (component)
    "Match a component as a strict leading initialism.
This means the characters in COMPONENT must occur in the
candidate, in that order, at the beginning of words, with
no words in between, beginning with the first word."
    (orderless-strict-initialism component t))

  ;; based on https://github.com/minad/consult/wiki#minads-orderless-configuration
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?` . orderless-strict-leading-initialism)
      (?= . orderless-literal)
      (?_ . orderless-prefix)
      (?~ . orderless-flex)))

  (defun +orderless--suffix-regexp ()
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * _prefix prefix_
  ;; * %char-fold char-fold%
  ;; * `strict-leading-initialism strict-leading-initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--suffix-regexp))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--suffix-regexp))))
     ;; Ignore single !
     ((equal "!" word) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref word 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 1))
        (when-let (x (assq (aref word (1- (length word))) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 0 -1))))))))

(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*")
                 :includes (corfu-indexed corfu-quick corfu-history corfu-info corfu-popupinfo))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.3)
  (corfu-count 16)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  (corfu-separator ?\s)                 ; Necessary for use with orderless
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)       ; Preview current candidate?
  ;;(corfu-preselect-first t)             ; Preselect first candidate?
  ;;(corfu-preselect 'directory)
  (corfu-preselect 'prompt)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :hook (elpaca-after-init . global-corfu-mode)
  ;;;; Optionally use TAB for cycling, default is `corfu-complete'.
  ;;:bind (:map corfu-map
  ;;        ("M-m" . corfu-move-to-minibuffer)
  ;;        ("<escape>". corfu-quit)
  ;;        ("<return>" . corfu-insert)
  ;;        ("M-d" . corfu-show-documentation)
  ;;        ("M-l" . 'corfu-show-location)
  ;;        ("TAB" . corfu-next)
  ;;        ([tab] . corfu-next)
  ;;        ("S-TAB" . corfu-previous)
  ;;        ([backtab] . corfu-previous))
  ;;:preface
  ;;(defun corfu-move-to-minibuffer ()
  ;;  (interactive)
  ;;  (when completion-in-region--data
  ;;    (let ((completion-extra-properties corfu--extra)
  ;;          completion-cycle-threshold completion-cycling)
  ;;      (apply #'consult-completion-in-region completion-in-region--data))))
  ;;
  ;;(defun corfu-enable-always-in-minibuffer ()
  ;;  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  ;;  (unless (or (bound-and-true-p mct--active)
  ;;              (bound-and-true-p vertico--input)
  ;;              (eq (current-local-map) read-passwd-map))
  ;;    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
  ;;    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
  ;;                corfu-popupinfo-delay nil)
  ;;    (corfu-mode 1)))
  ;;:hook
  ;;(minibuffer-setup . (lambda () (corfu-enable-always-in-minibuffer 1)))
  )

(use-extension corfu corfu-indexed
  :config (corfu-indexed-mode 1))

(use-extension corfu corfu-quick
  :bind (:map corfu-map
              ("M-;" . corfu-quick-insert)
              ("M-'" . corfu-quick-exit)))

(use-extension corfu corfu-history
  :after savehist
  :config
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-extension corfu corfu-popupinfo
  :hook (global-corfu-mode . corfu-popupinfo-mode))

(use-package cape
  ;; Completion at point functions, with the amazing `cape-super-capf' for
  ;; granular configuration of specific mode completion behavior.

  :custom
  (cape-dabbrev-min-length 2)
  (cape-dabbrev-check-other-buffers t)
  :config
  ;;(require 'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-file)
  ;;:bind (("C-c p p" . completion-at-point) ;; capf
  ;;       ("C-c p t" . complete-tag)        ;; etags
  ;;       ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;       ("C-c p h" . cape-history)
  ;;       ("C-c p f" . cape-file)
  ;;       ("C-c p k" . cape-keyword)
  ;;       ("C-c p s" . cape-elisp-symbol)
  ;;       ("C-c p e" . cape-elisp-block)
  ;;       ("C-c p a" . cape-abbrev)
  ;;       ("C-c p l" . cape-line)
  ;;       ("C-c p w" . cape-dict)
  ;;       ("C-c p :" . cape-emoji)
  ;;       ("C-c p \\" . cape-tex)
  ;;       ("C-c p _" . cape-tex)
  ;;       ("C-c p ^" . cape-tex)
  ;;       ("C-c p &" . cape-sgml)
  ;;       ("C-c p r" . cape-rfc1345))
  )

;; lots of snippets at
;; https://github.com/AndreaCrotti/yasnippet-snippets.git
;(use-package yasnippet
;  :config
;  (use-package yasnippet-snippets :after yasnippet)
;  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;  (yas-global-mode 1)
;  (setq yas/indent-line nil)
;  ;;:hook ((prog-mode LaTeX-mode org-mode) . yas-minor-mode)
;  ;;:bind
;  ;;((:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key))
;  ;; (:map yas-keymap
;  ;;       (("TAB" . smarter-yas-expand-next-field)
;  ;;        ([(tab)] . smarter-yas-expand-next-field))))
;  ;;:config
;  ;;(yas-reload-all)
;  ;;(defun smarter-yas-expand-next-field ()
;  ;;  "Try to `yas-expand' then `yas-next-field' at current cursor position."
;  ;;  (interactive)
;  ;;  (let ((old-point (point))
;  ;;        (old-tick (buffer-chars-modified-tick)))
;  ;;    (yas-expand)
;  ;;    (when (and (eq old-point (point))
;  ;;               (eq old-tick (buffer-chars-modified-tick)))
;  ;;      (ignore-errors (yas-next-field)))))
;  )
(use-package tempel
  ;; For awhile, I'd used yasnippets; themselves inspired by my beloved
  ;; TextMate.  However, I've found `tempel' to be both more than adequate and
  ;; has a narrower implementation foot-print, cleaving closer to emacs-lisp;
  ;; thus likely easing it's maintenance burden.
  :ensure (tempel :host github :repo "minad/tempel")

  :custom
  ;;(tempel-path (expand-file-name "templates" user-emacs-directory))
  (tempel-path (expand-file-name "tempel-templates" user-emacs-directory))

  :bind
  ;;("M-+" . tempel-complete) ;; Alternative tempel-expand
  ;;("M-*" . tempel-insert)
  (:map tempel-map (([backtab] . tempel-previous)
                    ([tab] . tempel-next)))
  :config
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  (add-to-list 'tempel-user-elements (lambda (elt)
                                       (when (eq (car-safe elt) 'i)
                                         (if-let (template (alist-get (cadr elt) (tempel--templates)))
                                             (cons 'l template)
                                           (message "Template %s not found" (cadr elt))
                                           nil))))
  :hook
  ((text-mode prog-mode conf-mode) . (lambda ()
                                       (tempel-abbrev-mode)
                                       (setq-local completion-at-point-functions
                                                   (cons #'tempel-expand
                                                         completion-at-point-functions))))

  ;; Hyper Macro!
  ;;(tempel-key "H-m d" tsomb-date org-mode-map)
  ;;(tempel-key "H-m u" update_block org-mode-map)
  ;;(tempel-key "H-m c" macro-cite org-mode-map)
  ;;(tempel-key "H-m i" macro-idiomatic org-mode-map)
  ;;(tempel-key "H-m m" macro-mechanic org-mode-map)
  ;;(tempel-key "H-m k" macro-keyboard org-mode-map)
  )

(use-package vertico
  :ensure (vertico :files (:defaults "extensions/*")
                   :includes (vertico-directory vertico-repeat vertico-indexed vertico-quick))
  :hook (elpaca-after-init . vertico-mode)
  :custom
  ;; Different scroll margin
  (vertico-scroll-margin 0)
  ;; Show more candidates
  (vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;;(vertico-resize t)
  (vertico-cycle t)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index start)
                (setq cand (funcall orig cand prefix suffix index start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))

  (defun down-from-outside ()
    "Move to next candidate in minibuffer, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro [down])))

  (defun up-from-outside ()
    "Move to previous candidate in minibuffer, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro [up])))

  (defun preview-from-outside ()
    "Preview the selected candidate, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro (kbd "M-."))))

  (defun to-and-fro-minibuffer ()
    "Go back and forth between minibuffer and other window."
    (interactive)
    (if (window-minibuffer-p (selected-window))
        (select-window (minibuffer-selected-window))
      (select-window (active-minibuffer-window))))

  (defun minibuffer-really-quit ()
    "Quit minibuffer session, even if it is not the selected window."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (minibuffer-keyboard-quit)))

  :bind (("C-M-<" . up-from-outside)
         ("C-M->" . down-from-outside)
         ("C-M-+" . preview-from-outside)
         ("M-X" . to-and-fro-minibuffer)
         ("C-M-S-g" . minibuffer-really-quit)
         (:map vertico-map ("M-RET" . minibuffer-force-complete-and-exit))))

(use-extension vertico vertico-directory
  :config
  (defvar switching-project nil)
  (defun vertico-directory-enter-or-select-project ()
    "vertico-directory-enter wrapper that plays nicely with selecting new projects."
    (interactive)
    ;; When selecting a project, use this to return, instead of entering the directory
    (if switching-project
        (vertico-exit)
      (vertico-directory-enter)))
  (defun vertico-directory-slash ()
    (interactive)
    (if (and (>= vertico--index 0)
             (string-suffix-p "/" (vertico--candidate))
             (eq 'file (vertico--metadata-get 'category)))
        (vertico-insert)
      (insert "/")))
  (defun vertico-directory-home ()
    (interactive)
    (if (and (string-suffix-p "/" (vertico--candidate))
             (eq 'file (vertico--metadata-get 'category)))
        (insert "~/")
      (insert "~")))
  (defun read-project (orig &rest args)
    (let ((switching-project t))
      (apply orig args)))
  (advice-add 'project-prompt-project-dir :around
              'read-project)

  ;; TODO this should be part of the vertico config
  (defun define-vertico-key (key &rest defs)
    "Define KEY conditionally in the vertico keymap.
DEFS is a plist associating completion categories to commands."
    (let ((default-command (lookup-key vertico-map (kbd key))))
      (define-key vertico-map (kbd key)
        (list 'menu-item nil defs :filter
              (lambda (d)
                (or (plist-get d (completion-metadata-get
                                  (completion-metadata (minibuffer-contents)
                                                       minibuffer-completion-table
                                                       minibuffer-completion-predicate)
                                  'category))
                    default-command))))))
  (define-vertico-key "/"
    'file #'vertico-directory-slash
    'project-file #'vertico-directory-slash)
  (define-vertico-key "RET"
    'file #'vertico-directory-enter-or-select-project
    'project-file #'vertico-directory-enter)
  (define-vertico-key "~"
    'file #'vertico-directory-home)
  (define-vertico-key "DEL"
    'file #'vertico-directory-delete-char
    'project-file #'vertico-directory-delete-char)
  (define-vertico-key "M-DEL"
    'file #'vertico-directory-delete-word
    'project-file #'vertico-directory-delete-word)
  :commands (vertico-directory-enter vertico-directory-delete-word vertico-directory-delete-char)
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-extension vertico vertico-repeat
  :after savehist
  :bind
  ("C-\\" . vertico-repeat)
  ("C-|" . vertico-repeat-select)
  :hook (minibuffer-setup . vertico-repeat-save)
  :config
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-extension vertico vertico-indexed
  :config
  (defmacro define-choose (n)
    `(defun ,(intern (format "vertico-indexed-choose-%s" n)) ()
       ,(format "Exit minibuffer with candidate %s." n)
       (interactive)
       (let ((current-prefix-arg ,n))
         (funcall-interactively 'vertico-exit))))
  (defmacro define-insert (n)
    `(defun ,(intern (format "vertico-indexed-insert-%s" n)) ()
       ,(format "Insert candidate %s in minibuffer." n)
       (interactive)
       (let ((current-prefix-arg ,n))
         (funcall-interactively 'vertico-insert))))
  (dotimes (n 10)
    (eval `(define-choose ,n))
    (eval `(define-insert ,n))
    (define-key vertico-map (kbd (format "C-%s" n)) (intern (format "vertico-indexed-choose-%s" n)))
    (define-key vertico-map (kbd (format "M-%s" n)) (intern (format "vertico-indexed-insert-%s" n))))
  (vertico-indexed-mode 1))

(use-extension vertico vertico-quick
  :bind (:map vertico-map
              ("M-;" . vertico-quick-insert)
              ("M-'" . vertico-quick-exit)))

(provide 'init-complete)
;;; init-complete.el ends here

