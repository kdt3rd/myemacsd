;;; init-global-config.el --- Global UI/General Configuration -*- lexical-binding: t -*-

;;;; if we hit kill buffer, why wouldn't we want to kill the current buffer???
;;(defun my:kill-current-buffer ()
;;    "Kill the current buffer without prompting."
;;    (interactive)
;;    (kill-buffer (current-buffer)))

(defun my:font-name ()
  "Return a string with the font name and size."
  (concat my:default-font "-" (number-to-string my:current-font-size)))

(defun my:set-font-size ()
  "Set the font to the default font plus current size."
  (let ((font-name (my:font-name)))
    (if (assoc 'font default-frame-alist)
        (setcdr (assoc 'font default-frame-alist) font-name)
      (add-to-list 'default-frame-alist (cons 'font font-name)))
    (set-frame-font font-name)))

(defun my:reset-font-size ()
  "Reset font size to default."
  (interactive)
  (setq my:current-font-size my:default-font-size)
  (my:set-font-size))

(defun my:increase-font-size ()
  "Increase current font size by increment."
  (interactive)
  (setq my:current-font-size
        (ceiling (* my:current-font-size my:font-change-increment)))
  (my:set-font-size))

(defun my:decrease-font-size ()
  "Decrease current font size by increment."
  (interactive)
  (setq my:current-font-size
        (max 1
             (floor (/ my:current-font-size my:font-change-increment))))
  (my:set-font-size))

(defun my:init-general-emacs ()
  (interactive)
  ;;(global-display-line-numbers-mode)
  (global-hl-line-mode +1)
  ;(global-eldoc-mode -1)

  (global-set-key (kbd "C-x k") 'kill-this-buffer)
  ;;(global-set-key (kbd "C-x C-S-k") 'kill-this-buffer)

  ;; Mode line controls...
  (column-number-mode t)
  (line-number-mode t)
  (display-time-mode t)
  ;;(display-battery-mode 1)

  (save-place-mode 1)
  (delete-selection-mode t)
  (global-auto-revert-mode 1)

  ;; mitigates slowness due to extremely long lines, only in latest emacsen
  (when (fboundp 'global-so-long-mode)
    (global-so-long-mode 1))

  ;;(kill-buffer "*Messages*")

  (transient-mark-mode t)

  (set-fill-column 80)

  (global-font-lock-mode t)
  (global-auto-revert-mode t)
  (global-prettify-symbols-mode t)
  ;; wraps lines
  (global-visual-line-mode 1)

  ;;(auto-image-file-mode t nil (image-file))
  ;;(case-fold-search t)
  ;;(frame-background-mode 'dark)
  ;;(mm-inline-large-images t)

  (unless *sys/win32*
    (prefer-coding-system 'utf-8)
    (set-language-environment "UTF-8")
    (set-selection-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8))

  ;;(cond
  ;; ((find-font (font-spec :name "iosevka comfy"))
  ;;  (set-face-attribute 'default nil :font "iosevka comfy"))
  ;; ((find-font (font-spec :name "iosevka"))
  ;;  (set-face-attribute 'default nil :font "iosevka")))
  (load-theme 'kdt t)

  (fringe-mode (cons my:fringe-width my:fringe-width))
  (my:reset-font-size)
  (blink-cursor-mode 0)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-feature emacs
  :bind
  ("C-x \\" . align-regexp)
  ("C-)" . my:reset-font-size)
  ("C-+" . my:increase-font-size)
  ("C--" . my:decrease-font-size)
  ;;(define-key global-map (kbd "C-=") 'my:increase-font-size)
  ;;(define-key global-map (kbd "C-_") 'my:decrease-font-size)
  :hook
  (elpaca-after-init . my:init-general-emacs)
  (before-save . (lambda ()
                   ;;(delete-trailing-whitespace-except-current-line)
                   (when buffer-file-name
                     (let ((dir (file-name-directory buffer-file-name)))
                       (when (and (not (file-exists-p dir))
                                  (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                         (make-directory dir t))))))
  ;; http://yummymelon.com/devnull/enhancing-navigation-in-emacs-view-mode.html
  (view-mode . (lambda ()
                 (cond ((derived-mode-p 'org-mode)
                        (define-key view-mode-map (kbd "p") 'org-previous-visible-heading)
                        (define-key view-mode-map (kbd "n") 'org-next-visible-heading))
                       ((derived-mode-p 'markdown-mode)
                        (define-key view-mode-map (kbd "p") 'markdown-outline-previous)
                        (define-key view-mode-map (kbd "n") 'markdown-outline-next))
                       ((derived-mode-p 'html-mode)
                        (define-key view-mode-map (kbd "p") 'sgml-skip-tag-backward)
                        (define-key view-mode-map (kbd "n") 'sgml-skip-tag-forward))
                       ((derived-mode-p 'emacs-lisp-mode)
                        (define-key view-mode-map (kbd "p") 'backward-sexp)
                        (define-key view-mode-map (kbd "n") 'forward-sexp))
                       ((derived-mode-p 'clojure-mode)
                        (define-key view-mode-map (kbd "p") 'backward-sexp)
                        (define-key view-mode-map (kbd "n") 'forward-sexp))
                       ((derived-mode-p 'makefile-mode)
                        (define-key view-mode-map (kbd "p") 'makefile-previous-dependency)
                        (define-key view-mode-map (kbd "n") 'makefile-next-dependency))
                       ((derived-mode-p 'c-mode)
                        (define-key view-mode-map (kbd "p") 'c-beginning-of-defun)
                        (define-key view-mode-map (kbd "n") 'c-end-of-defun))
                       (t
                        (define-key view-mode-map (kbd "p") 'scroll-down-command)
                        (define-key view-mode-map (kbd "n") 'scroll-up-command)))))
  :config
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-defun 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)

  ;; UTF-8
  (unless *sys/win32*
    (setq locale-coding-system 'utf-8)
    (setq-default bidi-paragraph-direction 'left-to-right)
    (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
    (setq bidi-inhibit-bpa t)
    )

  (when *sys/mac*
    (setq mac-option-modifier 'meta)
    (setq mac-right-option-modifier 'none)
    (setq mac-command-modifier 'super))

  ;; change clipboard input to UTF-8 string first, compound text next, etc.
  (when *sys/gui*
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
    ;; ;; Map Alt key to Meta
    ;; (setq x-alt-keysym 'meta)

    (setq no-blinking-cursor t)
    )

  (setq show-trailing-whitespace t)
  (setq-default indicate-empty-lines t)

  ;; http://whattheemacsd.com/appearance.el-02.html (if this gets to annoying, just set back to 'ignore)
  ;;(setq ring-bell-function 'ignore)
  (setq ring-bell-function (lambda ()
                             (invert-face 'mode-line)
                             (run-with-timer 0.05 nil 'invert-face 'mode-line))
        visible-bell t
        )

  ;; smooth scrolling
  ;; vertical
  (setq scroll-margin 1
        scroll-conservatively 101
        scroll-preserve-screen-position 1
        scroll-step 1
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01
        auto-window-vscroll nil
        fast-but-imprecise-scrolling nil
        mouse-wheel-scroll-amount '(1 ((shift) . 5))
        mouse-wheel-progressive-speed nil
        )
  ;; horizontal
  (setq hscroll-step 1
        hscroll-margin 1
        )

  (setq pixel-scroll-precision-mode t)

  ;; update progress instantly
  (setq echo-keystrokes 0.1)

  (setq-default indent-tabs-mode nil
                tab-width 4)
  (setq comment-auto-fill-only-comments t
        comment-padding 0)

  (setq sentence-end-double-space nil
        next-line-add-newlines nil
        require-final-newline t
        )

  (setq mouse-yank-at-point t
        save-interprogram-paste-before-kill t
        apropos-do-all t
        )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; misc flags

  (setq large-file-warning-threshold 100000000)
  (setq make-backup-files t)
  (setq delete-old-versions t) ; silently clean up old versions
  (setq version-control t) ; Allow numbered backups
  (setq vc-follow-symlinks t)

  ;; Set history-length longer
  (setq-default history-length 500)
  (setq create-lockfiles nil)

  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  (setq save-place-file (expand-file-name "saveplace" *cache-save-dir*))

  ;; Better Compilation
  (setq-default compilation-always-kill t) ; kill compilation before starting another
  (setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
  (setq-default compilation-scroll-output t)

  ;;(setq-default frame-title-format '("%b%* - " user-login-name "@" system-name))
  (setq-default frame-title-format "Emacs: %b%*")
  (setq icon-title-format "Emacs: %b%*")

  (setq message-log-max nil) ;; 10000

  (if (version< emacs-version "28")
      (defalias 'yes-or-no-p 'y-or-n-p)
    (setq use-short-answers t))
  ;;(fset 'yes-or-no-p 'y-or-n-p)

  (setq query-replace-highlight t)
  (setq search-highlight t)

  ;;(put 'eval-expression 'disabled nil)
  ;;(setq enable-local-variables t)
  ;;(setq enable-local-eval t)

  ;;(setq initial-major-mode 'text-mode)

  ;;(if (version< emacs-version "26")
  ;;    (global-linum-mode)
  ;;  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  ;;  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; General editing controls

  ;; TODO: is this safe
  ;;(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)
  ;;(add-hook 'before-save-hook #'delete-trailing-whitespace)

  )

;;(require 'server)
;;(unless (server-running-p) (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; History
(use-feature recentf
  :config
  (add-to-list 'recentf-exclude (expand-file-name "elpa" user-emacs-directory))
  (add-to-list 'recentf-exclude (expand-file-name "straight" user-emacs-directory))
  (add-to-list 'recentf-exclude (expand-file-name "elpaca" user-emacs-directory))
  :custom
  (recentf-save-file (expand-file-name "recentf" *cache-save-dir*))
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup (* 60 60))
  :hook (elpaca-after-init . recentf-mode))

(use-feature savehist
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" *cache-save-dir*))
  :hook (elpaca-after-init . savehist-mode))

(use-feature bookmark
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" *cache-save-dir*))
  (bookmark-save-flag 1))

(provide 'init-global-config)
;;; init-global-config.el ends here
