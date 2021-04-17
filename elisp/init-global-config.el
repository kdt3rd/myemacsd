
(eval-when-compile
  (require 'init-const))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; UTF-8
(unless *sys/win32*
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))

;; change clipboard input to UTF-8 string first, compound text next, etc.
(when *sys/gui*
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (require 'server)
  (unless (server-running-p) (server-start))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; History
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)

;; mitigates slowness due to extremely long lines, only in latest emacsen
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Replace selection on insert
(delete-selection-mode 1)

;; ;; Map Alt key to Meta
;; (setq x-alt-keysym 'meta)

;; When buffer is closed, saves the cursor location
(save-place-mode t)

;; Set history-length longer
(setq-default history-length 500)

;; update progress instantly
(setq echo-keystrokes 0.1)

;; skip lock files... dangerous?
(setq-default create-lockfiles nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)

(setq-default frame-title-format '("%b%* - " user-login-name "@" system-name))
(setq icon-title-format "Emacs - %b")

(setq message-log-max nil)
(kill-buffer "*Messages*")

(setq query-replace-highlight t)
(setq search-highlight t)
(transient-mark-mode t)

;; Mode line controls...
(column-number-mode t)
(line-number-mode t)
(display-time-mode t)
;(display-battery-mode 1)
;(let ((process-connection-type nil))    ; try not to hog a pty for this.
;  (display-time))

(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;(setq initial-major-mode 'text-mode)

;(if (version< emacs-version "26")
;    (global-linum-mode)
;  (add-hook 'text-mode-hook #'display-line-numbers-mode)
;  (add-hook 'prog-mode-hook #'display-line-numbers-mode))


;; smooth scrolling
;; vertical
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; horizontal
(setq hscroll-step 1)
(setq hscroll-margin 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General editing controls

;; TODO: is this safe
;(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)
;(add-hook 'before-save-hook #'delete-trailing-whitespace)
(set-fill-column 80)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq comment-padding 0)
(put 'eval-expression 'disabled nil)
(setq enable-local-variables t)
(setq enable-local-eval t)

(setq next-line-add-newlines nil)
(setq require-final-newline t)          ; always terminate last line in file
(setq visible-bell t)
;(setq ring-bell-function 'ignore)

(setq mouse-yank-at-point t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc flags

(setq make-backup-files t)
(setq delete-old-versions t) ; silently clean up old versions
(setq version-control t) ; Allow numbered backups
(setq vc-follow-symlinks t)

(global-font-lock-mode t)
(global-auto-revert-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; if we hit kill buffer, why wouldn't we want to kill the current buffer???
(defun my:kill-current-buffer ()
    "Kill the current buffer without prompting."
    (interactive)
    (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'my:kill-current-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

(provide 'init-global-config)
