;;; -*- lexical-binding: t; -*-

;; init emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load path bits
;;(defun update-to-load-path (folder)
;;  "Add folder FOLDER and its subdirectories to `load-path'."
;;  (let ((base folder))
;;    (unless (member base load-path)
;;      (add-to-list 'load-path base))
;;    (dolist (f (directory-files base))
;;      (let ((name (concat base "/" f)))
;;        (when (and (file-directory-p name)
;;                   (not (equal f ".."))
;;                   (not (equal f ".")))
;;          (unless (member base load-path)
;;            (add-to-list 'load-path name)))))))
;;
;;(update-to-load-path (expand-file-name "elisp" user-emacs-directory))

(defconst *cache-save-dir* (expand-file-name "cache" user-emacs-directory))
(defconst *user-elisp-dir* (expand-file-name "elisp" user-emacs-directory))
(defconst *site-elisp-dir* (expand-file-name "site-elisp" user-emacs-directory))
(defconst *user-theme-dir* (expand-file-name "themes" user-emacs-directory))
(add-to-list 'load-path *user-elisp-dir*)
(add-to-list 'load-path *site-elisp-dir*)

(add-to-list 'custom-theme-load-path *user-theme-dir*)

(unless (file-exists-p *cache-save-dir*)
  (make-directory *cache-save-dir*))

;; we set these things in early-init.el, clean them out when done initializing
;;(add-hook 'emacs-startup-hook
;;          (lambda ()
;;            (setq gc-cons-threshold better-gc-cons-threshold)
;;            (setq gc-cons-percentage 0.1)
;;            (setq file-name-handler-alist file-name-handler-alist-original)
;;            (makunbound 'file-name-handler-alist-original)
;;            (if (boundp 'after-focus-change-function)
;;                (add-function :after after-focus-change-function
;;                              (lambda ()
;;                                (unless (frame-focus-state)
;;                                  (garbage-collect))))
;;              (add-hook 'after-focus-change-function 'garbage-collect))
;;            (defun gc-minibuffer-setup-hook ()
;;              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))
;;
;;            (defun gc-minibuffer-exit-hook ()
;;              (garbage-collect)
;;              (setq gc-cons-threshold better-gc-cons-threshold))
;;
;;            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
;;            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

;;(setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-const)

(require 'init-packagemanager)

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq gc-cons-percentage 0.1)))

(require 'init-functions)
(require 'init-font-functions)
(require 'init-programming-functions)

(require 'init-global-config)

(require 'init-editor)

(require 'init-search)

(require 'init-highlight)

(require 'init-project)
(require 'init-complete)
;;(require 'init-nav)
;;
(require 'init-all-modes)

(require 'init-ai-agent)

(elpaca-process-queues)

;;; set up all the global key bindings
(require 'init-global-keys)

;;; set up local variables for project settings
;(require 'init-project-settings)

;;(defun force-debug (func &rest args)
;;  (condition-case e
;;      (apply func args)
;;    ((debug error) (signal (car e) (cdr e)))))
;;
;;(advice-add #'corfu--post-command :around #'force-debug)
;;(setq completion-at-point-functions (list (cape-capf-debug #'cape-dict)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)

;;; init.el ends here
