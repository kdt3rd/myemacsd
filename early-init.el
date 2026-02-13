;;; -*- lexical-binding: t; -*-

;; only emacs 27+....

;; garbage collection is apparently slow at startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq site-run-file nil)

(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;(when (featurep 'ns)
;  (push '(ns-transparent-titlebar . t) default-frame-alist))

(setq frame-inhibit-implied-resize t)

(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(tool-bar-mode 0)
(scroll-bar-mode -1)
(set-window-scroll-bars (minibuffer-window) nil nil)

(setq inhibit-splash-screen t
      use-dialog-box t
      use-file-dialog nil
      inhibit-startup-buffer-menu t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      initial-scratch-message nil)

(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))
(define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
  (advice-remove #'load-file #'load-file@silence))

(provide 'early-init)
