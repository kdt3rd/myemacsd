;; only emacs 27+....

;; garbage collection is apparently slow at startup
(setq gc-cons-threshold 100000000)

(setq package-enable-at-startup nil)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq site-run-file nil)

(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode -1)
(set-window-scroll-bars (minibuffer-window) nil nil)
;(unless (and (display-graphic-p) (eq system-type 'darwin))
;  (push '(menu-bar-lines . 0) default-frame-alist))
;(push '(tool-bar-lines . 0) default-frame-alist)
;(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
