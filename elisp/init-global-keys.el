;;; init-global-keys.el --- Global Key Bindings -*- lexical-binding: t -*-


;;(define-key global-map (kbd "C-=") 'my:increase-font-size)
;;(define-key global-map (kbd "C-_") 'my:decrease-font-size)
(use-feature emacs
  :bind
  ("C-x \\" . align-regexp)
  ("C-)" . my:reset-font-size)
  ("C-+" . my:increase-font-size)
  ("C--" . my:decrease-font-size)
  ("C-z" . nil)
  ("M-z" . nil)
  ("C-x C-z" . nil)
  ("C-x C-b" . ibuffer)
  ("M-n" . forward-paragraph)
  ("M-p" . backward-paragraph)
  ("M-C-g" . my:goto-line-with-feedback)
  ([home] . beginning-of-line)
  ([end] . end-of-line)
  ([\C-home] . beginning-of-buffer)
  ([\C-end] . end-of-buffer)
  ("C-c d" . copy-region-as-kill)
  ;("C-c C-i" . indent-region) ; C-c [tab]
  ("C-x c" . ff-find-other-file)
  ("C-c i" . start-new-file)
  ("C-c n" . insert-classfn-separator)
  ("C-c v" . insert-fn-separator)
  ("C-c c" . insert-copyright)
  ;;("M-C-i" . completion-at-point)
  ;;("TAB" . indent-for-tab-comand)
  ;;("[f1]" . eshell)
  ([f1] . ansi-term)
  ([f3] . query-replace)
  ([S-f3] . replace-string)
  ;([f4] . next-error)
  ;([\C-f4] . compile)
  ([f5] . copy-to-register)
  ([f6] . insert-register)
  ([\C-f6] . copy-to-register)
  ([f7] . prepend-string-to-range)
  ([S-f7] . unprepend-string-to-range)
  ([f8] . append-string-to-range)
  ([S-f8] . unappend-string-to-range)
  ([f9] . toggle-tab-size)
  ;;(global-set-key (kbd "C-x k") 'kill-this-buffer)
  ("C-x k" . my:kill-current-buffer)

)

; TODO: put this back if we turn off smooth scroll
;(global-set-key [mouse-4] 'skip-backward-lines)
;(global-set-key [mouse-5] 'skip-forward-lines)

(provide 'init-global-keys)
;;; init-global-keys.el ends here
