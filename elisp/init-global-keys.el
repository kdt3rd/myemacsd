
;; unbind some globals
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)

(global-set-key (kbd "C-x C-b") #'ibuffer)

(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)

;; Adjust font size like web browsers
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

(global-set-key "\M-\C-g" 'goto-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

(global-set-key "\C-cd" 'copy-region-as-kill)
(global-set-key "\C-c\C-i" 'indent-region)
(global-set-key (kbd "C-=") 'align-equals)
(global-set-key (kbd "C-,") 'align-vars)
;;;(global-set-key (kbd "C-.") 'bounce-sexp)

;(global-set-key "\C-xc" 'toggle-source-header)
(global-set-key "\C-xc" 'ff-find-other-file)
(global-set-key "\C-ci" 'start-new-file)
;(global-set-key "\C-cn" 'insert-class-name)
(global-set-key "\C-cn" 'insert-classfn-separator)
(global-set-key "\C-cv" 'insert-fn-separator)
(global-set-key "\C-cc" 'insert-copyright)

;(global-set-key [S-iso-lefttab] 'dabbrev-expand)
;(global-set-key [C-tab] 'bury-buffer)

;;; Set up some function keys
(global-set-key [f2] 'global-cleanup-file)
(global-set-key [f3] 'query-replace)
(global-set-key [S-f3] 'replace-string)
;(global-set-key [f4] 'replace-string)
(global-set-key [f4] 'next-error)
(global-set-key [C-f4] 'compile)
;(global-set-key [f5] 'replace-string)
(global-set-key [f5] 'copy-to-register)
(global-set-key [f6] 'insert-register)
;(global-set-key [f6] 'compile)
;(global-set-key [f6] 'do-pbxbuild)
(global-set-key [C-f6] 'copy-to-register)
(global-set-key [f7] 'prepend-string-to-range)
(global-set-key [S-f7] 'unprepend-string-to-range)
(global-set-key [f8] 'append-string-to-range)
;(global-set-key [f9] 'undo)
(global-set-key [f9] 'toggle-tab-size)
;(global-set-key [f10] 'other-window)
;(global-set-key [f10] '(lambda ()
;                         (interactive)
;                         (expand-class-functions (current-buffer))))
(global-set-key [(f10)] 'gnuplot-make-buffer)
;(global-set-key [f12] 'goto-line)

; TODO: put this back if we turn off smooth scroll
;(global-set-key [mouse-4] 'skip-backward-lines)
;(global-set-key [mouse-5] 'skip-forward-lines)

(provide 'init-global-keys)

