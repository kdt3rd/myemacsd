;;; Sets up functions and things related to fonts

;;; Code:

(use-feature emacs
  :bind
  :hook
  (elpaca-after-init . (lambda ()
                         ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specify custom colors for things
;(font-lock-make-faces)
;(set-face-font 'font-lock-function-name-face
;               "-*-Terminal-normal-r-*-*-18-108-*-*-c-*-*-oem-")
;(set-face-font 'font-lock-comment-face
;               "-*-Courier New-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
;(set-face-font 'font-lock-string-face
;               "-*-Lucida Console-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
;(set-face-font 'font-lock-keyword-face
;               "-*-Courier New-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
;(set-face-font 'font-lock-type-face
;               "-*-Courier New-normal-r-*-*-17-102-*-*-c-*-*-ansi-")
;(set-face-font 'font-lock-reference-face
;               "-*-Terminal-normal-r-*-*-18-108-*-*-c-*-*-oem-")

(provide 'init-font)
