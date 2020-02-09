;;; Sets up functions and things related to fonts

;;; Code:

(setq my:default-font "Inconsolata")
(setq my:default-font-size 14)
(setq my:current-font-size my:default-font-size)
(setq my:font-change-increment 1.1)

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

(define-key global-map (kbd "C-)") 'my:reset-font-size)
(define-key global-map (kbd "C-+") 'my:increase-font-size)
(define-key global-map (kbd "C--") 'my:decrease-font-size)
;(define-key global-map (kbd "C-=") 'my:increase-font-size)
;(define-key global-map (kbd "C-_") 'my:decrease-font-size)

;; Adjust font size like web browsers
;;(global-set-key (kbd "C-+") #'text-scale-increase)
;;(global-set-key (kbd "C--") #'text-scale-decrease)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(my:reset-font-size)
;;(global-font-lock-mode t)
(global-prettify-symbols-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; decorative types

;;(let ( ;( my-extra-types
;;		; '( "\\([UI]i*\\)nt\\(8\\|16\\|32\\|64\\)" "Index"
;;		;	"Float\\(32\\|64\\)" "sstring" "RE_\\(\\w*\\)Coord" ) )
;;	   ( my-mode-additions
;;		 '( ("\\<\\(TODO\\):" . font-lock-warning-face)
;;			("\\<\\(PROGRAMMING_ERROR\\|ASSERT\\w*\\|DEBUG_ABORT\\w*\\|BADPLACE\\w*\\)\\>" . font-lock-warning-face)
;;			("\\<\\(PRECONDITION\\|POSTCONDITION\\|CHECK_INVARIANT\\|REQUIRE\\|ENSURE\\|STATIC_CHECK\\)\\>" . font-lock-constant-face)
;;			) )
;;	   )
;;;  (setq c-font-lock-extra-types 
;;;		(append c-font-lock-extra-types my-extra-types))
;;;  (setq c++-font-lock-extra-types 
;;;		(append c++-font-lock-extra-types my-extra-types ) )
;;
;;  (font-lock-add-keywords 'c-mode my-mode-additions)
;;  (font-lock-add-keywords 'c++-mode my-mode-additions)
;;  (font-lock-add-keywords 'objc-mode my-mode-additions)
;;)

;;(setq font-lock-support-mode 'jit-lock-mode)
;;(setq font-lock-maximum-decoration t)
;;(setq-default font-lock-multiline t)
;(setq lazy-lock-defer-time 1)
;(setq lazy-lock-defer-on-the-fly nil)
;(setq lazy-lock-defer-on-scrolling nil)
;(setq lazy-lock-defer-contextually nil)
;(setq lazy-lock-minimum-size '((c-mode . 0) (c++-mode . 0) (t . 1000)))
;(setq lazy-lock-stealth-time 5)

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

(set-face-foreground 'font-lock-builtin-face "DarkOrchid1")
(set-face-foreground 'font-lock-comment-face "green3")
(set-face-foreground 'font-lock-doc-face "green3")
(set-face-foreground 'font-lock-constant-face "LightSeaGreen")
(set-face-foreground 'font-lock-keyword-face "magenta")
(set-face-foreground 'font-lock-string-face "yellow")
(set-face-foreground 'font-lock-type-face "goldenrod")
(set-face-foreground 'font-lock-variable-name-face "wheat")
(set-face-foreground 'font-lock-warning-face "OrangeRed1")

(set-face-foreground 'show-paren-mismatch "black")
(set-face-background 'show-paren-mismatch "OrangeRed1")
(set-face-foreground 'show-paren-match "black")
(set-face-background 'show-paren-match "DarkSlateGray3")
;(set-face-foreground 'show-paren-mismatch-face "black")
;(set-face-background 'show-paren-mismatch-face "OrangeRed1")
;(set-face-foreground 'show-paren-match-face "black")
;(set-face-background 'show-paren-match-face "DarkSlateGray3")

(set-face-foreground 'highlight "red")
(set-face-background 'highlight "grey10")
(set-face-foreground 'region "red")
(set-face-background 'region "grey10")
(set-face-foreground 'secondary-selection "red")
(set-face-background 'secondary-selection "grey10")
(set-face-background 'default "black")
(set-face-foreground 'default "gainsboro")
;(set-face-foreground 'modeline "white")
;(set-face-background 'modeline "black")

(provide 'init-font)
