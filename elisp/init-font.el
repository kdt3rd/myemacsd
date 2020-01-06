
(eval-when-compile
  (require 'init-const))

(when *sys/gui*
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "black" :foreground "gainsboro" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "unknown" :family "Inconsolata"))))
   '(cursor ((t (:background "yellow" :foreground "black"))))
   '(fringe ((((class color) (background dark)) nil)))
   '(header-line ((((class color grayscale) (background dark)) (:inherit mode-line))))
   '(mode-line ((t (:background "black" :foreground "gainsboro"))))
   '(trailing-whitespace ((((class color) (background dark)) (:background "magenta")))))
  )

(global-font-lock-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; decorative types

(let ( ;( my-extra-types
		; '( "\\([UI]i*\\)nt\\(8\\|16\\|32\\|64\\)" "Index"
		;	"Float\\(32\\|64\\)" "sstring" "RE_\\(\\w*\\)Coord" ) )
	   ( my-mode-additions
		 '( ("\\<\\(TODO\\):" . font-lock-warning-face)
			("\\<\\(PROGRAMMING_ERROR\\|ASSERT\\w*\\|DEBUG_ABORT\\w*\\|BADPLACE\\w*\\)\\>" . font-lock-warning-face)
			("\\<\\(PRECONDITION\\|POSTCONDITION\\|CHECK_INVARIANT\\|REQUIRE\\|ENSURE\\|STATIC_CHECK\\)\\>" . font-lock-constant-face)
			) )
	   )
;  (setq c-font-lock-extra-types 
;		(append c-font-lock-extra-types my-extra-types))
;  (setq c++-font-lock-extra-types 
;		(append c++-font-lock-extra-types my-extra-types ) )

  (font-lock-add-keywords 'c-mode my-mode-additions)
  (font-lock-add-keywords 'c++-mode my-mode-additions)
  (font-lock-add-keywords 'objc-mode my-mode-additions)
)

(setq font-lock-support-mode 'jit-lock-mode)
(setq font-lock-maximum-decoration t)
(setq-default font-lock-multiline t)
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
