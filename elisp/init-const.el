;;; init.el --- Init File -*- lexical-binding: t -*-
;; Miscellaneous constants....

(defconst *user-home-directory*
  (if (getenv "HOME") 
   (getenv "HOME")
   (concat (expand-file-name "~") "/"))
  "Path to user home directory.")

(setq user-full-name "Kimball D. Thurston III"
      user-mail-address "kdt3rd@gmail.com"
      ;;user-login-name (getenv "USER") ; this is the default
      calendar-latitude -41.2865
      calendar-longitude 174.7762
      calendar-location-name "Wellington, New Zealand")
(defvar user-full-company-name (if (string= (user-login-name) "kthurston") "Wētā FX" nil)
  "Full company name")
(defvar user-ascii-company-name (if (string= (user-login-name) "kthurston") "Weta" nil)
  "ASCII-safe company name")

(setq system-name
      (replace-regexp-in-string
       "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" ;; like perl chomp()
       (with-output-to-string
         (call-process "/usr/bin/hostname" nil standard-output nil))))

(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(defconst *sys/gui* (display-graphic-p) "GUI Emacs")

(defconst *sys/win32* (eq system-type 'windows-nt) "windows")
(defconst *sys/linux* (eq system-type 'gnu/linux) "linux")
(defconst *sys/mac* (eq system-type 'darwin) "Mac OS/X")

;; some programs...
;(defconst *ag* (executable-find "ag") "the_silver_searcher")
(defconst *rg* (executable-find "rg") "ripgrep")
;(defconst *python* (executable-find "python") "python")
(defconst *python3* (executable-find "python3") "python3")
(defconst *tr* (executable-find "tr") "tr")
(defconst *clangd*
  (or (executable-find "clangd")
      (executable-find "/usr/local/opt/llvm/bin/clangd"))
  "clangd")
; we use the clang-format module, it finds it...
(defconst *clang-format* (executable-find "clang-format") "clang-format")
(defconst *clang* (executable-find "clang") "clang")
(defconst *gcc* (executable-find "gcc") "gcc")
(defconst *git* (executable-find "git") "git")
(defconst *zsh* (executable-find "zsh") "zsh")

(defvar my:fringe-width 6)

;(setq my:default-font "Inconsolata" :group kdt)
(defvar my:default-font "Hack")
(defvar my:default-font-size 14)
(defvar my:current-font-size my:default-font-size)
(defvar my:font-change-increment 1.1)

(provide 'init-const)
;;; init-const.el ends here
