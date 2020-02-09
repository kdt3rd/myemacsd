;; Miscellaneous constants....
(setq user-full-name "Kimball D. Thurston III"
      user-mail-address "kdt3rd@gmail.com"
      calendar-latitude -41.2865
      calendar-longitude 174.7762
      calendar-location-name "Wellington, New Zealand")

(defconst *sys/gui* (display-graphic-p) "GUI Emacs")

(defconst *sys/win32* (eq system-type 'windows-nt) "windows")
(defconst *sys/linux* (eq system-type 'gnu/linux) "linux")
(defconst *sys/mac* (eq system-type 'darwin) "Mac OS/X")

;; some programs...
(defconst *ag* (executable-find "ag") "the_silver_searcher")
(defconst *rg* (executable-find "rg") "ripgrep")
(defconst *python* (executable-find "python") "python")
(defconst *python3* (executable-find "python3") "python3")
(defconst *tr* (executable-find "tr") "tr")
(defconst *clangd*
  (or (executable-find "clangd")
      (executable-find "/usr/local/opt/llvm/bin/clangd"))
  "clangd")
(defconst *gcc* (executable-find "gcc") "gcc")
(defconst *git* (executable-find "git") "git")

(provide 'init-const)
