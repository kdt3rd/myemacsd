(eval-when-compile
  (require 'init-global-config)
  (require 'init-const))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package counsel
  :after ivy
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-s" . 'swiper)
  ("C-S-h" . 'counsel-rg)
  ;:config
  ;(use-package flx)
  ;(use-package smex)
  :hook (after-init . counsel-mode)
  ;;prot  (setq counsel-yank-pop-preselect-last t)
  ;;prot  (setq counsel-yank-pop-separator "\n—————————\n")
  ;;prot  (setq counsel-rg-base-command
  ;;prot        "rg -SHn --no-heading --color never --no-follow --hidden %s")
  ;;prot  (setq counsel-find-file-occur-cmd; TODO Simplify this
  ;;prot        "ls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 ls -d --group-directories-first")
  ;;prot  (defun prot/counsel-fzf-rg-files (&optional input dir)
  ;;prot    "Run `fzf' in tandem with `ripgrep' to find files in the
  ;;protpresent directory.  If invoked from inside a version-controlled
  ;;protrepository, then the corresponding root is used instead."
  ;;prot    (interactive)
  ;;prot    (let* ((process-environment
  ;;prot            (cons (concat "FZF_DEFAULT_COMMAND=rg -Sn --color never --files --no-follow --hidden")
  ;;prot                  process-environment))
  ;;prot           (vc (vc-root-dir)))
  ;;prot      (if dir
  ;;prot          (counsel-fzf input dir)
  ;;prot        (if (eq vc nil)
  ;;prot            (counsel-fzf input default-directory)
  ;;prot          (counsel-fzf input vc)))))
  ;;prot  (defun prot/counsel-fzf-dir (arg)
  ;;prot    "Specify root directory for `counsel-fzf'."
  ;;prot    (prot/counsel-fzf-rg-files ivy-text
  ;;prot                               (read-directory-name
  ;;prot                                (concat (car (split-string counsel-fzf-cmd))
  ;;prot                                        " in directory: "))))
  ;;prot  (defun prot/counsel-rg-dir (arg)
  ;;prot    "Specify root directory for `counsel-rg'."
  ;;prot    (let ((current-prefix-arg '(4)))
  ;;prot      (counsel-rg ivy-text nil "")))
  ;;prot  ;; TODO generalise for all relevant file/buffer counsel-*?
  ;;prot  (defun prot/counsel-fzf-ace-window (arg)
  ;;prot    "Use `ace-window' on `prot/counsel-fzf-rg-files' candidate."
  ;;prot    (ace-window t)
  ;;prot    (let ((default-directory (if (eq (vc-root-dir) nil)
  ;;prot                                 counsel--fzf-dir
  ;;prot                               (vc-root-dir))))
  ;;prot      (if (> (length (aw-window-list)) 1)
  ;;prot          (find-file arg)
  ;;prot        (find-file-other-window arg))
  ;;prot      (balance-windows (current-buffer))))
  ;;prot  ;; Pass functions as appropriate Ivy actions (accessed via M-o)
  ;;prot  (ivy-add-actions
  ;;prot   'counsel-fzf
  ;;prot   '(("r" prot/counsel-fzf-dir "change root directory")
  ;;prot     ("g" prot/counsel-rg-dir "use ripgrep in root directory")
  ;;prot     ("a" prot/counsel-fzf-ace-window "ace-window switch")))
  ;;prot  (ivy-add-actions
  ;;prot   'counsel-rg
  ;;prot   '(("r" prot/counsel-rg-dir "change root directory")
  ;;prot     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))
  ;;prot  (ivy-add-actions
  ;;prot   'counsel-find-file
  ;;prot   '(("g" prot/counsel-rg-dir "use ripgrep in root directory")
  ;;prot     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))
  ;;prot  ;; Remove commands that only work with key bindings
  ;;prot  (put 'counsel-find-symbol 'no-counsel-M-x t)
  ;;prot  :bind (("M-x" . counsel-M-x)
  ;;prot         ("C-x C-f" . counsel-find-file)
  ;;prot         ("s-f" . counsel-find-file)
  ;;prot         ("s-F" . find-file-other-window)
  ;;prot         ("C-x b" . ivy-switch-buffer)
  ;;prot         ("s-b" . ivy-switch-buffer)
  ;;prot         ("C-x B" . counsel-switch-buffer-other-window)
  ;;prot         ("s-B" . counsel-switch-buffer-other-window)
  ;;prot         ("C-x d" . counsel-dired)
  ;;prot         ("s-d" . counsel-dired)
  ;;prot         ("s-D" . dired-other-window)
  ;;prot         ("C-x C-r" . counsel-recentf)
  ;;prot         ("s-m" . counsel-mark-ring)
  ;;prot         ("s-r" . counsel-recentf)
  ;;prot         ("s-y" . counsel-yank-pop)
  ;;prot         ("C-h f" . counsel-describe-function)
  ;;prot         ("C-h v" . counsel-describe-variable)
  ;;prot         ("M-s r" . counsel-rg)
  ;;prot         ("M-s g" . counsel-git-grep)
  ;;prot         ("M-s l" . counsel-find-library)
  ;;prot         ("M-s z" . prot/counsel-fzf-rg-files)
  ;;prot         :map ivy-minibuffer-map
  ;;prot         ("C-r" . counsel-minibuffer-history)
  ;;prot         ("s-y" . ivy-next-line)        ; Avoid 2× `counsel-yank-pop'
  ;;prot         ("C-SPC" . ivy-restrict-to-matches))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-counsel)

