;;; init-functions.el --- misc support functions -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

;;;; if we hit kill buffer, why wouldn't we want to kill the current buffer???
(defun my:kill-current-buffer ()
    "Kill the current buffer without prompting."
    (interactive)
    ;;(kill-buffer (current-buffer))
    (kill-buffer (buffer-name))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-to-list* (list-var &rest elts)
  "Add `ELTS' to `LIST-VAR'."
  (dolist (elt elts)
    (add-to-list list-var elt)))

(defun append-to-list* (list-var &rest elts)
  "Append `ELTS' to `LIST-VAR'."
  (dolist (elt elts)
    (add-to-list list-var elt t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro user/filter-form (form list)
  "Return list with elements for which FORM are non-nil in LIST."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (when ,form (!cons it ,r)))
       (nreverse ,r))))

(defun list-toggle-element (list element)
  "Return LIST with ELEMENT removed if present or added if not present."
  (if (member element list)
      (user/filter-form (not (eq element it)) list)
    (cons element list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro try-eval (fn &optional finally)
  "Safely evaluate expression FN and run FINALLY after."
  (declare (debug t)
           (indent 1))
  `(let (retval)
     (condition-case-unless-debug ex
         (setq retval (progn ,fn))
       ('error
        (setq retval (cons 'exception (list ex)))))
     ,@finally
     retval))

(defun feature-p (feature)
  "Check if FEATURE is available."
  (or (featurep feature)
      (when (functionp 'package-installed-p)
        (package-installed-p feature))
      (locate-library (symbol-name feature))))

(defun add-command-switch (handler &rest switch-list)
  "Add HANDLER for SWITCH-LIST."
  (dolist (switch switch-list)
    (add-to-list 'command-switch-alist (cons switch handler))))

(defun add-auto-mode (mode &rest patterns)
  "Use `MODE' for all given files matching `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun add-magic-mode (mode &rest patterns)
  "Use `MODE' for all files containing header `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'magic-mode-alist (cons pattern mode))))

(defun add-interpreter-mode (mode &rest interpreters)
  "Use `MODE' for all files with shebang `INTERPRETERS'."
  (dolist (interpreter interpreters)
    (add-to-list 'interpreter-mode-alist (cons interpreter mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun path-abs-buffer ()
  "Get the current buffer absolute path."
  (file-truename (or (buffer-file-name) default-directory)))


(defun path-dirname (path)
  "Get the parent directory of PATH."
  (file-name-directory (directory-file-name path)))


(defun path-join (root &rest dirs)
  "Join paths together starting at ROOT and proceeding with DIRS.
Ex: (path-join \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'path-join
           (expand-file-name (car dirs) root)
           (cdr dirs))))

(defun load-all-files-from-dir (dir)
  "Load all Emacs Lisp files in DIR."
  (dolist (f (directory-files dir))
    (when (and
           (file-directory-p (path-join dir f))
           (not (string= "." f))
           (not (string= ".." f)))
      (load-all-files-from-dir (path-join dir f)))
    (when (and
           (not (file-directory-p (path-join dir f)))
           (not (string= "bootstrapper.el" f))
           (not (string= ".#" (substring f 0 2)))
           (string= ".el" (substring f (- (length f) 3))))
      (load-file (path-join dir f)))))

(defun osx-app-installed-p (app)
  "Return t if APP is installed."
  (when *sys/mac*
    (let ((lsregister
           "/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister"))
      (and (file-executable-p lsregister)
         (not (string-equal "" (shell-command-to-string
                              (concat lsregister " -dump|grep " app))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getenv-or (env value)
  "Fetch the value of ENV or, if it is not set, return VALUE."
  (if (getenv env)
      (getenv env)
    value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dos2unix ()
  "convert a buffer from dos ^M end of lines to unix end of lines"
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  "convert a buffer from unix end of lines to dos ^M end of lines"
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-trailing-whitespace-except-current-line ()
  "Sometimes `delete-trailing-whitespace' becomes very annoying when editing a line, avoid changing the current one"
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) begin)
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) end)
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(defun smart-delete-trailing-whitespace ()
  "Invoke `delete-trailing-whitespace-except-current-line' on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))

(defun toggle-auto-trailing-ws-removal ()
  "Toggle trailing whitespace removal."
  (interactive)
  (if (member #'smart-delete-trailing-whitespace before-save-hook)
      (progn
        (remove-hook 'before-save-hook #'smart-delete-trailing-whitespace)
        (message "Disabled auto remove trailing whitespace."))
    (add-hook 'before-save-hook #'smart-delete-trailing-whitespace)
    (message "Enabled auto remove trailing whitespace.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my:toggle-line-numbers (&optional relative)
  "toggle line number display"
  (interactive)
  (cond (display-line-numbers (display-line-numbers-mode nil)
                              (setq display-line-numbers nil))
        (t (display-line-numbers-mode t)
           (setq display-line-numbers (or relative t)))))

(defun my:toggle-line-numbers-relative ()
  "toggle line number display with relative line numbers"
  (interactive)
  (my:toggle-line-numbers 'relative))

(defun my:goto-line-with-feedback ()
  "show line numbers while prompting for line number"
  (interactive)
  (let ((line-numbers-off-p (not display-line-numbers)))
    (unwind-protect
        (progn (when line-numbers-off-p
                 (my:toggle-line-numbers))
               (call-interactively 'goto-line))
      (when line-numbers-off-p
        (my:toggle-line-numbers)))))

(provide 'init-functions)
;;; init-functions.el ends here
