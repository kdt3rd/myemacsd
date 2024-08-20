;;; init-programming-functions.el --- Initialize misc prog functions -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VERY VERY USEFUL FOR DEBUGGING...
;;		(send-string-to-terminal "I'm here")
;;
;;

;; define some global overrideable variables we'll use
(defvar source-default-header-ext "h")
(defvar source-default-source-ext "cpp")
(defvar source-header-ext-regexp "\\.\\(hpp\\|h\\|h2\\|hh\\|H\\|inline\\|inl\\|tcc\\)$")
(defvar source-header-extension-list '("h" "hh" "h2" "H" "hpp" "inline" "inl" "tcc"))
(defvar source-source-ext-regexp "\\.\\(cpp\\|cu\\|c\\|cc\\|C\\|m\\|mm\\|M\\|MM\\)$")
(defvar source-source-extension-list '("cpp" "c" "cu" "cc" "cpp2" "m" "mm" "C"))
(defvar source-header-extra-path '("/../inc/" "/../include/"))
(defvar source-source-extra-path '("/../src/"))

;; Default regexp for includes
(defvar project-c++-include-regexp "#include[ \t]+\\(\\(<[^>]*>\\)\\|\\(\"[^\"]*\"\\)\\)[ \t]*\n")
;; Default regexp for class declarations
(defvar project-c++-class-decl-regexp "class[ \t]+\\([A-Za-z][A-Za-z0-9_]*\\);[ \t]*\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for moving around the file(s)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'comint)
;;(require 'ansi-color)
;;(ansi-color-for-comint-mode-on)
;;
;;(defconst ansi-code-ignore-regexp "\033\\[[K]+")
;;(defun ansi-code-ignore (string)
;;  "Skip some (irrelevant) ansi escape codes"
;;  (let ((start-marker (or comint-last-output-start
;;			  (point-min-marker)))
;;		(end-marker (process-mark (get-buffer-process (current-buffer)))))
;;	(save-excursion
;;	  (goto-char start-marker)
;;	  (while (re-search-forward ansi-code-ignore-regexp end-marker t)
;;		(replace-match "")))))
;;
;;(add-hook 'comint-output-filter-functions 'ansi-code-ignore)


;;(defun my-find-string-in-files-ack (p dir)
;;   (interactive "sPattern to find: \nDDirectory to start search for %s: ")
;;   (let ((buffer-name "*Find Results*"))
;;	 (make-comint-in-buffer "Ack" buffer-name "/bin/zsh" nil "-c" (concat "cd " dir "; ack --color --heading --break " p))
;;	 (switch-to-buffer buffer-name)
;;	 ))
;;
;;(defun my-find-string-in-files-rg (p dir)
;;   (interactive "sPattern to find: \nDDirectory to start search for %s: ")
;;   (let ((buffer-name "*Find Results*"))
;;	 (make-comint-in-buffer "File Search" buffer-name *rg* nil "-p" p dir)
;;	 (switch-to-buffer buffer-name)
;;	 ))

(defun find-source-root-dir (d)
  "Finds last directory w/ makefile in it"
  (if (or (not d) (string-equal "/home" d))
	  nil
	(let ((curdir (directory-file-name (file-name-directory d))))
	  (if (or (file-exists-p (concat curdir "/Makefile"))
			  (file-exists-p (concat curdir "/makefile")))
		  curdir
		(find-source-root-dir curdir)))))

(defun find-source-root ()
  "Starting at the current buffers directory, finds the last one with makefile in it"
  (find-source-root-dir (buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun toggle-tab-size ()
  "Toggles between tab size of 4 and 8"
  (interactive)
  (if (eq tab-width '4) (set-variable 'tab-width 8)
	  (set-variable 'tab-width 4)))

; if our source file uses tabs, we use tabs, if spaces spaces, and if
; neither, we use the current indent-tabs-mode
(defun infer-tabs-style ()
  "Chooses tabs or spaces based on how many there are in the file"
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Custom function to switch between files
;;
(defun toggle-source-header()
  "Switches to the source buffer if currently in the header buffer and vice versa."
  (interactive)
  (let ((name (file-name-nondirectory (buffer-file-name)))
		(filesansext (file-name-sans-extension (buffer-file-name))))
	(let ((dirpart (file-name-directory filesansext)))
	  (let ((subdir (file-name-nondirectory (substring dirpart 0 -1)))
			(basedir (file-name-directory (substring dirpart 0 -1)))
			(basepart (file-name-sans-extension name))
			openfilename
			offs)
		(setq offs (string-match source-header-ext-regexp name))
		(if offs
			(let ((lst source-source-extension-list)
				  (ok nil)
				  (tmpbuf nil)
				  ext)
			  (while (and lst (not ok))
				(setq ext (car lst))
				(setq openfilename (concat filesansext "." ext))
;				(send-string-to-terminal (concat openfilename "\n"))
				(setq tmpbuf (get-buffer (file-name-nondirectory openfilename)))
				(if tmpbuf
					(setq ok t)
				  (if (file-exists-p openfilename)
					  (setq ok t)
					(let ()
					  (setq openfilename (concat (file-name-directory (substring basedir 0 -1)) "/" subdir "/" basepart "." ext))
					  (setq tmpbuf (get-buffer (file-name-nondirectory openfilename)))
					  (if tmpbuf
						  (setq ok t)
						(if (file-exists-p openfilename)
							(setq ok t))))))
				(let ((xxx source-source-extra-path)
					  curexdir)
				  (while (and xxx (not ok))
					(setq openfilename (concat dirpart basepart "." ext))
					(if (file-exists-p openfilename)
						(setq ok t))
					(setq xxx (cdr xxx))
					)
				  )
				  (setq lst (cdr lst)))
			  (if tmpbuf
				  (switch-to-buffer tmpbuf)
				(if ok
					(find-file openfilename))))
		  (let ()
			(setq offs (string-match source-source-ext-regexp name))
			(if offs
				(let ((lst source-header-extension-list)
					  (ok nil)
					  (tmpbuf nil)
					  ext)
				  (while (and lst (not ok))
					(setq ext (car lst))
					(setq openfilename (concat filesansext "." ext))
					(setq tmpbuf (get-buffer (file-name-nondirectory openfilename)))
					(if tmpbuf
						(setq ok t)
					  (if (file-exists-p openfilename)
						  (setq ok t)
						(let ()
						  (setq openfilename (concat basedir "/include/" subdir "/" basepart "." ext))
						  (setq tmpbuf (get-buffer (file-name-nondirectory openfilename)))
						  (if tmpbuf
							  (setq ok t)
							(if (file-exists-p openfilename)
								(setq ok t))))))
					(let ((xxx source-header-extra-path)
						  curexdir)
					  (while (and xxx (not ok))
						(setq openfilename (concat dirpart basepart "." ext))
						(if (file-exists-p openfilename)
							(setq ok t))
						(setq xxx (cdr xxx))
						)
					  )
					(setq lst (cdr lst)))
				  (if tmpbuf
					  (switch-to-buffer tmpbuf)
					(if ok
						(find-file openfilename)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When TAB is hit, do the dynamic completion if next to a character,
;; otherwise do the normal thing...
(defun indent-or-expand (&optional pre-arg)
  "Either expand the string, jump to the next expand pos, or insert or expand"
  (interactive)
  (cond ((and (= ?w (char-syntax (preceding-char)))
			  (or (eq nil (char-after))
				  (not (= ?w (char-syntax (char-after))))))
		 (hippie-expand pre-arg)
		 (if (and (= he-num -1)
				  (not (eq nil my-template-jump-positions)))
			 (goto-char (+ (point) (pop my-template-jump-positions)))))
		((not (eq nil my-template-jump-positions))
		 (goto-char (+ (point) (pop my-template-jump-positions))))
		((> (current-column) (current-indentation))
		 (indent-relative))
		(t (indent-according-to-mode)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add a string in front of all lines in the region
(defun prepend-string-to-range (start end s)
  "Add a string in front of all lines in the region."
  (interactive "*r\nMEnter a string: ")
  (save-excursion
    (save-restriction
      (narrow-to-region
       (progn (goto-char start) (beginning-of-line) (point))
       (progn (goto-char end) (end-of-line) (point)))
      (goto-char (point-min))
	  (beginning-of-line)
      (while (not (eobp))
        (insert s)
        (forward-line 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove a string from the beginning of all lines in the region
(defun unprepend-string-to-range (start end s)    
  "Remove a string from the front of all lines in the region."
  (interactive "*r\nMEnter a string: ")
  (save-excursion
    (save-restriction
      (narrow-to-region
       (progn (goto-char start) (beginning-of-line) (point))
       (progn (goto-char end) (end-of-line) (point)))
      (goto-char (point-min))
	  (beginning-of-line)
      (while (not (eobp))
        (if (looking-at (regexp-quote s))
            (delete-region (match-beginning 0) (match-end 0)))
        (forward-line 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add a string to the end of all lines in the region
(defun append-string-to-range (start end s)
  "Add a string to the end of all lines in the region."
  (interactive "*r\nMEnter a string: ")
  (save-excursion
    (save-restriction
      (narrow-to-region
       (progn (goto-char start) (beginning-of-line) (point))
       (progn (goto-char end) (end-of-line) (point)))
      (goto-char (point-min))
      (end-of-line)
      (while (not (eobp))
        (insert s)
        (forward-line 1)
		(end-of-line)))))

;; remove a string from the beginning of all lines in the region
(defun unappend-string-to-range (start end s)
  "Remove a string from the end of all lines in the region."
  (interactive "*r\nMEnter a string: ")
  (save-excursion
    (save-restriction
      (narrow-to-region
       (progn (goto-char start) (beginning-of-line) (point))
       (progn (goto-char end) (end-of-line) (point)))
      (goto-char (point-min))
      (end-of-line)
      (while (not (eobp))
        (if (looking-back (regexp-quote s) (length s))
            (delete-region (match-beginning 0) (match-end 0)))
        (forward-line 1)
        (end-of-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inside-string-or-comment (&optional pos)
  "inside a C / C++ string or comment"
  (interactive)
  (unless pos (setq pos (point)))
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (or (member 'font-lock-string-face fontfaces)
        (member 'font-lock-comment-face fontfaces)
        (member 'font-lock-doc-face fontfaces)
        (member 'font-lock-comment-delimiter-face fontfaces))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-comment-if-not-inside ()
  "Returns a comment start char if not already in a comment"
  (interactive)
  (if (inside-string-or-comment) ""
    (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Insert a standard comment break
;;
(defun insert-fn-separator ()
  "Insert the standard fn separator"
  (interactive)
  (cond ((or (eq major-mode 'c++-mode)
			 (eq major-mode 'objc-mode))
		 (insert "\n////////////////////////////////////////\n" ))
		((eq major-mode 'c-mode)
		 (insert "\n/**************************************/\n" ))
		((or (eq major-mode 'lisp-mode)
             (derived-mode-p 'emacs-lisp-mode))
		 (insert "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n" ))
        (t (insert "\n########################################\n" ))
		)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helper--start-new-file ()
  "implementation of start-new-file"
  (mark-whole-buffer)
  (cond ((string-match source-source-ext-regexp (buffer-file-name))
         (if-let (template (alist-get 'source_template (tempel--templates)))
             (tempel-insert template)
           (user-error "Template source_template not found")))
		((string-match source-header-ext-regexp (buffer-file-name))
         (if-let (template (alist-get 'header_template (tempel--templates)))
             (tempel-insert template)
           (user-error "Template header_template not found")))
        (t
         (if-let (template (alist-get 'file_template (tempel--templates)))
             (tempel-insert template)
           (user-error "Template file_template not found")))
        )
  )

(defun start-new-file ()
  "Inserts boiler plate for a new file"
  (interactive)
  (if (= (buffer-size (current-buffer)) 0)
      (helper--start-new-file)
    (save-excursion (helper--start-new-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-header-def-from-buffer ()
  "Returns a string of the form _BUFNAME_EXT_"
  (replace-regexp-in-string
   "^[0-9]" "_"
   (replace-regexp-in-string
    "[^a-zA-Z0-9]" "_"
    (upcase
     (let (
           (pname (if (fboundp 'projectile-project-name) (projectile-project-name) ""))
           (dname (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
           )
       (concat
        (if (bound-and-true-p user-ascii-company-name) (concat user-ascii-company-name "_") "")
        (if (> (length pname) 0) (concat pname "_") "")
        (if (string= pname dname) "" (concat dname "_"))
        (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
        "_H")
       )
     )
    )
   )
  )

(defun test-header-def ()
  "Inserts boiler plate for a new file"
  (interactive)
  (save-excursion
    (insert (get-header-def-from-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-programming-functions)
