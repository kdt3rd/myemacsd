;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VERY VERY USEFUL FOR DEBUGGING...
;;		(send-string-to-terminal "I'm here")
;;
;;

;; define some global overrideable variables we'll use
(defvar source-default-header-ext "h")
(defvar source-default-source-ext "cpp")
(defvar source-header-ext-regexp "\\.\\(hpp\\|h\\|\hh\\|H\\|inline\\|inl\\|tcc\\)$")
(defvar source-header-extension-list '("h" "hh" "H" "hpp" "inline" "inl" "tcc"))
(defvar source-source-ext-regexp "\\.\\(cpp\\|cu\\|c\\|cc\\|C\\|m\\|mm\\|M\\|MM\\)$")
(defvar source-source-extension-list '("cpp" "cu" "cc" "m" "mm" "C" "c" ))
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

(require 'comint)
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

(defconst ansi-code-ignore-regexp "\033\\[[K]+")
(defun ansi-code-ignore (string)
  "Skip some (irrelevant) ansi escape codes"
  (let ((start-marker (or comint-last-output-start
			  (point-min-marker)))
		(end-marker (process-mark (get-buffer-process (current-buffer)))))
	(save-excursion
	  (goto-char start-marker)
	  (while (re-search-forward ansi-code-ignore-regexp end-marker t)
		(replace-match "")))))

(add-hook 'comint-output-filter-functions 'ansi-code-ignore)


(defun my-find-string-in-files (p dir)
   (interactive "sPattern to find: \nDDirectory to start search for %s: ")
   (let ((buffer-name "*Find Results*"))
	 (make-comint-in-buffer "Ack" buffer-name "/bin/zsh" nil "-c" (concat "cd " dir "; ack --color --heading --break " p))
	 (switch-to-buffer buffer-name)
	 ))

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


;; (defun bounce-sexp ()
;;   "Will bounce between matching parens just like % in vi"
;;   (interactive "^p")
;;   (let ((prev-char (char-to-string (preceding-char)))
;; 		(next-char (char-to-string (following-char))))
;; 	(cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
;; 		  ((string-match "[\]})>]" prev-char) (backward-sexp 1))
;; 		  (t (error "%s" "Not on a paren, brace, or bracket")))))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for cleaning up source code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun global-cleanup-file ()
  "Perform a general cleanup of a source or header file"
  (interactive)
  (save-excursion
	(buffer-disable-undo)

	; fix up any if( with if (
	(beginning-of-buffer)
	(replace-regexp "\\([\t ]\\)\\(if\\|while\\|for\\|switch\\)(" "\\1\\2 (" )

	; fix up any ifndef - define header wrappers so they match the filename
;	(if (string-match source-header-ext-regexp (buffer-name))
;		(let ((bufname (buffer-name))
;			  defname
;			  defstring)
;		  (setq defname (concat "__" (file-name-sans-extension bufname) "_"
;								(file-name-extension bufname) "__" ))
;		  (setq defstring (concat
;						   "#ifndef " defname "\n"
;						   "#define " defname "\n"))
;		  (beginning-of-buffer)
;		  (if (search-forward-regexp (concat 
;									  "^#ifndef[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*[\n]"
;									  "#define[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*[\n]")
;									 nil t)
;			  (replace-match defstring t t ))))

;	(beginning-of-buffer)
;	(replace-regexp "^\\([ ]*\\)\t" "\\1    ")

	; clean up any blank lines with white space only
	(beginning-of-buffer)
	(replace-regexp "^[\t ]+$" "")

;;	(beginning-of-buffer)
;;	(replace-string "OS_" "PC")

	; cleanup old macros and stuff
;;	(beginning-of-buffer)
;;	(replace-regexp "NOTNULL([\t ]*\\([^\t )]+\\)[\t ]*)" "\\1" )
	;;;;;;; NEED MORE HERE...

	(buffer-enable-undo)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; find-assignment and align-equals align assignment statements for a region
;; originally from Paul Hudson
(defun find-assignment ()
  (if (re-search-forward
	   "[^<>=!]=\\|\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>="
	   (save-excursion (end-of-line) (point)) t)
      (progn
		(goto-char (match-beginning 0))
		(if (looking-at ".==")
			nil
		  (if (looking-at "\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>=")
			  (set-mark (match-end 0))
			(forward-char 1)
			(set-mark (1+ (point))))
		  (delete-horizontal-space)
		  t))
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun align-equals (start end)
  "make the first assignment operator on each line line up vertically"
  (interactive "*r")
  (save-excursion
	(let ((indent 0))
	  (indent-region start end)
	  (narrow-to-region start end)
	  (beginning-of-buffer)
	  (while (not (eobp))
		(if (find-assignment)
			(progn
			  (exchange-point-and-mark)
			  (setq indent (max indent (current-column)))
			  (delete-horizontal-space)
			  (insert " ")))
		(forward-line 1))
	  (beginning-of-buffer)
	  (while (not (eobp))
		(if (find-assignment)
			(indent-to-column (1+ (- indent  (- (mark) (point))))))
		(forward-line 1)))
	(widen)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun align-vars (start end)
  "Aligns c/c++ variable declaration names on the same column, with beginning and end taken from selected region."
  (interactive "*r")
  (save-excursion
    (let (bol eol expr-end
			  (max-col 0) col
			  (max-ptr 0)
			  poslist curpos)
      (goto-char end)
      (if (not (bolp))
		  (setq end (line-end-position)))
      (goto-char start)
      (while (and (> end (point)) (not (eobp)))
		(setq bol (line-beginning-position))
		(setq eol (line-end-position))
		(beginning-of-line)
		(setq expr-end (point))
		(if (search-forward-regexp (concat "^[ \t]*\\("
										   "[a-zA-Z\\_][a-zA-Z0-9\\_:]*\\|"
										   "[a-zA-Z\\_][a-zA-Z0-9\\_:]*[ \t]*<[^>]+>\\|"
										   "\\)[ \t]+[^;]") eol t)
			(let ()
			  (setq expr-end (match-end 1))
			  (while (search-forward-regexp "\\([a-zA-Z][a-zA-Z0-9\\_:]*\\(<[^>]+>\\)?\\)[ \t]+[^;=]" eol t)
				(setq expr-end (match-end 1)))
			  (goto-char expr-end)
			  (setq col (current-column))
			  (if (search-forward-regexp (concat "\\([\\*&]+[ \t]*\\)?"
												 "\\([a-zA-Z\\_][a-zA-Z0-9\\_]*\\)[ \t]*\\(\\[[^\\]]+\\]\\)?\\([ \t]*=[ \t]*[^;,]+\\)?"
												 "\\([ \t]*,[ \t]*\\([a-zA-Z\\_][a-zA-Z0-9\\_]*\\)[ \t]*\\(\\[[^\\]]+\\]\\)?\\([ \t]*=[ \t]*[^;,]+\\)?\\)*"
												 "[ \t]*;$") eol t)
;				  (let ((name-col-end 0))
				  (let ((name-col-end (- (match-beginning 2) (match-beginning 0))))
					(if (> name-col-end max-ptr)
						(setq max-ptr name-col-end))
;					(message (format "name-col-end %d max-ptr %d" name-col-end max-ptr))
					(setq poslist (cons (list t expr-end col (match-beginning 0) name-col-end) poslist))
					(if (> col max-col)
						(setq max-col col))
					(beginning-of-next-line))
				(let ()
				  (setq poslist (cons (list nil nil nil nil nil) poslist))
				  (message "check for rest of line failed" )
				  (beginning-of-next-line))))
		  (let ()
			(setq poslist (cons (list nil nil nil nil nil) poslist))
			(message "initial check for var decl failed" )
			(beginning-of-next-line))))
      (setq curpos poslist)
      (while curpos
		(let* ((pos (car curpos))
			   (ok (car pos))
			   (col (car (cdr (cdr pos))))
			   (col-end (car (cdr (cdr (cdr pos)))))
			   (col-end-name (car (cdr (cdr (cdr (cdr pos))))))
			   (abs-pos (car (cdr pos))))
		  (if ok
			  (let ()
				(goto-char abs-pos)
				(delete-region abs-pos col-end)
				(insert-string (make-string (+ (+ (- max-col col) 1) (- max-ptr col-end-name)) 32)))))
		(setq curpos (cdr curpos))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Custom function to get rid of spaces at the end of all lines
;;
(defun remove-eol-spaces ()
  "Remove spaces from the end of lines."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(replace-regexp "[\t ]+$" "")))

;;
;; Custom function to get rid of spaces at the end of all lines
;;
(defun remove-space-tabs ()
  "Remove space-tab pairs from lines."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(replace-regexp " \t" "\t" )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Program editing convienence functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
; UNFINISHED
;(defun unappend-string-to-range (start end s)    
;  "Remove a string from the end of all lines in the region."
;  (interactive "*r\nMEnter a string: ")
;  (save-excursion
;    (save-restriction
;      (narrow-to-region
;       (progn (goto-char start) (beginning-of-line) (point))
;       (progn (goto-char end) (end-of-line) (point)))
;      (goto-char (point-min))         
;      (while (not (eobp))
;        (if (looking-at (regexp-quote s))
;            (delete-region (match-beginning 0) (match-end 0)))
;        (forward-line 1)))))


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
		((or (eq major-mode 'perl-mode)
			 (eq major-mode 'python-mode))
		 (insert "\n########################################\n" ))
		((eq major-mode 'c-mode)
		 (insert "\n/**************************************/\n" ))
		((eq major-mode 'lisp-mode)
		 (insert "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n" ))
		)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Insert the beginnings of a new class function
;;
(defun insert-classfn-separator ()
  "Insert the standard fn separator"
  (interactive)
  (cond ((eq major-mode 'c++-mode)
		 (insert "\n////////////////////////////////////////\n" )
		 (let ((namesp (get-namespace-from-buffer))
			   (classnm (get-class-name-from-file)))
		   (insert (concat namesp "::" classnm "::"))
		   (beginning-of-line)
		   (previous-line 1)
		 ))
		((eq major-mode 'objc-mode)
		 (insert "\n////////////////////////////////////////\n" ))
		((eq major-mode 'perl-mode)
		 (insert "\n########################################\nsub " ))
		((eq major-mode 'python-mode)
		 (insert "\n########################################\ndef " ))
		)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Insert braces and leave point in middle
;;
(defun insert-braces ()
  "Insert matched braces, leave point inside."
  (interactive "*")
  (let (blink-paren-function) ;nil it temporarily
    (execute-kbd-macro
     (if (and (eq major-mode 'cc-c++-mode) (not (looking-at ";")))
         "{};" "{}")))
  (backward-sexp 1)
  (if (save-excursion
        (forward-char -1)
        (looking-at "\\$"))
      nil
    (reindent-then-newline-and-indent)
    (c-indent-exp)
    (forward-char 1)
    (newline-and-indent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Standard file starter...
;;
(defun start-comment-block ()
  (cond ((eq major-mode 'c++-mode)
		 t) ; (insert "//\n")
		((or (eq major-mode 'perl-mode)
		     (eq major-mode 'python-mode))
		 (insert "#\n"))
		((eq major-mode 'c-mode)
		 (insert "/*\n"))
		((eq major-mode 'lisp-mode)
		 (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"))
		)
  )

(defun comment (s)
  (cond ((eq major-mode 'c++-mode)
		 (insert "//" (if (string-equal "\n" s) s (concat " " s))))
		((or (eq major-mode 'perl-mode)
		     (eq major-mode 'python-mode))
		 (insert "#" (if (string-equal "\n" s) s (concat " " s))))
		((eq major-mode 'c-mode)
		 (insert " *" (if (string-equal "\n" s) s (concat " " s))))
		((eq major-mode 'lisp-mode)
		 (insert ";" (if (string-equal "\n" s) s (concat " " s))))
		)
  )

(defun end-comment-block ()
  (cond ((eq major-mode 'c++-mode)
		 t) ;(insert "//\n")
		((or (eq major-mode 'perl-mode)
		     (eq major-mode 'python-mode))
		 (insert "#\n"))
		((eq major-mode 'c-mode)
		 (insert " */\n"))
		((eq major-mode 'lisp-mode)
		 (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"))
		)
  )

(defun start-doxygen-block ()
  (cond ((eq major-mode 'c++-mode)
		 (insert "///\n"))
		((eq major-mode 'c-mode)
		 (insert "/**\n"))
		(t (start-comment-block)))
  )

(defun doxygen (s)
  (cond ((eq major-mode 'c++-mode)
		 (insert "///" (if (string-equal "\n" s) s (concat " " s))))
		(t (comment s)) )
  )

(defun end-doxygen-block ()
  (cond ((eq major-mode 'c++-mode)
		 (insert "///\n"))
		(t (end-comment-block)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun insert-copyright ()
  "Inserts standard stuff at top of file"
  (interactive)

  (start-comment-block)
  (comment "Copyright (c) 2019 Kimball Thurston\n")
  (comment "SPDX-License-Identifier: MIT\n")
  (end-comment-block)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun insert-copyright-footer ()
  "Inserts standard stuff at top of file"
  (interactive)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun start-new-file ()
  "Inserts standard stuff at top of file"
  (interactive)

  (let ( (basefile (file-name-nondirectory (buffer-file-name)))
		 (namesp (get-namespace-from-buffer))
		 (classnm (get-class-name-from-file))
		 )
;	(cond ((eq major-mode 'c++-mode)
;		   (insert (concat "// " basefile " -*- C++ -*-\n\n"))))

	(insert-copyright)

    (cond ((string-match source-source-ext-regexp (buffer-file-name))
		   (insert "\n#include \"" (file-name-sans-extension basefile) ".h\"\n")
		   (insert-fn-separator)
		   (insert (concat "namespace " namesp "\n{\n"))
		   (insert-fn-separator)
		   (insert (concat classnm "::" classnm "( void )\n{\n}\n" ))
		   (insert-fn-separator)
		   (insert (concat classnm "::~" classnm "( void )\n{\n}\n" ))
		   (insert-fn-separator)
		   (insert (concat "} // " namesp "\n\n"))
		   )
		  ((string-match source-header-ext-regexp (buffer-file-name))
;		   (insert (concat "\n#pragma once\n#ifndef " (get-header-def-from-buffer) "\n#define " (get-header-def-from-buffer) " 1\n"))
		   (insert "\n#pragma once\n")
		   (insert-fn-separator)
;		   (start-doxygen-block)
;		   (doxygen (concat "@file " (file-name-nondirectory (buffer-file-name)) "\n"))
;		   (doxygen "\n")
;		   (doxygen "@author Kimball Thurston\n")
;		   (end-doxygen-block)
		   (insert (concat "\nnamespace " namesp "\n{\n\n"))
		   (start-doxygen-block)
		   (doxygen (concat "@brief Class " classnm " provides...\n"))
		   (end-doxygen-block)
		   (insert (concat "class " classnm "\n{\npublic:\n" ))
		   (insert (concat "	" classnm "( void );\n"))
		   (insert (concat "	virtual ~" classnm "( void );\n"))
		   (insert "private:\n")
;		   (insert (concat "\n};\n\n} // namespace " namesp "\n\n#endif // " (get-header-def-from-buffer) "\n" ))
		   (insert (concat "\n};\n\n} // namespace " namesp "\n\n"))
		   )
		  (t
		   (start-comment-block)
		   (comment (concat "Filename: " basefile "\n"))
		   (comment "\n")
		   (comment "Author: Kimball Thurston\n")
		   (comment "\n")
		   (end-comment-block))
		  )
	)

  (insert "\n\n")
  (insert-copyright-footer)

  ;; Goto a line about where we would start editing... :)
  (if (eq major-mode 'c++-mode)
	  (if (string-match source-source-ext-regexp (buffer-file-name))
		  (goto-line 39)
		(goto-line 34))
	(goto-line 22))
  (end-of-line)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Insert the class name based on the file name
;;
(defun insert-class-name ()
  "inserts the class name based on the .C or .h filename"
  (interactive)
  (setq file (file-name-nondirectory (buffer-file-name)))
  (cond ((setq suffix (string-match "\\.\\([Ch]\\|cpp\\|cc\\)$" file))
         (insert (substring file 0 suffix))
         )
        ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Get the class name of the class we are in
;; From Elijah Daniels
;;
(defun get-class-name-from-def ()
  "If the point is in a class definition, get the name of the class.  Return
nil otherwise."
  (save-excursion
    (let ((brace (assoc 'inclass (c-guess-basic-syntax))))
      (if (null brace) '()
        (goto-char (cdr brace))
        (let ((class-open (assoc 'class-open (c-guess-basic-syntax))))
          (if class-open (goto-char (cdr class-open)))
          (if (looking-at "^class[ \t]+\\([A-Za-z_][^ \t:{]*\\)")
              (buffer-substring (match-beginning 1) (match-end 1))
            (error "Error parsing class definition!")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-class-name-from-file ()
  "get the class name from the file name"
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-header-def-from-buffer ()
  "Returns a string of the form _BUFNAME_EXT_"
;  (concat "_" (upcase (file-name-sans-extension
;					   (file-name-nondirectory (buffer-file-name)))) "_"
;					   (upcase (file-name-extension (buffer-file-name))) "_" )
  (let ( (classnm (get-class-name-from-file))
		 (namesp (get-namespace-from-buffer)) )
	  (concat "_" namesp "_" classnm "_h_"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-namespace-from-buffer ()
  (let (( namesp (file-name-nondirectory
				  (directory-file-name
				   (file-name-directory (buffer-file-name))))))
	(cond ((string-match "comp_.$" namesp)
		   "comp")
		  ((string-equal "graph_view" namesp)
		   "graph")
		  (t namesp)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Removes virtual and static from a type
(defun string-remove-type ( str reg )
  (interactive)
  (let (tmp)
    (if (eq str nil)
	(setq tmp "")
      (if (string-match "\\(virtual\\|static\\)" str)
	  (setq tmp "")
	(if reg
	    (setq tmp (concat str "[ \t]+"))
	  (setq tmp (concat str " ")))))
    tmp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Finds all functions in a class in a given buffer
;; Creates a list which looks like this.
;; ((classname) func1 func2 ...)
;; where func1 looks like
;; (type1 type2 type3 reference name args)
(defun find-class-functions ( buf )
  (interactive)
  (save-excursion
    (set-buffer buf)
    (beginning-of-buffer)
;;    (message "searching")
    (if (search-forward-regexp (concat "^\\(template[ \t]*<[^>]+>[ \t]*\\)?class[ \t]+\\([a-zA-Z0-9_]+\\)[ \t\n]*"
				       "\\([:][ \t\n]*\\(public\\|protected\\|private\\)?[ \t\n]*\\<[a-zA-Z0-9_]+\\>\\)?"
				       "[ \t\n]*{") nil t)
	(let (start
	      stop
	      (name (match-string-no-properties 2)))
;;	  (message "found first")
	  (setq start (match-end 0))
	  (if ( search-forward "};" nil t)
	      (let ((funclist '()))
		(setq stop (match-beginning 0))
;;		(message "found second")
		(save-restriction
		  (narrow-to-region start stop)
		  (beginning-of-buffer)
		  (while (search-forward-regexp (concat
						 "\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]+\\)?"
						 "\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]+\\)?"
						 "\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]+\\)?"
						 "\\([*&]*\\)[ \t]*"
						 "\\(~?[a-zA-Z][a-zA-Z0-9_]*\\)[ \t]*"
						 "(\\([^\)]*\\))\\([ \t]*const\\|\\)[ \t]*;")
						nil t)
		    (let (
			  (type1 (match-string-no-properties 2))
			  (type2 (match-string-no-properties 4))
			  (type3 (match-string-no-properties 6))
			  (ref (match-string-no-properties 7))
			  (name (match-string-no-properties 8))
			  (args (match-string-no-properties 9))
			  (hasconst (match-string-no-properties 10)))
			  (if (and (not (string-equal "inline" type1))
					   (not (string-equal "inline" type2)))
				  (setq funclist (cons (list type1 type2 type3 ref name args hasconst) funclist)))))
		  (setq funclist (cons (list name) funclist ))
		  funclist)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Finds all functions in this buffers class and adds the one's missing from
;; the source file.
(defun expand-class-functions ( buf )
  (interactive)
  (if (string-match source-header-ext-regexp (buffer-name buf))
      (save-excursion
        (set-buffer buf)
        (let ((lst (find-class-functions buf))
              item
              classname)
          (toggle-source-header)
          (beginning-of-buffer)
          (setq classname (car (car lst)))
          (setq lst (cdr lst))
          (let (type1 type1_reg
                      type2 type2_reg
                      type3 type3_reg
                      ref ref_reg
                      name
                      args args_reg
                      hasconst )
            (if (eq hasconst nil)
                (setq hasconst ""))
            (setq lst (nreverse lst))
            (while lst
              (setq item (car lst))
              (setq type1 (car item))
              (setq item (cdr item))
              (setq type2 (car item))
              (setq item (cdr item))
              (setq type3 (car item))
              (setq item (cdr item))
              (setq ref (car item))
              (setq item (cdr item))
              (setq name (car item))
              (setq item (cdr item))
              (setq args (car item))
              (setq item (cdr item))
              (setq hasconst (car item))
              (setq item (cdr item))
              (setq type1_reg (string-remove-type type1 t))
              (setq type2_reg (string-remove-type type2 t))
              (setq type3_reg (string-remove-type type3 t))
              (if (eq ref nil)
                  (setq ref_reg "[ \t]*")
                (setq ref_reg (concat "[" ref "]" "[ \t]*")))
              (setq args_reg (regexp-opt (list args)))
              (beginning-of-buffer)
              (if (search-forward-regexp (concat "^" type1_reg type2_reg type3_reg ref_reg
                                                 classname "::" name "[ \t]*" "(\\([^)]+\\))" ) nil t)
                  (let (args_org
                        args_new
                        (offs_org 0) (len_org 0)
                        (offs_new 0) (len_new 0)
                        type1 type2 type3 ref
                        (all t)
                        (args_reg (concat "\\(\\([a-zA-Z]+\\)[ \t]+\\)?"
                                          "\\(\\([a-zA-Z]+\\)[ \t]+\\)?"
                                          "\\(\\([a-zA-Z]+\\)[ \t]+\\)?"
                                          "\\([&*]\\)?[ \t]*\\([a-zA-Z_][a-zA-Z_]*\\)?\\([ \t]*=[^,]+\\)?")))
                    (setq args_new (match-string 1))
;                   (yes-or-no-p (concat "match " name))
                    (while (and (not offs_org) (not offs_new))
                      (setq offs_org (string-match args_reg args offs_org))
                      (setq type1 (substring args (match-beginning 2) (match-end 2)))
                      (setq type2 (substring args (match-beginning 4) (match-end 4)))
                      (setq type3 (substring args (match-beginning 6) (match-end 6)))
                      (setq ref (substring args (match-beginning 7) (match-end 7)))
;;                    (setq offs_new (string-match args_reg args_new offs_new))
                      (yes-or-no-p (concat type1 ":" type2 ";" type3 ":" ref ))))
                (save-excursion
;;                (message (concat "^" type1_reg type2_reg type3_reg ref_reg
;;                                  classname "::" name "[ \t]*" "(" args_reg ")" ))
                  (end-of-buffer)
                  (setq type1_reg (string-remove-type type1 nil))
                  (setq type2_reg (string-remove-type type2 nil))
                  (setq type3_reg (string-remove-type type3 nil))
                  (if (eq ref nil)
                      (setq ref_reg "")
                    (setq ref_reg (concat ref)))
                  (if (not (bolp))
                      (insert-string "\n"))
                  (insert-string (concat "\n\n////////////////////////////////////////\n\n\n"
                                         type1_reg type2_reg type3_reg ref_reg
                                         classname "::" name "(" args ")" hasconst
                                         "\n{\n}\n"))))
              (setq lst (cdr lst))
              ))))))

(provide 'init-programming-functions)
