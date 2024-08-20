
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; emacs28 with native compilation

(setq load-prefer-newer t
      ;;native-comp-async-report-warnings-errors 'silent
      native-comp-async-report-warnings-errors nil
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Elpaca installer block
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process *git* nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process *git* nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; End of elpaca installer block

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq use-package-always-ensure t
        use-package-always-defer t
        package-native-compile t
        elpaca-queue-limit 16)
  (setq use-package-verbose init-file-debug
        use-package-expand-minimally (not init-file-debug)
        use-package-compute-statistics nil
        debug-on-error init-file-debug)
  (bind-key "C-c e u" 'elpaca-fetch-all)
  (bind-key "C-c e m" 'elpaca-manager))

(elpaca diminish)

;; Block until current queue processed.
(elpaca-wait)

;; https://github.com/progfolio/elpaca/wiki/Logging#auto-hiding-the-elpaca-log-buffer
(defvar +elpaca-hide-log-commands '(eval-buffer eval-region eval-defun eval-last-sexp org-ctrl-c-ctrl-c eros-eval-defun eros-eval-last-sexp elisp-eval-region-or-buffer)
  "List of commands for which a successfully processed log is auto hidden.")
(defun +elpaca-hide-successful-log ()
  "Hide Elpaca log buffer if queues processed successfully."
  (message "this: %S last: %S" this-command last-command)
  (if-let ((incomplete (cl-find 'incomplete elpaca--queues :key #'elpaca-q<-status))
           ((elpaca-q<-elpacas incomplete)))
      nil
    (when-let ((log (bound-and-true-p elpaca-log-buffer))
               (window (get-buffer-window log t)) ;; log buffer visible
               ((or (member last-command +elpaca-hide-log-commands)
                    (member this-command +elpaca-hide-log-commands))))
      (with-selected-window window (quit-window 'kill window)))))
(add-hook 'elpaca-post-queue-hook #'+elpaca-hide-successful-log)

;; https://github.com/progfolio/elpaca/wiki/Logging#how-to-change-a-commands-log-query
(with-eval-after-load 'elpaca-log
  (setf (alist-get +elpaca-hide-log-commands
                   elpaca-log-command-queries nil nil #'equal)
        "#unique | !finished"))

;; https://github.com/progfolio/elpaca/wiki/Logging#customizing-the-position-of-the-elpaca-log-buffer
(add-to-list 'display-buffer-alist '("\\*elpaca-log\\*" (display-buffer-reuse-window display-buffer-at-bottom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/radian-software/radian/blob/e3aad124c8e0cc870ed09da8b3a4905d01e49769/emacs/radian.el#L352
(defmacro use-feature (name &rest args)
  "Like `use-package', but without elpaca integration.
`NAME' and `ARGS' are as with `use-package'"
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; useful for corfu and vertico extensions
(defmacro use-extension (pkg name &rest args)
  "Like `use-package', but for a package extension.
`PKG' is the name of the package, `NAME' and `ARGS' are as with `use-package'"
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     :after ,pkg
     :demand t
     ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; useful for conditional enabling

(defmacro with-feature (feature &rest body)
  "If FEATURE is available, load it and evaluate BODY."
  (declare (indent defun))
  `(when (require ,feature nil :noerror)
     ,@body))


(defmacro with-function (function &rest body)
  "If FUNCTION is available, evaluate BODY."
  (declare (indent defun))
  `(when (functionp ,function)
     ,@body))


(defmacro with-executable (executable &rest body)
  "If EXECUTABLE is available in path, evaluate BODY."
  (declare (indent defun))
  `(when (executable-find (symbol-name ,executable))
     ,@body))


(defmacro with-any-executable (executables &rest body)
  "If any of EXECUTABLES are available in the path, evaluate BODY."
  (declare (indent defun))
  `(when (some (lambda (x) (executable-find (symbol-name x))) ,executables)
     ,@body))

;;;; Some straight functions need to be able to reload everything, so require won't do
;;(defun require! (feature &optional filename noerror)
;;  "Like `require', but if `force-reload' is non-nil, `load' instead.
;;`FEATURE', `FILENAME' and `NOERROR' have the same meaning as with require"
;;  (if (and (boundp 'force-reload) force-reload)
;;      (load (prin1-to-string feature) noerror nil nil t)
;;    (require feature filename noerror)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((update-file (expand-file-name ".last-package-update-day" user-emacs-directory)))
  (let ((last-update-day (string-to-number
                        (if (file-exists-p update-file)
                         (with-temp-buffer
                           (insert-file-contents update-file)
                           (buffer-string))
			 "0"))))
    (when (or
           (not (file-exists-p update-file))
           (>= (-
		(time-to-days (current-time))
		last-update-day)
               7))
      (elpaca-merge-all t t)
      (with-temp-buffer
	(insert (int-to-string (time-to-days (current-time))))
	(when (file-writable-p update-file)
	  (write-region (point-min)
			(point-max)
			update-file)))
      )))

;;;; minions hides all the minor mode lines instead of having to
;;;; add a diminish call to every use-package definition
;;(use-package minions
;;  :config
;;  (setq minions-mode-line-lighter ""
;;        minions-mode-line-delimiters '("" . ""))
;;  :hook (elpaca-after-init . (lambda () (minions-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package benchmark-init
;;  :straight t
;;  :config
;;  ;; To disable collection of benchmark data after init is done.
;;  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;;
;;(add-hook 'after-init-hook
;;          (lambda () (message "loaded emacs in %s" (emacs-init-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-startup-echo-area-message ()
  "Custom version of `display-startup-echo-area-message'."
  (message "%s packages loaded in %s seconds"
           (cdar elpaca--status-counts)
           (emacs-init-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-packagemanager)
