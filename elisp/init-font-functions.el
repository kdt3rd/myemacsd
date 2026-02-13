;;; init-font-functions.el --- Global UI/General Configuration -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

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

(provide 'init-font-functions)
;;; init-font-functions.el ends here
