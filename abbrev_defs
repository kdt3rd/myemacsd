;;; init-programming-functions.el --- Initialize misc prog functions -*-coding: utf-8; lexical-binding: t -*-

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("esq" "#+BEGIN_SRC es :jq .

#+END_SRC" (lambda nil (forward-line -1)) :count 0)
   ))

