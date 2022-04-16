;; -*- mode: emacs-lisp;  lexical-binding: t;  indent-tabs-mode: nil -*-

;;; From Working with Coding Systems and Unicode in Emacs by Mickey Petersen.
;;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs

(defvar universal-coding-system-env-list '("PYTHONENCODING")
  "List of environment variables \\[universal-coding-system-argument] should set.")

(defadvice universal-coding-system-argument (around provide-env-handler activate)
  "Augments \\[universal-coding-system-argument] so it also sets environment variables.

Naively sets all environment variables specified in
`universal-coding-system-env-list' to the literal string
representation of the argument `coding-system'.

No guarantees are made that the environment variables set by this advice support
the same coding system as Emacs."
  (let ((process-environment (copy-alist process-environment)))
    (dolist (extra-env universal-coding-system-env-list)
      (setenv extra-env (symbol-name (ad-get-arg 0))))
    ad-do-it))

(provide 'universal-coding-system)
