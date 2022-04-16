;; -*- mode: emacs-lisp;  lexical-binding: t;  indent-tabs-mode: nil -*-

;;; Swapping quote symbols in Emacs with parse-partial-sexp
;;; https://www.masteringemacs.org/article/swapping-quote-symbols-emacs-parsepartialsexp


(defun point-in-string-p (pt)
  "Returns t if PT is in a string."
  (eq 'string (syntax-ppss-context (syntax-ppss pt))))


(defun beginning-of-string ()
  "Moves to the beginning of a syntactic string."
  (interactive)
  (unless (point-in-string-p (point))
    (error "You must be in a string for this command to work."))
  (goto-char (nth 8 (syntax-ppss (point))))
  (point))


(defun swap-quotes ()
  "Swaps the quote symbols in a \\[python-mode] string."
  (interactive)
  (save-excursion
    (let ((bos (save-excursion (beginning-of-string)))
          (eos (save-excursion
                 (beginning-of-string)
                 (forward-sexp)
                 (point)))
          (replacement-char ?\'))
      (goto-char bos)
      ;; if the following character is a single quote then the
      ;; `replacement-char' should be a double quote.
      (when (eq (following-char) ?\')
        (setq replacement-char ?\"))
      (delete-char 1)
      (insert replacement-char)
      (goto-char eos)
      (delete-char -1)
      (insert replacement-char))))


(provide 'swap-quotes)
