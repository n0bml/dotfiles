;; -*- mode: emacs-lisp;  lexical-binding: t;  indent-tabs-mode: nil -*-

;;; Fixing the mark commands in transient mark mode By Mickey Petersen
;;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring."))

(bind-key "C-`" #'push-mark-no-activate)


(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(bind-key "M-`" #'jump-to-mark)


(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(bind-key [remap exchange-point-and-mark] #'exchange-point-and-mark)


(provide 'fix-mark-commands)
