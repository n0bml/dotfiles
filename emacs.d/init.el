;;; init.el -*- lexical-binding: t -*-

;; Copyright 2021 Brendan Leber
;; Author: Brendan Leber <bleber@skytap.com>

(setq user-full-name "Brendan Leber"
      user-mail-address "bleber@skytap.com")

;; add this early to capture how long starting emacs takes.
(defun bml/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'bml/display-startup-time)

;; help out with some startup performance
(setq gc-cons-threshold (* 50 1000 1000))

;; initialize installed packages
(setq package-enable-at-startup t)

;; disable some gui elements i don't like
(menu-bar-mode -1)
(tooltip-mode -1)

(setq inhibit-spash-screen t
      inhibit-startup-buffer-menu t
      ;; inhibit-startup-echo-area-message "bml"
      inhibit-startup-screen t)
      ;use-dialog-box t
      ;use-file-dialog nil)

;; initialize package sources
(require 'package)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq load-prefer-newer t
      use-package-always-ensure t)
(require 'use-package)

;; various advice functions
(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the attached buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

;; various helper functions that aren't part of my modules
(defun bml/add-to-exec-path-and-path-if-exists (apath)
  "Add APATH to `exec-path' and the PATH environment variable if it exists."
  (let ((test-path (convert-standard-filename (expand-file-name apath))))
     (if (file-directory-p test-path)
	 (progn
	   (add-to-list 'exec-path test-path)
	   (setenv "PATH" (concat test-path ":" (getenv "PATH")))))))

(defun bml/add-to-load-path-if-exists (apath)
  "Add APATH to `load-path' if it exists."
  (let ((test-path (convert-standard-filename (expand-file-name apath))))
     (if (file-directory-p test-path)
	 (progn
	   (add-to-list 'load-path test-path)))))

(defun bml/check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun bml/collapse-blank-lines ()
  "Collapse consecutive blank lines into a single blank line.
Collapsing starts at `point-min' and ends at `point-max'."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n\n" nil t)
    (while (eq (following-char) ?\n)
      (delete-char 1))))

(defun bml/ediff-current-buffer-revision ()
  "Run ediff to diff the current buffer's file against VC depot.
Uses `vc.el' or `rcs.el' depending on `ediff-version-control-package'."
  (interactive)
  (let ((file (or (buffer-file-name)
                  (error "Current buffer is not visiting a file."))))
    (if (and (buffer-modified-p)
             (y-or-n-p (message "Buffer %s is modified.  Save buffer?" (buffer-name))))
        (save-buffer (current-buffer)))
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%s-internal" ediff-version-control-package)) "" "" nil)))

(defun bml/flush-blank-lines ()
  "Delete all blank lines from the current buffer."
  (interactive)
  (save-excursion
    (flush-lines "$^" (point-min) (point-max))))

(defun bml/load-if-exists (file)
  "Load `file' if it exists."
  (when (file-exists-p file)
    (load file)))

(defun bml/mark-line-and-copy ()
  "Copy the current line into the kill ring."
  (interactive)
  (beginning-of-line)
  (push-mark)
  (forward-line 1)
  (kill-ring-save (region-beginning) (region-end)))

(defun bml/match-paren (arg)
  "Go to the matching paren if on a paren; otherwise `self-insert'."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))

(defun bml/move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun bml/move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun bml/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens.  Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p))
         (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t
                (org-narrow-to-subtree))))
        (t
         (narrow-to-defun))))

(defun bml/narrow-to-line (&optional arg)
  "Narrow to the text of the current line.
A numeric prefix means to move forward, or backward if negative, that many
lines, thus narrowing to a line other than the one the point was
originally in."
  (interactive "P")
  (setq arg (if arg
                (prefix-numeric-value arg)
              0))
  (let ((inhibit-field-motion t))
    (save-excursion
      (forward-line arg)
      (narrow-to-region (line-beginning-position) (line-end-position)))))

(defun bml/org-src-block-copy ()
  "Copy the current source code block to the clipboard."
  (interactive)
  (org-edit-src-code)
  (clipboard-kill-ring-save (point-min) (point-max))
  (org-edit-src-abort))

(defun bml/org-src-block-yank ()
  "Yank the clipboard as a source block."
  (interactive)
  (insert "#+begin_src ")
  (save-excursion
    (insert "\n")
    (yank)
    (insert "\n#+end_src\n")))

(defun bml/prog-mode-before-save ()
  "Execute before saving in `prog-mode' buffers, added to `prog-mode-hook'."
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

(defun bml/prog-mode-setup ()
  "Various settings used in all `prog-mode' modes."
  (setq fill-column 100
        show-trailing-whitespace t))

(defun bml/revert-this-buffer ()
  "Revert the current buffer without confirmation."
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer: " (buffer-name))))

(defun bml/show-trailing-whitespace-on ()
  "Turn on displaying of trailing whitespace."
  (interactive)
  (setq show-trailing-whitespace t))

(defun bml/show-trailing-whitespace-toggle ()
  "Toggle `show-trailing-whitespace' on and off."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(defun bml/switch-to-previous-buffer ()
  "Switch to most recent buffer.  Repeated calls toggle back and forth
between the two most recent buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun bml/toggle-window-split ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges) (car next-win-edges))
                                     (<= (cadr this-win-edges) (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges) (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun bml/whitespace-mode-on ()
  "Turn on `whitespace-mode' as a hook."
  (whitespace-mode t))

(defun bml/sort-buffer ()
  "Sort the entire buffer using `sort-lines'."
  (interactive)
  (save-excursion
    (sort-lines nil (point-min) (point-max))))

(defun bml/unfill-paragraph ()
  "Replace newline characters in the current paragraph by single spaces.

This does the opposite of `fill-paragraph'."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (call-interactively #'fill-paragraph)))

(defun bml/unfill-region ()
  "Replace newline characters in the current region by single spaces.

This does the opposite of `fill-region'."
  (interactive "*r")
  (let (deactivate-mark
        (fill-column
         (if (eq last-command this-command)
             (progn
               (setq this-command nil)
               most-positive-fixnum)
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun bml/uniq (beg end test-line add-line)
  (save-excursion
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (not (eobp))
      (if (funcall test-line (thing-at-point 'line))
          (kill-line 1)
        (progn
          (funcall add-line (thing-at-point 'line))
          (forward-line))))
    (widen)))

;; location of third-party elisp packages not included in elpa or melpa.
(bml/add-to-load-path-if-exists (concat user-emacs-directory "third-party"))

;; location of my elisp packages
(bml/add-to-load-path-if-exists (concat user-emacs-directory "modules"))

(bml/add-to-exec-path-and-path-if-exists "/usr/local/bin")
(bml/add-to-exec-path-and-path-if-exists "~/.local/bin")

;; Configure everything in init.el and don't use custom.el
(setq custom-file (make-temp-file "bml-custom-"))

;; general global settings that aren't in cusomize or packages.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; store backup and auto-save files in the temporary directory.
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))

;; yes i want to be able to narrow my buffers!!!
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; follow links automatically
(require 'vc)
(setq vc-follow-symlinks t)

(setq delete-by-moving-to-trash t
      delete-old-versions t)

(global-auto-revert-mode t)

;; setup my display/UI preferences
(setq column-number-mode t
      display-line-numbers-type 'relative
      line-number-mode t
      global-prettify-symbols-mode t
      show-paren-mode t
      size-indication-mode t
      show-paren-when-point-in-periphery t)

;; for the love of god, no tabs!
(setq-default indent-tabs-mode nil)

;; (use-package eshell
;;   :ensure nil
;;   :init
;;   (defun bml/eshell-here ()
;;     "Opens a new shell in the directory associated with the current buffer's
;; file.  The eshell is named to match the directory to make multiple shell
;; windows easier."
;;     (interactive)
;;     (let* ((parent (if (buffer-file-name)
;;                        (file-name-directory (buffer-file-name))
;;                      default-directory))
;;            (height (/ (window-total-height) 3))
;;            (name (car (last (split-string parent "/" t)))))
;;       (split-window-vertically (- height))
;;       (other-window 1)
;;       (eshell "new")
;;       (rename-buffer (concat "*eshell: " name "*"))
;;       (insert (concat "ls"))
;;       (eshell-send-input)))

;;   (defun eshell/x ()
;;     "Closes the eshell session and gets ride of the eshell window."
;;     (kill-buffer)
;;     (delete-window))

;;   :config
;;   (use-package em-smart
;;     :ensure nil)
;;   (setq eshell-review-quick-commands nil
;;         eshell-scroll-to-bottom-on-input t
;;         eshell-smart-space-goes-to-end t
;;         eshell-where-to-jump 'begin)
;;   :bind ("C-!" . bml/eshell-here)
;;   :hook ((eshell-mode . eshell-smart-initialize)
;;          (eshell-mode . bml/disable-display-line-numbers-mode)))

;; we don't need non-work related holidays in our diary/calendar
(setq holiday-bahai-holidays nil
      holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-oriental-holidays nil)

;; calendar and agenda
(require 'calendar)
(setq ;calendar-date-display-form calendar-iso-date-display-form
      ;calendar-date-style 'iso
      calendar-holidays (append holiday-local-holidays
                                holiday-other-holidays
                                holiday-solar-holidays)
      calendar-latitude 47.3054
      calendar-location-name "Auburn, WA"
      calendar-longitude -122.2159
      calendar-mark-diary-entries-flag t
      calendar-week-start-day 1)

(add-hook 'calendar-today-visible-hook #'calendar-mark-today)

;; writing and prose
(add-hook 'text-mode-hook #'turn-on-flyspell)
(setq ispell-personal-dictionary (expand-file-name "ispell_personal" user-emacs-directory)
      sentence-end-double-space t)

(setq org-directory (expand-file-name "~/notes/"))

(setq ;org-agenda-files (ignore-errors (directory-files org-directory t "\\.org\\'" t))
      org-agenda-include-diary t
      org-agenda-span 'fortnight
      org-agenda-window-setup 'current-window
      org-capture-templates `(("n" "Notes" entry (file+olp+datetree org-default-notes-file)
			       "* %^{Description} %^g %?")
			      ("t" "Todo" entry (file+headline ,(expand-file-name "todo.org" org-directory) "Tasks")
			       "* TODO %?"))

      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-hide-emphasis-markers t
      org-html-htmlize-output-type 'css
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-startup-folded nil)

;; (add-hook 'org-mode-hook #'org-indent-mode)
;; (add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'turn-on-visual-line-mode)
(add-hook 'org-mode-hook #'turn-on-flyspell)

(require 'ox-md)

;; general programming
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      gdb-many-windows t)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'bml/prog-mode-setup)
(add-hook 'before-save-hook #'bml/prog-mode-before-save)

;; Emacs lisp
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)

;; C/C++
(defun bml/c-set-offset-setup ()
  "Override some `c-set-offset's to match my style."
  (c-set-offset 'arglist-cont-nonempty '*))

(setq c-default-style '((c-mode . "stroustrup")
                        (c++-mode . "stroustrup")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "gnu")))

(add-hook 'c-mode-common #'bml/c-set-offset-setup)

;; Python
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(setq python-shell-interpreter (executable-find "ipython")
      python-shell-interpreter-args "--pylab")

(use-package conf-toml-mode
  :ensure nil
  :mode (("\\.toml\\'" . conf-toml-mode)
         ("\\.flake8\\'" . conf-toml-mode)))

(use-package crux
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-buffer tabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-buffer delete-duplicate-lines)
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-line] . crux-smart-kill-line)
         ("C-<return>" . crux-smart-open-line)
         ("C-M-z" . crux-indent-defun)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-^" . crux-top-join-line)
         ("C-c ," . crux-find-user-custom-file)
         ("C-c <tab>" . crux-indent-regidly-and-copy-to-clipboard)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c F" . crux-recentf-find-directory)
         ("C-c I" . crux-find-user-init-file)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c S" . crux-find-shell-init-file)
         ("C-c T" . crux-visit-term-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c e" . crux-eval-and-replace)
         ("C-c f" . crux-recentf-find-file)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c o" . crux-open-with)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c u" . crux-view-url)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-x C-l" . crux-downcase-region)
         ("C-x C-u" . crux-upcase-region)
         ("C-x M-c" . crux-capitalize-region)
         ("M-o" . crux-other-window-or-switch-buffer)))

(use-package diminish
  :config
  (diminish 'elisp-doc-mode))

(use-package discover
  :config
  (global-discover-mode 1))

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)
  :config
  ;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; open an emacs mode buffer in a given state.  use to override the default evil mode selected
  ;; (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; (evil-set-initial-state 'dashboard-mode 'normal))
  (evil-mode 1))

;; (use-package evil-leader
;;   :after evil
;;   :config
;;   (global-evil-leader-mode)
;;   (evil-leader/set-leader "<SPC>")
;;   (evil-leade/set-key "e" 'counsel-find-file
;;                        "b" 'counsel-switch-buffer
;;                        "k" 'kill-buffer
;;                        "i" 'swiper
;;                        "z" 'shell
;;                        "s" 'save-buffer))

(use-package flycheck)

(use-package gotham-theme
  :config
  (load-theme 'gotham t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartscan
  :hook (prog-mode . smartscan-mode-turn-on))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package yaml-mode
  :mode ("\\.yaml$" . yaml-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package whitespace
  :diminish whitespace-mode
  :config
  (setq whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [9166 10])
                                      (tab-mark 9 [9654 9] [92 9]))
        whitespace-line-column nil)
  :hook (prog-mode . bml/show-trailing-whitespace-on))

;; setup keybindings near the end in case something has overriden them
(global-unset-key (kbd "C-z"))  ; unset keys i don't want to use
(global-unset-key (kbd "C-x C-z"))

(bind-keys ("C-s" . isearch-forward-regexp)  ; searching
           ("C-r" . isearch-backward-regexp)
           ("C-M-s" . isearch-forward)
           ("C-M-r" . isearch-backward))

(bind-keys ("C-x C-b" . ibuffer)  ; buffers
           ("C-x k" . kill-this-buffer)
           ("<f6>" . bml/revert-this-buffer))

(bind-keys ("C-S-<right>" . enlarge-window-horizontally)  ; windows
           ("C-S-<left>" . shrink-window-horizontally)
           ("C-S-<up>" . shrink-window)
           ("C-S-<down>" . enlarge-window))

(bind-keys ("M-z" . zap-up-to-char)  ; misc
           ("C-;" . comment-or-uncomment-region))

(bind-keys :prefix-map bml/prefix-toggle-map ; toggles
           :prefix "C-c t"
           ("f" . auto-fill-mode)
           ("e" . toggle-debug-on-error)
           ("s" . flyspell-mode)
           ("t" . bml/show-trailing-whitespace-toggle)
           ("w" . whitespace-mode))

(bind-keys :map lisp-mode-shared-map  ; lispy buffers
           ("%" . bml/match-paren)
           ("C-c v" . eval-buffer))

(bind-keys :map narrow-map ; narrowing
           ("l" . bml/narrow-to-line))

(bind-keys ("C-c a" . org-agenda)  ; org-mode
           ("C-c c" . org-capture)
           ("C-c l" . org-store-link))

;;           ("<return>" . reindent-then-newline-and-indent)
;;           ("C-\\" . lisp-complete-symbol)
;;           ("C-c c" . comment-region)
;;           ("C-c t" . bml/indent-buffer)

;; load local overrides if they exist
(let ((local-init-file (expand-file-name (concat user-emacs-directory "local-init.el"))))
  (when (file-exists-p local-init-file)
    (load-file local-init-file)))

;;; init.el ends here
