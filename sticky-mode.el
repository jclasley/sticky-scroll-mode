;;; sticky-mode.el --- Sticky scrolling -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jon Lasley
;;
;; Author: Jon Lasley <jon.lasley+sticky@gmail.com>
;; Maintainer: Jon Lasley <jon.lasley+sticky@gmail.com>
;; Created: August 09, 2024
;; Modified: August 09, 2024
;; Version: 0.1.0
;; Keywords:  convenience extensions tools
;; Homepage: https://github.com/jclasley/sticky-mode
;; Package-Requires: ((emacs "29.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package provides sticky scrolling, a feature
;;  which keeps the scope(s) containing POINT at the top
;;  of your current window.
;;  To activate a live-reloading sticky scroll window, toggle `sticky-mode'.
;;  To show a one time sticky scroll window, `M-x sticky-toggle'.
;;  This package relies on `treesit' to parse the syntax tree.
;;
;;; Code:
(eval-when-compile
  (require 'treesit))

(defvar sticky-scroll--last-line 0) ;; local var for lines
(make-variable-buffer-local 'sticky-scroll--last-line)

;;;###autoload
(define-minor-mode sticky-scroll-mode
  "Enable a live-reloading sticky scroll window.
Keep track of the context in this buffer. Safe for use in mutliple buffers
at once. Utilizes `sticky-scroll--language-list' to determine which braces
to look for to define a context,based on
the `treesit' parser in the current buffer."
  :init-value nil ;; initial value
  ;; indicator
  :lighter " sticky"
  :keymap '(([C-tab] . sticky-scroll-toggle) ([C-backspace] . sticky-live-init))
  (let ((lang (treesit-language-at 1)))
    (unless (treesit-ready-p lang)
      (message "Treesit required for sticky-scroll-mode"))))

;; turn off the live window when sticky-scroll-mode is turned off
(add-hook 'sticky-scroll-mode-hook #'(sticky-scroll--live-toggle sticky-mode))

(defvar sticky-scroll--live-toggle nil
  "Whether this buffer has sticky-scroll--live-toggle enabled")
(make-variable-buffer-local 'sticky-scroll--live-toggle)

(defun sticky-scroll--live-toggle (&optional on)
  "Set up all the local variables and hooks necessary to update the overlay"
  (interactive)
  ;; check if we should rerun every single time a command is issued (performance?)
  (if (or sticky-scroll--live-toggle (not on))
      (progn
        (setq-local sticky-scroll--live-toggle nil)
        (remove-hook 'post-command-hook #'sticky-scroll--should-rerun t)
        (sticky-scroll-close-buffer))
    (setq-local sticky-scroll--live-toggle t)
    (add-to-list 'post-command-hook #'sticky-scroll--should-rerun)))

;; (defvar sticky-scroll--display)
(defun sticky-scroll--display (content)
  (sticky-scroll--window content))

(defvar sticky-scroll--buffer-alist nil
  "An ALIST of buffers and their associated sticky buffers")

(defun sticky-scroll--buffer-for-buffer (&optional buffer)
  "Get the "
  (if-let ((buf (alist-get (or buffer (current-buffer)) sticky-scroll--buffer-alist))
           (_ (buffer-live-p buf)))
      buf
    ;; just in case we have a hanging ref, clean it up
    (setq sticky-scroll--buffer-alist
          (assoc-delete-all (current-buffer) sticky-scroll--buffer-alist))
    (let ((buf (generate-new-buffer "*sticky*")))
      (setq sticky-scroll--buffer-alist
            (cons `(,(current-buffer) . ,buf) sticky-scroll--buffer-alist))
      buf)))

(defun sticky-scroll-close-buffer (&optional kill)
  (if-let ((buf-list (assoc (current-buffer) sticky-scroll--buffer-alist)))
      ;; kill the buffer and the window
      (if kill
          (unwind-protect ; in case the user has deleted the buffer on their own
              (if-let ((w (get-buffer-window (cdr buf-list))))
                  (quit-window t w)
                (kill-buffer (cdr buf-list)))
            (setq sticky-scroll--buffer-alist
                  (assoc-delete-all (car buf-list) sticky-scroll--buffer-alist)))
        (quit-windows-on (cdr buf-list)))))

(defun sticky-scroll--window (content)
  (let ((mode major-mode)
        (buf (sticky-scroll--buffer-for-buffer)))
    (if (equal content "")
        ;; kill it if it's empty
        (sticky-scroll-close-buffer)
      ;; otherwise, create the buffer
      (with-current-buffer buf
        ;; only change the major mode if necessary
        (unless (eq major-mode mode)
          ;; TODO: turn off any hooks affecting the mode
          (funcall mode))
        (hide-mode-line-mode 1)
        (erase-buffer)
        (let ((standard-output nil))
          (insert content))
        ;; goto the last character showing text
        (goto-char (point-max))
        (forward-line -1))
      ;; only make the window if there isn't one yet
      ;; (unless (get-buffer-window buf)
      (let ((window-height (seq-count (lambda (elt)
                                        (equal elt ?\n))
                                      content)))
        (display-buffer
         buf
         `(display-buffer-in-direction
           (direction . above) (window-height . ,(max window-height sticky-scroll--max-window-height))
           (preserve-size . (nil . t))
           (set-window-parameter . ((no-other-window . t)
                                    (no-delete-other-window . t)))))))))

(defun sticky-scroll--display-overlay (content)
  (overlay-put sticky-scroll-overlay
               'before-string (propertize content 'face
                                          '(:slant italic :weight bold
                                            :height 1.1 :box t))))

(defun sticky-scroll--should-rerun ()
  "Only rerun if the new line is different from the last time we checked."
  (while-no-input
    (redisplay)
    (let* ((pos (point))
           (line (line-number-at-pos pos)))
      (unless (equal line sticky-scroll--last-line)
        (sticky-scroll--window (sticky-scroll--content pos)))
      (setq-local sticky-scroll--last-line line))))

(defvar sticky-scroll-p nil)

(defun sticky-scroll--buffer-hook-func ()
  "Check the sticky-scroll--buffer-alist to see if any of the parent buffers
have been killed. At the same time, check if any non-killed buffers
 are no longer actively being shown. If so, bury their sticky buffers."
  (dolist (cell sticky-scroll--buffer-alist)
    (if (or (not (buffer-live-p (car cell)))
            (not (some-window #'(lambda (w)
                                  (eq (window-buffer w) (car cell))))))
        (with-current-buffer (cdr cell)
          ;; need to remove it from list first
          (setq sticky-scroll--buffer-alist
                (assoc-delete-all (car cell) sticky-scroll--buffer-alist))
          (kill-buffer-and-window)))))

(add-hook 'buffer-list-update-hook #'sticky-scroll--buffer-hook-func)
(add-hook 'kill-buffer-hook #'(sticky-scroll-close-buffer t))

(defun sticky-scroll-toggle (pos)
  "Briefly show the sticky window relevant to the current position."
  (interactive "d")
  (if sticky-scroll-mode
      (message "Sticky mode already enabled")
    (let ((content (sticky-scroll--content pos)))
      (if (eql content "")
          (message "No outer blocks to display")
        (sticky-scroll--window content)
        (setq-local sticky-scroll-quit nil)
        (add-hook 'post-command-hook #'sticky-scroll--toggle-hide-hook)))))

(defun sticky-scroll--toggle-hide-hook ()
  "Quit the `*sticky' window on the next command input"
  (while-no-input
    (redisplay)
    (when (and (not sticky-scroll-mode) (assoc (current-buffer) sticky-buffer-alist))
      (if (not sticky-scroll-quit)
          (setq-local sticky-scroll-quit t)
        ;; close window and kill buffer
        (sticky-scroll-close-buffer t)))))

(defun sticky-scroll--content (pos)
  ;; find the offscreen lines, and concat the content
  (let ((lines (sticky-scroll--offscreen-lines pos)))
    (mapconcat #'(lambda (elt) (concat elt "\n")) (reverse lines))))

(defgroup sticky-scroll nil
  "A list of scope characters per language, for identifying sticky blocks"
  :group 'convenience)

(defcustom sticky-scroll--max-window-height
  10
  "The max lines to display in the sticky scroll window"
  :type 'integer)

(defvar sticky-scroll--language-list
  '((go . (("{" . "}") ("(" . ")")))
    (tsx . (("{" . "}") ("(" . ")")))))

(add-to-list 'sticky-scroll--language-list '(rust . (("{" . "}") ("(" . ")"))))

(defun sticky-scroll--build-treesit-query ()
  (let* ((lang (treesit-language-at 1))
         (symbols (alist-get lang sticky-scroll--language-list))
         (openers (seq-reduce #'(lambda (memo elt)
                                  (push (car elt) memo)) symbols nil))
         (closers (seq-reduce #'(lambda (memo elt)
                                  (push (cdr elt) memo)) symbols nil)))
    ;; return in form of `(OPENING . CLOSING)`
    (list `(_ ,(vconcat openers) @opening)
          `(_ ,(vconcat closers) @closing))))


(defun sticky-scroll--block-line-numbers ()
  "Find all the opening and closing blocks in the buffer, then create
an ALIST with it"
  (let ((query (sticky-scroll--build-treesit-query)))
    ;; add all the openings to a list
    (setq-local sticky-scroll--blocks
                (treesit-query-capture (treesit-buffer-root-node) query))
    (sticky-scroll--section-line-alist sticky--blocks)))

(defun sticky-scroll--section-line-alist (capture-alist)
  "Turn a list of all opening and closing captures into an alist with in the form
of `(start_line . end_line)'"
  (let (stack (ret '()))
    (dolist (capture capture-alist stack)
      (let ((line (line-number-at-pos (treesit-node-start (cdr capture)))))
        (if (eql (car capture) 'opening)
            ;; use the classic push-to-stack to find matching parens
            (push line stack)

          ;; have to pop and assign to new -- maybe better way but do that
          ;; don't push single line blocks or duplicates
          (let ((open (pop stack)))
            (unless (or (eql open line)
                        (assoc open ret))
              (push (cons open line) ret))))))
    ret))

(defun sticky-scroll--unclosed-blocks-before (pos)
  (let ((line (line-number-at-pos pos)))
    (seq-filter #'(lambda (elt)
                    (and
                     ;; first check that it's offscreen
                     (< (car elt) (line-number-at-pos (window-start)))
                     ;; then check that it's before our line
                     (< (car elt) line)
                     ;; then check that its closing is after our line
                     (> (cdr elt) line)))
                (sticky-scroll--block-line-numbers))))

(defun sticky-scroll--offscreen-lines (pos)
  "Get the string content of the offscreen opening blocks"
  (when-let ((lines (sticky-scroll--unclosed-blocks-before pos)))
    (let (ret)
      (dolist (line lines ret)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- (car line)))
          (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) ret))))))

(provide 'sticky-mode)
;;; sticky-mode.el ends here
