;;; sticky-scroll-mode.el --- Sticky scrolling -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jon Lasley
;;
;; Author: Jon Lasley <jon.lasley+sticky@gmail.com>
;; Maintainer: Jon Lasley <jon.lasley+sticky@gmail.com>
;; Created: August 09, 2024
;; Modified: August 13, 2024
;; Version: 0.3.1
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
;;  To activate a live-reloading sticky scroll window, toggle `sticky-scroll-mode'.
;;  To show a one time sticky scroll window, `M-x sticky-scroll-popup'.
;;
;;; Code:

;; TODO: support mouse links for lines in sticky scrolling

;;;###autoload
(define-minor-mode sticky-scroll-mode
  "Enable a live-reloading sticky scroll window.
Keep track of the context in this buffer. Safe for use in mutliple buffers
at once. Utilizes indentation to find outer scopes and display them in a
window at the top of the `sticky-scroll-mode' window."
  :init-value nil ;; initial value
  ;; indicator
  :lighter " sticky"
  (if sticky-scroll-mode
      (progn
        (add-hook 'post-command-hook #'sticky-scroll--should-rerun)
        (add-hook 'kill-buffer-hook #'sticky-scroll-close-buffer)
        (add-hook 'pre-command-hook #'sticky-scroll--pre-command-hook))
    (sticky-scroll-close-buffer)
    (remove-hook 'post-command-hook #'sticky-scroll--should-rerun)
    (remove-hook 'kill-buffer-hook #'sticky-scroll-close-buffer)))

(defgroup sticky-scroll nil
  "A list of scope characters per language, for identifying sticky blocks."
  :group 'convenience)

(defcustom sticky-scroll--max-window-height
  10
  "The max lines to display in the sticky scroll window."
  :type 'integer)

(defun sticky-scroll-popup (&optional count)
  "Briefly show the sticky window relevant to the current position.
If COUNT is provided, only show COUNT number of outer contexts, starting
with the closest. Calling with `C-u N' sets COUNT to `N'."
  (interactive "p")
  (if sticky-scroll-mode
      (message "Sticky mode already enabled")
    (let ((content (sticky-scroll--collect-lines))
          (sticky-scroll--max-window-height (or count sticky-scroll--max-window-height)))
      (if (not content)
          (message "No outer blocks to display")
        (sticky-scroll--window content)
        (setq-local sticky-scroll-quit nil)
        (add-hook 'post-command-hook #'sticky-scroll--toggle-hide-hook)))))

(defun sticky-scroll--toggle-hide-hook ()
  "Quit the `*sticky' window on the next command input."
  (while-no-input
    (redisplay)
    (when (and (not sticky-scroll-mode) (assoc (current-buffer) sticky-scroll--buffer-alist))
      (if (not sticky-scroll-quit)
          (setq-local sticky-scroll-quit t)
        ;; close window and kill buffer
        (sticky-scroll-close-buffer)))))

(defvar sticky-scroll--buffer-alist nil
  "An ALIST of buffers and their associated sticky buffers.")

(defun sticky-scroll--buffer-for-buffer (&optional buffer)
  "Return the sticky buffer that is correlated to BUFFER.
If BUFFER is nil, use `current-buffer' instead. If there is no
sticky buffer, return nil"
  (if-let ((buf (alist-get (or buffer (current-buffer)) sticky-scroll--buffer-alist))
           (buffer-live-p buf))
      buf
    ;; just in case we have a hanging ref, clean it up
    (setq sticky-scroll--buffer-alist
          (assoc-delete-all (current-buffer) sticky-scroll--buffer-alist))
    (let ((buf (generate-new-buffer "*sticky*")))
      (setq sticky-scroll--buffer-alist
            (cons `(,(current-buffer) . ,buf) sticky-scroll--buffer-alist))
      buf)))

(defun sticky-scroll--window (content)
  "Create the sticky scroll window, displaying CONTENT."
  (let ((buf (sticky-scroll--buffer-for-buffer))
        (height (min (length content) sticky-scroll--max-window-height)))
    (if (not content)
        ;; kill it if it's empty
        (sticky-scroll-close-buffer)
      ;; otherwise, create the buffer
      (with-current-buffer buf
        ;; only change the major mode if necessary
        (setq-local mode-line-format nil)
        (erase-buffer)
        (let ((inhibit-message t))
          (dolist (str content)
            (insert str)
            (insert ?\n)))
        (if (< sticky-scroll--max-window-height (length content))
            (progn
              (goto-char (point-min))
              (forward-line (- (length content) sticky-scroll--max-window-height)))
          (goto-char (point-min))))
      ;; only make the window if there isn't one yet
      ;; (unless (get-buffer-window buf)
      (when (> height 0)
        (display-buffer
         buf
         `(display-buffer-in-direction
           (direction . above) (window-height . ,height)
           (preserve-size . (nil . t))
           (set-window-parameter . ((no-other-window . t)
                                    (no-delete-other-window . t)))))))))

;; TODO: don't drop buffer when we are on an empty line inside valid context
(defun sticky-scroll--collect-lines (&optional point start-indent content seen-levels)
  "Move backwards through the buffer, line by line, gathering contexts.
Collect the first line that has an indentation level
that is less than START-INDENT, the identation level at
the starting POINT. CONTENT is the list of string content found on
those lines. Only find at most one item at each indentation level."
  (save-excursion
    (goto-char (or point (point)))
    (let ((indent (current-indentation))
          (start-indent (or start-indent (current-indentation))))
      (if (and (eq (car seen-levels) 0)
               (not (eq (char-after (point)) ?\n)))
          content
        (forward-line -1)
        (let ((prev-indent (current-indentation))
              (content content)
              (empty (eq (char-after (point)) ?\n)))
          (when (and
                 (> indent prev-indent)
                 (< prev-indent start-indent)
                 (not (memq prev-indent seen-levels))
                 (not empty)
                 ;; only add the content if it's off screen
                 (< (point) (window-start)))
            (setq content (cons
                           (buffer-substring (line-beginning-position) (line-end-position))
                           content)))
          (unless empty
            (setq seen-levels (cons prev-indent seen-levels)))
          (sticky-scroll--collect-lines (line-beginning-position) start-indent
                                        content seen-levels))))))

(defun sticky-scroll-close-buffer (&optional cell)
  "Close the current buffer's sticky window, or `(cdr CELL)'.
If not found or already closed, do nothing."
  (if-let ((buf-list (or cell (assoc (current-buffer) sticky-scroll--buffer-alist))))
      (progn
        ;; kill the buffer and the window
        (setq sticky-scroll--buffer-alist
              (assoc-delete-all (car buf-list) sticky-scroll--buffer-alist))
        (unwind-protect ; in case the user has deleted the buffer on their own
            (if-let ((w (get-buffer-window (cdr buf-list))))
                (quit-window t w)
              (kill-buffer (cdr buf-list)))))))

(defvar sticky-scroll--last-line 0) ;; local var for lines
(make-variable-buffer-local 'sticky-scroll--last-line)

(defun sticky-scroll--should-rerun ()
  "Only rerun if the new line is different from the last time we checked."
  (if sticky-scroll-mode
      (while-no-input
        (redisplay)
        (let* ((pos (point))
               (line (line-number-at-pos pos)))
          (unless (equal line sticky-scroll--last-line)
            (let ((content (sticky-scroll--collect-lines)))
              (sticky-scroll--window content))
            (setq-local sticky-scroll--last-line line))))
    (sticky-scroll-close-buffer)))

(defun sticky-scroll--pre-command-hook ()
  "If we are going to swap buffers, go ahead and close the sticky window first."
  (when (or
         (eq this-command #'previous-buffer)
         (eq this-command #'next-buffer))
    (sticky-scroll-close-buffer)))

(provide 'sticky-scroll-mode)
;;; sticky-scroll-mode.el ends here
