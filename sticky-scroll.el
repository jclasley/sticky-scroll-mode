;; ;; TODO: doesn't work in Go -- have to fix this to be language agnostic

;; (defvar last-line 0) ;; local var for lines

;; (define-minor-mode sticky-mode
;;   "Use treesit to parse the syntax tree and have contextual opening block information
;; displayed in an unintrusive buffer at the top of the screen"
;;   :init-value nil ;; initial value
;;   ;; indicator
;;   :lighter " sticky"
;;   :keymap '(([C-tab] . sticky-toggle) ([C-backspace] . sticky-live-init))
;;   (if-let* ((lang (treesit-language-at 1))
;;             (treesit-ready-p lang))
;;       (make-variable-buffer-local 'sticky-buffer-name)
;;     (message "Treesit required for sticky-mode")))

;; ;; turn off the live window when sticky-mode is turned off
;; (add-hook! 'sticky-mode-hook (sticky-live-toggle sticky-mode))

;; (defvar live-toggle nil
;;   "Whether this buffer has live-toggle enabled")

;; (defun sticky-live-toggle (&optional on)
;;   "Set up all the local variables and hooks necessary to update the overlay"
;;   (interactive)
;;   (make-variable-buffer-local 'last-line)
;;   (make-variable-buffer-local 'live-toggle)
;;   ;; check if we should rerun every single time a command is issued (performance?)
;;   (if (or live-toggle (not on))
;;       (progn
;;         (setq-local live-toggle nil)
;;         (remove-hook 'post-command-hook #'sticky--should-rerun t)
;;         (sticky-close-buffer))
;;     (setq-local live-toggle t)
;;     (add-to-list 'post-command-hook #'sticky--should-rerun)))

;; ;; (defvar sticky--display)
;; (defun sticky--display (content)
;;   (sticky--window content))

;; (defvar sticky-buffer-alist nil
;;   "An ALIST of buffers and their associated sticky buffers")

;; (defun sticky-buffer-for-buffer ()
;;   (if-let ((buf (alist-get (current-buffer) sticky-buffer-alist))
;;            (_ (buffer-live-p buf)))
;;      buf
;;     ;; just in case we have a hanging ref, clean it up
;;     (setq sticky-buffer-alist (assoc-delete-all (current-buffer) sticky-buffer-alist))
;;     (let ((buf (generate-new-buffer "*sticky*")))
;;       (setq sticky-buffer-alist (cons `(,(current-buffer) . ,buf) sticky-buffer-alist))
;;     buf)))

;; (defun sticky-close-buffer (&optional kill)
;;   (if-let ((buf-list (assoc (current-buffer) sticky-buffer-alist)))
;;       ;; kill the buffer and the window
;;       (if kill
;;           (unwind-protect ; in case the user has deleted the buffer on their own
;;               (if-let ((w (get-buffer-window (cdr buf-list))))
;;                   (quit-window t w)
;;                 (kill-buffer (cdr buf-list)))
;;             (setq sticky-buffer-alist
;;                   (assoc-delete-all (car buf-list) sticky-buffer-alist)))
;;       (quit-windows-on (cdr buf-list)))))

;; (defun sticky--window (content)
;;   (let ((mode major-mode)
;;         (buf (sticky-buffer-for-buffer)))
;;     (if (equal content "")
;;         ;; kill it if it's empty
;;         (sticky-close-buffer)
;;       ;; otherwise, create the buffer
;;       (with-current-buffer buf
;;         ;; only change the major mode if necessary
;;         (unless (eq major-mode mode)
;;           ;; TODO: turn off any hooks affecting the mode
;;           (funcall mode))
;;         (hide-mode-line-mode 1)
;;         (erase-buffer)
;;         (quiet! (insert content))
;;         (goto-char 1))
;;       ;; only make the window if there isn't one yet
;;       ;; (unless (get-buffer-window buf)
;;       (display-buffer
;;        buf
;;        `(display-buffer-in-direction
;;          (direction . above) (window-height . ,(seq-count (lambda (elt) (equal elt ?\n)) content))
;;          (preserve-size . (nil . t))
;;          (set-window-parameter . ((no-other-window . t)
;;                                   (no-delete-other-window . t))))))))

;; (defun sticky--display-overlay (content)
;;   (overlay-put sticky-overlay
;;                'before-string (propertize content 'face
;;                                           '(:slant italic :weight bold
;;                                             :height 1.1 :box t))))

;;   (defun sticky--should-rerun ()
;;     "Only rerun if the new line is different from the last time we checked."
;;     (while-no-input
;;       (redisplay)
;;         (let* ((pos (point))
;;                (line (line-number-at-pos pos)))
;;           (unless (equal line last-line)
;;             (sticky--window (sticky--content pos)))
;;           (setq-local last-line line))))

;;   (defvar sticky-p nil)

;; (defun sticky--buffer-hook-func ()
;;   "Check the sticky-buffer-alist to see if any of the parent buffers
;; have been killed. At the same time, check if any non-killed buffers are no longer
;; actively being shown. If so, bury their sticky buffers."
;;   (dolist (cell sticky-buffer-alist)
;;     (if (or (not (buffer-live-p (car cell)))
;;                 (not (some-window #'(lambda (w)
;;                                   (eq (window-buffer w) (car cell))))))
;;       (with-current-buffer (cdr cell)
;;         ;; need to remove it from list first
;;         (setq sticky-buffer-alist
;;                   (assoc-delete-all (car cell) sticky-buffer-alist))
;;         (kill-buffer-and-window)))))

;; (add-hook 'buffer-list-update-hook #'sticky--buffer-hook-func)


;;   (defun sticky-toggle (pos)
;;     "Briefly show the sticky window relevant to the current position."
;;     (interactive "d")
;;     (if sticky-mode
;;       (message "Sticky mode already enabled")
;;      (let ((content (sticky--content pos)))
;;       (if (eql content "")
;;           (message "No outer blocks to display")
;;         (sticky--window content)
;;     (setq-local sticky-quit nil)
;;     (add-hook 'post-command-hook #'sticky-toggle-hide-hook)))))

;;   (defun sticky-toggle-hide-hook ()
;;     "Quit the `*sticky' window on the next command input"
;;     (while-no-input
;;       (redisplay)
;;      (when (and (not sticky-mode) (assoc (current-buffer) sticky-buffer-alist))
;;      (if (not sticky-quit)
;;         (setq-local sticky-quit t)
;;        ;; close window and kill buffer
;;         (sticky-close-buffer t)))))

;;   (defun sticky--content (pos)
;;     ;; find the offscreen lines, and concat the content
;;     (let ((lines (sticky--offscreen-lines pos)))
;;      (mapconcat #'(lambda (elt) (concat elt "\n")) (reverse lines))))

;; ;; TODO: variables to indicate blocks
;; (defgroup sticky-scope-characters
;;   nil
;;   "A list of scope characters per language, for identifying sticky blocks")

;; (defvar sticky--language-list
;;   '((go . (("{" . "}") ("(" . ")")))
;;     (tsx . (("{" . "}") ("(" . ")")))))

;; (add-to-list 'sticky--language-list '(rust . (("{" . "}") ("(" . ")"))))

;; (defun sticky--build-treesit-query ()
;;   (let* ((lang (treesit-language-at 1))
;;          (symbols (alist-get lang sticky--language-list))
;;          (opening-q '(@opening))
;;          (closing-q '(@closing))
;;          (openers (seq-reduce #'(lambda (memo elt)
;;                                   (push (car elt) memo)) symbols nil))
;;          (closers (seq-reduce #'(lambda (memo elt)
;;                                   (push (cdr elt) memo)) symbols nil)))
;;     ;; return in form of `(OPENING . CLOSING)`
;;     (list `(_ ,(vconcat openers) @opening)
;;           `(_ ,(vconcat closers) @closing))))
    

;;   (defun sticky--block-line-numbers ()
;;     "Find all the opening and closing blocks in the buffer, then create
;; an ALIST with it"
;;     (let ((opening-blocks-q '(statement_block "{" @opening))
;;            (closing-blocks-q '(statement_block "}" @closing))
;;            (query (sticky--build-treesit-query)))
;;       ;; add all the openings to a list
;;       (setq-local sticky--blocks
;;                   (treesit-query-capture (treesit-buffer-root-node) query))
;;       (sticky--section-line-alist sticky--blocks)))

;;   (defun sticky--section-line-alist (capture-alist)
;;     "Turn a list of all opening and closing captures into an alist with in the form
;; of `(start_line . end_line)'"
;;     (let (stack (ret '()))
;;       (dolist (capture capture-alist stack)
;;         (let ((line (line-number-at-pos (treesit-node-start (cdr capture)))))
;;         (if (eql (car capture) 'opening)
;;             ;; use the classic push-to-stack to find matching parens
;;             (push line stack)

;;           ;; have to pop and assign to new -- maybe better way but do that
;;           ;; don't push single line blocks or duplicates
;;           (let ((open (pop stack)))
;;             (unless (or (eql open line)
;;                          (assoc open ret))
;;               (push (cons open line) ret))))))
;;       ret))

;;   (defun sticky--unclosed-blocks-before (pos)
;;     (let ((line (line-number-at-pos pos)))
;;       (seq-filter #'(lambda (elt)
;;                       (and
;;                        ;; first check that it's offscreen
;;                        (< (car elt) (line-number-at-pos (window-start)))
;;                        ;; then check that it's before our line
;;                        (< (car elt) line)
;;                        ;; then check that its closing is after our line
;;                        (> (cdr elt) line)))
;;                   (sticky--block-line-numbers))))

;;   (defun sticky--offscreen-lines (pos)
;;     "Get the string content of the offscreen opening blocks"
;;     (when-let ((lines (sticky--unclosed-blocks-before pos)))
;;       (let (ret)
;;         (dolist (line lines ret)
;;           (save-excursion
;;             (goto-line (car line))
;;             (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) ret))))))
