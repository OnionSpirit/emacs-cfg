;; Fullscreen on sturtup
;;(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; Scroll half-screen
(defvar scroll-half-by-default t
  "Whether scroll-* commands should scroll by half the window.
If this variable is non-nil, scroll commands receive half the current
window height")
(defun window-real-body-height (&optional wnd)
  (let ((w (or wnd (selected-window))))
    (/ (window-body-height w t)
       (window-font-height w))))

(defun or-half-window (args)
  (if scroll-half-by-default
      (list (or (car args) (/ (window-real-body-height) 2)))
    args))

(defun scroll-*@half (args)
  (or-half-window args))

(defun scroll-other-window-*@half (args)
  (with-selected-window (other-window-for-scrolling)
    (or-half-window args)))

(advice-add 'scroll-up :filter-args 'scroll-*@half)
(advice-add 'scroll-down :filter-args 'scroll-*@half)
(advice-add 'scroll-other-window
            :filter-args 'scroll-other-window-*@half)
(advice-add 'scroll-other-window-down
            :filter-args 'scroll-other-window-*@half)

;; Move selected text
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

;; Move selected text down
(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

;; Move selected text up
(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))


;; Centering frame at the screen
(defun frame-center ()
  "Center the current frame."
  (interactive)
  (let* ((dw (display-pixel-width))
         (dh (display-pixel-height))
         (f  (selected-frame))
         (fw (frame-pixel-width f))
         (fh (frame-pixel-height f))
         (x  (- (/ dw 2) (/ fw 2)))
         (y  (- (/ dh 2) (/ fh 2))))
    (message (format "dw %d dh %d fw %d fh %d x %d y %d" dw dh fw fh x y))
    (set-frame-position f x y)))


;; Switching throught func args
(defun arg-forward ()
  "Move point forward until we hit an argument separator, the comma, colon, semicolon"
  (interactive)
  (condition-case nil
      (let ((separator-regexp "[,;]"))
        (forward-sexp)
        (while (not (or (looking-at (format "[ \t]*%s" separator-regexp))
                        (save-excursion (forward-char -1)
                                        (looking-at (format "[]\"')}]*[ \t]*%s " separator-regexp)))))
          (forward-sexp))
        (if (looking-at separator-regexp)
            (forward-char 1))
        ;; going forward one sexp and back puts point at front of sexp
        ;; essentially skipping the whitespace, but also ensuring we don't
        ;; jump out of a larger closing set of parentheses
        (forward-sexp)
        (forward-sexp -1))
    (error
     (beep)
     (message "At the end of argument list"))))

(defun arg-backward ()
  "Move point forward until we hit an argument separator, the comma, colon, semicolon"
  (interactive)
  (condition-case nil
      (let ((separator-regexp "[,;]"))
        (backward-sexp)
        (while (not (or (looking-at (format "[ \t]*%s" separator-regexp))
                        (save-excursion (backward-char -1)
                                        (looking-at (format "[]\"')}]*[ \t]*%s " separator-regexp)))))
          (backward-sexp))
        (if (looking-at separator-regexp)
            (backward-char 1))
        ;; going forward one sexp and back puts point at front of sexp
        ;; essentially skipping the whitespace, but also ensuring we don't
        ;; jump out of a larger closing set of parentheses
        (backward-sexp)
        (backward-sexp +1))
    (error
     (beep)
     (message "At the top of argument list"))))


;; Backtabing blocks
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))


;; Moving to dots and comas
(defun xah-forward-dot-comma ()
  (interactive)
  (re-search-forward "\\.+\\|,+\\|;+" nil t))
(defun xah-backward-dot-comma ()
  (interactive)
  (re-search-backward "\\.+\\|,+\\|;+" nil t))


;; Moving to equal sign
(defun xah-forward-equal-sign ()
  (interactive)
  (re-search-forward "=+" nil t))

(defun xah-backward-equal-sign ()
  (interactive)
  (when (re-search-backward "=+" nil t)
    (while (search-backward "=" (- (point) 1) t)
      (left-char))))



;; Functions to replace yes|no ask to auto-yes for callable functions
(defun my/return-t (orig-fun &rest args)
  t)
(defun my/disable-yornp (orig-fun &rest args)
  (advice-add 'yes-or-no-p :around #'my/return-t)
  (advice-add 'y-or-n-p :around #'my/return-t)
  (let ((res (apply orig-fun args)))
    (advice-remove 'yes-or-no-p #'my/return-t)
    (advice-remove 'y-or-n-p #'my/return-t)
    res))


;; Restart emacs
(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'launch-separate-emacs-under-x
                                                         #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))
(advice-add 'restart-emacs :around #'my/disable-yornp)
