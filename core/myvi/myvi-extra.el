;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'myvi-core)

(defun myvi-quit-buffer (&optional force)
  "Kill current buffer and switch to *scratch*.
If FORCE is non-nil, abandon any modifications."
  (interactive)
  (let ((current (current-buffer)))
    (condition-case err
        (progn
          ;; Check if there are unsaved modifications
          (when (and (not force)
                     (buffer-modified-p))
            (error "Buffer modified; use :q! to force quit"))

          ;; Prepare to switch to *scratch*
          (if (get-buffer "*scratch*")
              (switch-to-buffer "*scratch*")
            (switch-to-buffer (get-buffer-create "*scratch*"))
            (lisp-interaction-mode))

          ;; Close the original buffer
          (kill-buffer current))
      (error (message "Error: %s" (error-message-string err))))))

;; Another read-char
(defun myvi-read-char ()
  "Wrapper around `read-char`."
  (let ((event (read-event)))
    (if (characterp event)
        event
      (progn (message "Unknown command")
             nil))))

;; deal with the Numeric Arguments
(defun myvi-read-number (first-digit)
  "Read number starting with FIRST-DIGIT. Return (number . next-char)."
  (let ((num (- first-digit ?0))
        (limit 10000)
        (ch (myvi-read-char)))
    (while (and ch (<= ?0 ch ?9) (<= num limit))
      (setq num (+ (* num 10) (- ch ?0)))
      (setq ch (myvi-read-char)))
    (when (> num limit)
      (message "Number too large, using %d" limit)
      (setq num limit))
    (cons num ch)))

;; deal with c, d or y commands
(defun myvi-handle-cdy-command (type)
  "Handle vi-like commands for TYPE (c, d or y)."
  (let* ((first-ch (myvi-read-char))
         (num-and-ch (if (and first-ch (<= ?1 first-ch ?9))
                         (myvi-read-number first-ch)
                       (cons 1 first-ch)))
         (num (car num-and-ch))
         (cmd (cdr num-and-ch)))
    (if (null cmd)
	(progn
          (message "Unknown command")
          nil)  ; 回傳 nil 表示失敗
      (cond
       ((char-equal cmd type) (myvi-delete-line num type))
       ((char-equal cmd ?w) (myvi-delete-word num type))
       ((char-equal cmd ?b) (myvi-delete-backward-word num type))
       ((char-equal cmd ?$) (myvi-delete-to-eol type))
       ((char-equal cmd ?0) (myvi-delete-to-bol type))
       ((char-equal cmd ?l) (myvi-delete-char num type 'forward))
       ((char-equal cmd ?h) (myvi-delete-char num type 'backward))
       (t (message "Unknown command")
	  nil)))))

;; yank
(defun myvi-yank-region (orig beg end)
  "Yank (copy) the region from BEG to END and restore cursor to ORIG."
  (goto-char beg)
  (push-mark end t t)
  (sit-for 0.15)
  (kill-ring-save beg end)
  (goto-char orig))

;; delete/yank N lines
(defun myvi-delete-line (n type)
  "Delete/yank N lines based on TYPE."
  (let ((orig (point))
        (beg (progn (beginning-of-line) (point)))
        (end (progn (forward-line n) (point))))
    (if (eq type ?y)
        (myvi-yank-region orig beg end)
      (progn
        (goto-char beg)
        (kill-line n)
        (when (eq type ?c)
          (open-line 1))
	t))))

;; delete/yank to beginning of line
(defun myvi-delete-to-bol (type)
  "Delete/yank to beginning of line based on TYPE."
  (let ((beg (line-beginning-position))
        (end (point)))
    (if (eq type ?y)
          (myvi-yank-region end beg end)
      (progn
	(kill-region beg end)
	t))))

;; delete/yank to end of line
(defun myvi-delete-to-eol (type)
  "Delete/yank to end of line based on TYPE."
  (let ((beg (point))
        (end (line-end-position)))
    (if (eq type ?y)
	(myvi-yank-region beg beg end)
      (progn
	(kill-region beg end)
	t))))

;; delete/yank N words backward
(defun myvi-delete-backward-word (n type)
  "Delete/yank N words backward based on TYPE."
  (let ((end (point))
	(beg (progn
               (backward-word n)
               (point))))
    (if (eq type ?y)
        (myvi-yank-region end beg end)
      (progn
	(kill-region beg end)
	t))))

;; delete/yank N words forward to their end
(defun myvi-delete-word (n type)
  "Delete N words forward to their end."
  (let ((beg (point))
        (end (progn
               (forward-word n)
               (point))))
    (if (eq type ?y)
        (myvi-yank-region beg beg end)
      (progn
	(kill-region beg end)
	t))))

;; delete char
(defun myvi-delete-char (n type direction)
  "Delete N chars in DIRECTION ('forward or 'backward). Return t if successful, nil if at buffer boundary."
  (let* ((pos (point))
         (max-possible (if (eq direction 'forward)
                           (- (point-max) pos)
                         (- pos (point-min))))
         (n (min n max-possible)))
    (if (= n 0)
        (progn
          (message (if (eq direction 'forward)
                       "End of buffer"
                     "Beginning of buffer"))
          nil)
      (let* ((beg (if (eq direction 'forward)
                      pos
                    (- pos n)))
             (end (if (eq direction 'forward)
                      (+ pos n)
                    pos)))
        (if (eq type ?y)
            (myvi-yank-region pos beg end)  ; 保持原始位置
          (kill-region beg end)
          (when (eq direction 'backward)
            (goto-char beg)))  ; 往回刪時，游標要移到新位置
        t))))


;; read-pattern
(defun myvi-read-pattern ()
  "Read search pattern from minibuffer. If input is empty, use last search pattern."
  (let ((input (read-from-minibuffer "/")))
    (cond
     ;; 輸入為空，且有上次搜尋
     ((and (string-empty-p input) myvi-last-search)
      myvi-last-search)
     ;; 輸入為空，且無上次搜尋
     ((string-empty-p input)
      (user-error "No previous search pattern"))
     ;; 有輸入，使用新輸入
     (t input))))


;; search
(defun myvi-do-search (pattern reverse)
  "Perform the actual search. Return matched position or nil if not found."
  (let ((case-fold-search nil))  ; 設定為 case-sensitive
    (if reverse
        (re-search-backward pattern nil t)
      (re-search-forward pattern nil t))))

(defun myvi-try-wrap-search (pattern reverse)
  "Try searching from beginning/end after wrap. Return matched position or nil."
  (let ((case-fold-search nil))  ; 這裡也要設定
    (message "Search wrapped")
    (if reverse
        (progn
          (goto-char (point-max))
          (re-search-backward pattern nil t))
      (progn
        (goto-char (point-min))
        (re-search-forward pattern nil t)))))


;;
(defun myvi-update-match-positions ()
  "Update the match positions after successful search."
  (setq myvi-last-match-beg (match-beginning 0))
  (setq myvi-last-match-end (match-end 0)))

;;
(defun myvi-set-position-for-next-search (reverse)
  "Set cursor position for the next search based on direction."
  (if reverse
      (goto-char myvi-last-match-beg)
    (goto-char myvi-last-match-end)))

;; myvi-forward-search
(defun myvi-forward-search (pattern &optional reverse)
  "Search forward for PATTERN. If REVERSE is non-nil, search backward."
  (interactive (list (myvi-read-pattern)))
  (let ((start-pos (point))
        (found nil))
    ;; 如果是重複上次搜尋，調整起始位置
    (when (and myvi-last-search
               (string= pattern myvi-last-search)
               myvi-last-match-beg)
      (myvi-set-position-for-next-search reverse))
    ;; 保存搜尋字串
    (setq myvi-last-search pattern)
    ;; 嘗試第一次搜尋
    (setq found (myvi-do-search pattern reverse))
    ;; 如果找不到則嘗試 wrap 搜尋
    (when (not found)
      (setq found (myvi-try-wrap-search pattern reverse)))
    (if found
        (progn
          (myvi-update-match-positions)
          (goto-char myvi-last-match-beg))
      (goto-char start-pos)
      (message "Pattern not found: %s" pattern))))


(defun myvi-repeat-search (&optional reverse)
  "Repeat last search. If REVERSE is non-nil, search in opposite direction."
  (interactive)
  (if myvi-last-search
      (myvi-forward-search myvi-last-search reverse)
    (message "No previous search")))


(provide 'myvi-extra)
;;; myvi-extra.el ends here
