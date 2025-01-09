;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Code:
(require 'myvi-core)
(require 'myvi-commands)

(defgroup myvi nil
  "A simple Vi-like editing mode."
  :group 'myvi)


;;; 光標移動類 (Movement)
;; a:
(defun myvi-key-a ()
  "Move forward and disable myvi commands."
  (interactive)
  (unless (eolp)
    (forward-char))
  (myvi-disable))

(define-key myvi-map "a" #'myvi-key-a)

;; b: 向後移動一個單字
(defun myvi-key-b ()
  "Move backward by word."
  (interactive)
  (backward-word))

(define-key myvi-map "b" #'myvi-key-b)

;;; 變更/修改類 (Change/Edit)
;; c: 變更
(defun myvi-key-c ()
  "Placeholder for change/edit command."
  (interactive)
  (when (myvi-handle-cdy-command ?c)  ; 只有在成功時才執行
    (myvi-disable)))

(define-key myvi-map "c" #'myvi-key-c)

;;; 刪除類 (Delete)
;; d: 刪除
(defun myvi-key-d ()
  "Start a delete command sequence"
  (interactive)
  (setq cursor-type '(hbar . 5))
  (myvi-handle-cdy-command ?d)
  (setq cursor-type 'box))

(define-key myvi-map "d" #'myvi-key-d)


;;; 光標移動類 (Movement)
;; e: 移動到單字結尾
(defun myvi-key-e ()
  "Move to end of word."
  (interactive)
  (forward-word)
  (backward-word)
  (forward-word))

(define-key myvi-map "e" #'myvi-key-e)

;;; 搜尋類 (Search)
;; f: 向前搜尋
(defun myvi-key-f ()
  "Search forward."
  (interactive)
  (isearch-forward))

(define-key myvi-map "f" #'myvi-key-f)

;;; 導航類 (Navigation)
;; g: 跳到指定行號
(defun myvi-key-g ()
  "Go to line number."
  (interactive)
  (call-interactively 'goto-line))

(define-key myvi-map "g" #'myvi-key-g)

;;; 光標移動類 (Movement)
;; h: 向左移動一個字元
(defun myvi-key-h ()
  "Move one char backward."
  (interactive)
  (if (bolp)
      (message "Begin of line!")
    (backward-char)))

(define-key myvi-map "h" #'myvi-key-h)

;;; 插入類 (Insert)
;; i: 回到 emacs 的編輯模式
(defun myvi-key-i ()
  "Exit command mode.
Unlike Vi’s insert mode, simply return to normal Emacs editing."
  (interactive)
  (myvi-disable))

(define-key myvi-map "i" #'myvi-key-i)


;;; 光標移動類 (Movement)
;; j: 向下移動一行
(defun myvi-key-j ()
  "Move one line down."
  (interactive)
  (if (eobp)
      (message "End of buffer!")
    (forward-line)))

(define-key myvi-map "j" #'myvi-key-j)

;; k: 向上移动一行
(defun myvi-key-k ()
  "Move one line up."
  (interactive)
  (if (bobp)
      (message "Beginning of buffer!")
    (forward-line -1)))

(define-key myvi-map "k" #'myvi-key-k)

;; l: 向右移動一個字元
(defun myvi-key-l ()
  "Move one char forward."
  (interactive)
  (forward-char)
  (if (eolp)
      (progn (backward-char)
             (message "End of line"))))

(define-key myvi-map "l" #'myvi-key-l)

;;; 標記類 (Mark)
;; m: 設置標記
(defun myvi-key-m ()
  "Set mark at current position."
  (interactive)
  (set-mark-command nil))

(define-key myvi-map "m" #'myvi-key-m)

;;; 搜尋類 (Search)
;; n: 重複上一次搜尋
(defun myvi-key-n ()
  "Search for next occurrence."
  (interactive)
  (myvi-repeat-search nil))

(define-key myvi-map "n" #'myvi-key-n)

;;; 插入類 (Insert)
;; o: 插入新行
(defun myvi-key-o ()
  "Open a new line below."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (myvi-disable))

(define-key myvi-map "o" #'myvi-key-o)

;;; 貼上類 (Paste)
;; p: 貼上
(defun myvi-key-p ()
  "Paste (put) text after cursor, handling line-wise paste specially."
  (interactive)
  (let ((text (car kill-ring)))
    (if (and text (string-match "\n$" text))
        (progn
          (forward-line)
          (beginning-of-line)
          (yank)
	  (forward-line -1))
      (forward-char)
      (yank)
      (backward-char))))

(define-key myvi-map "p" #'myvi-key-p)

;;; 巨集/錄製類 (Macro)
;; q: [TODO] 巨集錄製
(defun myvi-key-q ()
  "Placeholder for macro recording command."
  (interactive)
  (message "Q macro command not implemented"))

(define-key myvi-map "q" #'myvi-key-q)

;;; 替換/重做類 (Replace/Redo)
;; r: 替換當前光標位置的字符
(defun myvi-key-r ()
  "Replace character under cursor with next input character."
  (interactive)
  (let ((cursor-type '(hbar . 5)))
    (let ((event (read-event)))
      (if (and (characterp event)
               (char-displayable-p event))
          (progn
            (delete-char 1)
            (insert-char event)
            (backward-char))
        (message "Invalid character input"))))
  (setq cursor-type 'box))

(define-key myvi-map "r" #'myvi-key-r)

;;; 替換類 (Substitute)
;; s: [TODO] 替換
(defun myvi-key-s ()
  "Placeholder for substitute command."
  (interactive)
  (message "S substitute command not implemented"))

(define-key myvi-map "s" #'myvi-key-s)

;;; 傳輸/跳轉類 (Transfer)
;; t: 移動到下一個字元之前
(defun myvi-key-t ()
  "Move to before next character."
  (interactive)
  (call-interactively 'zap-to-char))

(define-key myvi-map "t" #'myvi-key-t)

;;; 復原類 (Undo)
;; u: 復原
(defcustom myvi-warn-no-undo-fu t
  "Whether to warn when undo-fu is not available."
  :type 'boolean
  :group 'myvi)

(defun myvi-key-u ()
  "Undo last action."
  (interactive)
  (if (fboundp 'undo-fu-only-undo)
      (undo-fu-only-undo)
    (when myvi-warn-no-undo-fu
      (message "Consider installing undo-fu for better undo support"))
    (undo)))

(define-key myvi-map "u" #'myvi-key-u)

;;; 視覺選擇類 (Visual)
;; v: 進入視覺模式
(defun myvi-key-v ()
  "Enter visual mode (mark region)."
  (interactive)
  (set-mark-command nil))

(define-key myvi-map "v" #'myvi-key-v)

;;; 光標移動類 (Movement)
;; w: 向前移動一個單字
(defun myvi-key-w ()
  "Move forward by word."
  (interactive)
  (forward-word))

(define-key myvi-map "w" #'myvi-key-w)

;;; 刪除類 (Delete)
;; x: 刪除游標下字元
(defun myvi-key-x ()
  "Delete character under cursor."
  (interactive)
  (delete-char 1))

(define-key myvi-map "x" #'myvi-key-x)

;;; 複製類 (Yank/Copy)
;; y: 複製
(defun myvi-key-y ()
  "Copy line or region."
  (interactive)
  (setq cursor-type '(hbar . 5))
  (myvi-handle-cdy-command ?y)
  (setq cursor-type 'box))

(define-key myvi-map "y" #'myvi-key-y)

;;; 摺疊/捲動類 (Fold/Scroll)
;; z: [TODO] 摺疊或捲動
(defun myvi-key-z ()
  "Placeholder for fold/scroll command."
  (interactive)
  (message "Z fold/scroll command not implemented"))

(define-key myvi-map "z" #'myvi-key-z)

;;; 插入類 (Insert)
;; A: 在行尾插入
(defun myvi-key-A ()
  "Move to the end of line and disable myvi commands."
  (interactive)
  (end-of-line)
  (myvi-disable))

(define-key myvi-map "A" #'myvi-key-A)

;;; 光標移動類 (Movement)
;; B: 向後移動一個大寫單字
(defun myvi-key-B ()
  "Move backward by WORD (non-word-breaking)."
  (interactive)
  (backward-word))

(define-key myvi-map "B" #'myvi-key-B)

;;; 變更/修改類 (Change)
;; C: [TODO] 變更
(defun myvi-key-C ()
  "Placeholder for change command."
  (interactive)
  (message "C change command not implemented"))

(define-key myvi-map "C" #'myvi-key-C)

;;; 刪除類 (Delete)
;; D: 刪除到行尾
(defun myvi-key-D ()
  "Delete from cursor to end of line."
  (interactive)
  (kill-line))

(define-key myvi-map "D" #'myvi-key-D)

;;; 光標移動類 (Movement)
;; E: 移動到大寫單字結尾
(defun myvi-key-E ()
  "Move to end of WORD (non-word-breaking)."
  (interactive)
  (forward-word)
  (backward-word)
  (forward-word))

(define-key myvi-map "E" #'myvi-key-E)

;;; 搜尋類 (Search)
;; F: 向後搜尋
(defun myvi-key-F ()
  "Search backward for a character."
  (interactive)
  (call-interactively 'search-backward))

(define-key myvi-map "F" #'myvi-key-F)

;;; 導航類 (Navigation)
;; G: 跳到文件末尾
(defun myvi-key-G ()
  "Go to last line of buffer."
  (interactive)
  (goto-char (point-max)))

(define-key myvi-map "G" #'myvi-key-G)

;;; 光標移動類 (Movement)
;; H: 移動到視窗頂部
(defun myvi-key-H ()
  "Move to top of screen."
  (interactive)
  (move-to-window-line 0))

(define-key myvi-map "H" #'myvi-key-H)

;;; 插入類 (Insert)
;; I: 移到行首並回到一般 emacs 的編輯模式
(defun myvi-key-I ()
  "Move to begin of line and Exit command mode.
Unlike Vi’s insert mode, simply return to normal Emacs editing."
  (interactive)
  (beginning-of-line)
  (myvi-disable))

(define-key myvi-map "I" #'myvi-key-I)

;;; 光標移動類 (Movement)
;; J: 合併行
(defun myvi-key-J ()
  "Join current line with next line."
  (interactive)
  (join-line -1))

(define-key myvi-map "J" #'myvi-key-J)

;;; 其他類 (Miscellaneous)
;; K: [TODO] 未定義
(defun myvi-key-K ()
  "Placeholder for K command."
  (interactive)
  (message "K command not implemented"))

(define-key myvi-map "K" #'myvi-key-K)

;;; 光標移動類 (Movement)
;; L: 移動到視窗底部
(defun myvi-key-L ()
  "Move to bottom of screen."
  (interactive)
  (move-to-window-line -1))

(define-key myvi-map "L" #'myvi-key-L)

;;; 光標移動類 (Movement)
;; M: 移動到視窗中間
(defun myvi-key-M ()
  "Move to middle of screen."
  (interactive)
  (move-to-window-line nil))

(define-key myvi-map "M" #'myvi-key-M)

;;; 搜尋類 (Search)
;; N: 反向重複上次搜尋
(defun myvi-key-N ()
  "Search for previous occurrence."
  (interactive)
  (myvi-repeat-search t))

(define-key myvi-map "N" #'myvi-key-N)

;;; 插入類 (Insert)
;; O: 在上方插入新行
(defun myvi-key-O ()
  "Open a new line above."
  (interactive)
  (forward-line -1)
  (end-of-line)
  (newline-and-indent)
  (myvi-disable))

(define-key myvi-map "O" #'myvi-key-O)

;;; 貼上類 (Paste)
;; P: 在游標前貼上
(defun myvi-key-P ()
  "Paste before cursor."
  (interactive)
  (yank-pop))

(define-key myvi-map "P" #'myvi-key-P)

;;; 巨集/錄製類 (Macro)
;; Q: [TODO] 巨集相關
(defun myvi-key-Q ()
  "Placeholder for macro command."
  (interactive)
  (message "Q macro command not implemented"))

(define-key myvi-map "Q" #'myvi-key-Q)

;;; 替換類 (Replace)
;; R: 進入替換模式
(defun myvi-key-R ()
  "Enter replace mode."
  (interactive)
  (overwrite-mode 1))

(define-key myvi-map "R" #'myvi-key-R)

;;; 替換類 (Substitute)
;; S: 替換整行
(defun myvi-key-S ()
  "Substitute entire line."
  (interactive)
  (kill-whole-line)
  (open-line 1))

(define-key myvi-map "S" #'myvi-key-S)

;;; 傳輸/跳轉類 (Transfer)
;; T: 移動到前一個字元
(defun myvi-key-T ()
  "Move to before previous character."
  (interactive)
  (call-interactively 'zap-to-char))

(define-key myvi-map "T" #'myvi-key-T)

;;; 復原類 (Undo)
;; U: 復原整行
(defun myvi-key-U ()
  "Redo last action."
  (interactive)
  (if (fboundp 'undo-fu-only-redo)
      (undo-fu-only-redo)
    (user-error "Redo not available. Consider installing undo-fu")))

(define-key myvi-map "U" #'myvi-key-U)

;;; 視覺選擇類 (Visual)
;; V: [TODO] 整行視覺選擇
(defun myvi-key-V ()
  "Placeholder for line visual mode."
  (interactive)
  (message "V line visual mode not implemented"))

(define-key myvi-map "V" #'myvi-key-V)

;;; 光標移動類 (Movement)
;; W: 向前移動一個大寫單字
(defun myvi-key-W ()
  "Move forward by WORD (non-word-breaking)."
  (interactive)
  (forward-word))

(define-key myvi-map "W" #'myvi-key-W)

;;; 刪除類 (Delete)
;; X: 刪除前一個字元
(defun myvi-key-X ()
  "Delete character before cursor."
  (interactive)
  (backward-delete-char 1))

(define-key myvi-map "X" #'myvi-key-X)

;;; 複製類 (Yank/Copy)
;; Y: 複製整行
(defun myvi-key-Y ()
  "Yank (copy) entire line."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(define-key myvi-map "Y" #'myvi-key-Y)

;;; 摺疊/捲動類 (Fold/Scroll)
;; Z: [TODO] 捲動或摺疊
(defun myvi-key-Z ()
  "Placeholder for scroll/fold command."
  (interactive)
  (message "Z scroll/fold command not implemented"))

(define-key myvi-map "Z" #'myvi-key-Z)

;;; 數字類 (Number)
;; 0: 移動到行首
(defun myvi-key-zero ()
  "Move to beginning of line."
  (interactive)
  (move-beginning-of-line nil))

(define-key myvi-map "0" #'myvi-key-zero)

;;; 1-9: [TODO] 可能用於行號或其他操作
;;1
(defun myvi-key-one ()
  "Placeholder for 1 key."
  (interactive)
  (message "One key not implemented"))

(define-key myvi-map "1" #'myvi-key-one)

;; 2
(defun myvi-key-two ()
  "Placeholder for 2 key."
  (interactive)
  (message "Two key not implemented"))

(define-key myvi-map "2" #'myvi-key-two)

;; 3
(defun myvi-key-three ()
  "Placeholder for 3 key."
  (interactive)
  (message "Three key not implemented"))

(define-key myvi-map "3" #'myvi-key-three)

;; 4
(defun myvi-key-four ()
  "Placeholder for 4 key."
  (interactive)
  (message "Four key not implemented"))

(define-key myvi-map "4" #'myvi-key-four)

;; 5
(defun myvi-key-five ()
  "Placeholder for 5 key."
  (interactive)
  (message "Five key not implemented"))

(define-key myvi-map "5" #'myvi-key-five)

;; 6
(defun myvi-key-six ()
  "Placeholder for 6 key."
  (interactive)
  (message "Six key not implemented"))

(define-key myvi-map "6" #'myvi-key-six)

;; 7
(defun myvi-key-seven ()
  "Placeholder for 7 key."
  (interactive)
  (message "Seven key not implemented"))

(define-key myvi-map "7" #'myvi-key-seven)

;; 8
(defun myvi-key-eight ()
  "Placeholder for 8 key."
  (interactive)
  (message "Eight key not implemented"))

(define-key myvi-map "8" #'myvi-key-eight)

;; 9
(defun myvi-key-nine ()
  "Placeholder for 9 key."
  (interactive)
  (message "Nine key not implemented"))

(define-key myvi-map "9" #'myvi-key-nine)

;;; 特殊字符類 (Special Characters)
;; 反引號
(defun myvi-key-backtick ()
  "Placeholder for backtick key."
  (interactive)
  (message "Backtick key not implemented"))

;; 波浪號
(defun myvi-key-tilde ()
  "Placeholder for tilde key."
  (interactive)
  (message "Tilde key not implemented"))

;; 驚嘆號
(defun myvi-key-exclamation ()
  "Placeholder for exclamation key."
  (interactive)
  (message "Exclamation key not implemented"))

;; @ 符號
(defun myvi-key-at-sign ()
  "Placeholder for at sign key."
  (interactive)
  (message "At sign key not implemented"))

;; 井號
(defun myvi-key-hash ()
  "Placeholder for hash key."
  (interactive)
  (message "Hash key not implemented"))

;; 美元符號
(defun myvi-key-dollar-sign ()
  "Move to end of line."
  (interactive)
  (unless (eolp)
    (progn (end-of-line)
           (backward-char)
           (message "End of line"))))

(define-key myvi-map "$" #'myvi-key-dollar-sign)

;; 百分號
(defun myvi-key-percent-sign ()
  "Placeholder for percent sign key."
  (interactive)
  (message "Percent sign key not implemented"))

;; 插入符號
(defun myvi-key-caret ()
  "Placeholder for caret key."
  (interactive)
  (message "Caret key not implemented"))

;; 和號
(defun myvi-key-ampersand ()
  "Placeholder for ampersand key."
  (interactive)
  (message "Ampersand key not implemented"))

;; 星號
(defun myvi-key-asterisk ()
  "Placeholder for asterisk key."
  (interactive)
  (message "Asterisk key not implemented"))

;; 左右括號
(defun myvi-key-left-parenthesis ()
  "Placeholder for left parenthesis key."
  (interactive)
  (message "Left parenthesis key not implemented"))

(defun myvi-key-right-parenthesis ()
  "Placeholder for right parenthesis key."
  (interactive)
  (message "Right parenthesis key not implemented"))

;; 底線
(defun myvi-key-underscore ()
  "Placeholder for underscore key."
  (interactive)
  (message "Underscore key not implemented"))

;; 加號
(defun myvi-key-plus ()
  "Placeholder for plus key."
  (interactive)
  (message "Plus key not implemented"))

;; 等號
(defun myvi-key-equals ()
  "Placeholder for equals key."
  (interactive)
  (message "Equals key not implemented"))

;; 大括號
(defun myvi-key-left-brace ()
  "Placeholder for left brace key."
  (interactive)
  (message "Left brace key not implemented"))

(defun myvi-key-right-brace ()
  "Placeholder for right brace key."
  (interactive)
  (message "Right brace key not implemented"))

;; 豎線
(defun myvi-key-vertical-bar ()
  "Placeholder for vertical bar key."
  (interactive)
  (message "Vertical bar key not implemented"))

;; 方括號
(defun myvi-key-left-bracket ()
  "Placeholder for left bracket key."
  (interactive)
  (message "Left bracket key not implemented"))

(defun myvi-key-right-bracket ()
  "Placeholder for right bracket key."
  (interactive)
  (message "Right bracket key not implemented"))

;; 冒號
(defun myvi-key-colon ()
  "Enter command mode, similar to Vi's command mode."
  (interactive)
  (let* ((command (read-from-minibuffer ":"))
         (cmd-parts (split-string command " ")))
    (pcase (car cmd-parts)
      ("w" (if (cdr cmd-parts)
               (write-file (cadr cmd-parts))
             (save-buffer)))
      ("q" (myvi-quit-buffer))
      ("q!" (myvi-quit-buffer t))       ; Force quit.
      ("wq" (progn
              (if (buffer-file-name)
                  (save-buffer)
                (call-interactively #'write-file))
              (myvi-quit-buffer)))
      (_ (message "Unknown command: %s" command)))))

(define-key myvi-map ":" #'myvi-key-colon)

;; 分號
(defun myvi-key-semicolon ()
  "Placeholder for semicolon key."
  (interactive)
  (message "Semicolon key not implemented"))

;; 引號
(defun myvi-key-quote ()
  "Placeholder for quote key."
  (interactive)
  (message "Quote key not implemented"))

;; 雙引號
(defun myvi-key-double-quote ()
  "Placeholder for double quote key."
  (interactive)
  (message "Double quote key not implemented"))

;; 角括號
(defun myvi-key-left-angle ()
  "Placeholder for left angle bracket key."
  (interactive)
  (message "Left angle bracket key not implemented"))

(defun myvi-key-right-angle ()
  "Placeholder for right angle bracket key."
  (interactive)
  (message "Right angle bracket key not implemented"))

;; 逗號
(defun myvi-key-comma ()
  "Placeholder for comma key."
  (interactive)
  (message "Comma key not implemented"))

;; 句號
(defun myvi-key-period ()
  "Placeholder for period key."
  (interactive)
  (message "Period key not implemented"))

;; 斜線
(defun myvi-key-slash ()
  "Search forward like vi's /, with wrap-around and vi-like cursor behavior."
  (interactive)
  (myvi-forward-search (myvi-read-pattern)))

(define-key myvi-map "/" #'myvi-key-slash)

;; 反斜線
(defun myvi-key-backslash ()
  "Placeholder for backslash key."
  (interactive)
  (message "Backslash key not implemented"))

;; 上
(defun myvi-key-up-arrow ()
  "Placeholder for right-arrow key."
  (interactive)
  (message "Up arrow key not implemented"))

;; 下
(defun myvi-key-down-arrow ()
  "Placeholder for down-arrow key."
  (interactive)
  (message "Down arrow key not implemented"))

;; 左
(defun myvi-key-left-arrow ()
  "Placeholder for down-arrow key."
  (interactive)
  (message "Down arrow key not implemented"))

;; 右
(defun myvi-key-right-arrow ()
  "Placeholder for up-arrow key."
  (interactive)
  (message "Right arrow key not implemented"))

;;;###autoload
(defun myvi-setup ()
  "Setup myvi mode advices."
  (interactive)
  (dolist (mode '(text-mode conf-mode fundamental-mode))
    (advice-add mode :after #'myvi-mode))
  (add-hook 'prog-mode-hook #'myvi-mode)
  (message "Myvi mode advices are set up"))

;;;###autoload
(defun myvi-teardown ()
  "Remove myvi mode advices."
  (interactive)
  (dolist (mode '(text-mode conf-mode fundamental-mode))
    (when (advice-member-p #'myvi-mode mode)
      (advice-remove mode #'myvi-mode)))
  (remove-hook 'prog-mode-hook #'myvi-mode)
  (message "Myvi mode advices are removed"))


(provide 'myvi)
;;; myvi.el ends here
