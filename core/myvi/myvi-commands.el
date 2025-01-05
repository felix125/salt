;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Code:
(require 'myvi-core)
(require 'myvi-extra)
;;; 光標移動類 (Movement)
;; a:
(defun myvi-key-a ()
  "Move forward and disable myvi commands."
  (interactive)
  (unless (eolp)
    (forward-char))
  (myvi-disable))

;; b: 向後移動一個單字
(defun myvi-key-b ()
  "Move backward by word."
  (interactive)
  (backward-word))

;;; 變更/修改類 (Change/Edit)
;; c: [TODO] 尚未實現的變更命令
(defun myvi-key-c ()
  "Placeholder for change/edit command."
  (interactive)
  (message "C change command not implemented"))

;;; 刪除類 (Delete)
;; d: [TODO] 刪除行或選取區域
(defun myvi-key-d ()
  "Delete line or region."
  (interactive)
  (message "D delete command not implemented"))

;;; 光標移動類 (Movement)
;; e: 移動到單字結尾
(defun myvi-key-e ()
  "Move to end of word."
  (interactive)
  (forward-word)
  (backward-word)
  (forward-word))

;;; 搜尋類 (Search)
;; f: 向前搜尋
(defun myvi-key-f ()
  "Search forward."
  (interactive)
  (isearch-forward))

;;; 導航類 (Navigation)
;; g: 跳到指定行號
(defun myvi-key-g ()
  "Go to line number."
  (interactive)
  (call-interactively 'goto-line))

;;; 光標移動類 (Movement)
;; h: 向左移動一個字元
(defun myvi-key-h ()
  "Move one char backward."
  (interactive)
  (if (bolp)
      (message "Begin of line!")
    (backward-char)))

;;; 插入類 (Insert)
;; i: 回到 emacs 的編輯模式
(defun myvi-key-i ()
  "Exit command mode.
Unlike Vi’s insert mode, simply return to normal Emacs editing."
  (interactive)
  (myvi-disable))

;;; 光標移動類 (Movement)
;; j: 向下移動一行
(defun myvi-key-j ()
  "Move one line down."
  (interactive)
  (if (eobp)
      (message "End of buffer!")
    (forward-line)))

;; k: 向上移动一行
(defun myvi-key-k ()
  "Move one line up."
  (interactive)
  (if (bobp)
      (message "Beginning of buffer!")
    (forward-line -1)))

;; l: 向右移動一個字元
(defun myvi-key-l ()
  "Move one char forward."
  (interactive)
  (forward-char)
  (if (eolp)
      (progn (backward-char)
             (message "End of line"))))

;;; 標記類 (Mark)
;; m: 設置標記
(defun myvi-key-m ()
  "Set mark at current position."
  (interactive)
  (set-mark-command nil))

;;; 搜尋類 (Search)
;; n: 重複上一次搜尋
(defun myvi-key-n ()
  "Repeat last search."
  (interactive)
  (isearch-repeat-forward))

;;; 插入類 (Insert)
;; o: 插入新行
(defun myvi-key-o ()
  "Open a new line below."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (myvi-disable))

;;; 貼上類 (Paste)
;; p: 貼上
(defun myvi-key-p ()
  "Paste after cursor."
  (interactive)
  (yank))

;;; 巨集/錄製類 (Macro)
;; q: [TODO] 巨集錄製
(defun myvi-key-q ()
  "Placeholder for macro recording command."
  (interactive)
  (message "Q macro command not implemented"))

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

;;; 替換類 (Substitute)
;; s: [TODO] 替換
(defun myvi-key-s ()
  "Placeholder for substitute command."
  (interactive)
  (message "S substitute command not implemented"))

;;; 傳輸/跳轉類 (Transfer)
;; t: 移動到下一個字元之前
(defun myvi-key-t ()
  "Move to before next character."
  (interactive)
  (call-interactively 'zap-to-char))

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

;;; 視覺選擇類 (Visual)
;; v: 進入視覺模式
(defun myvi-key-v ()
  "Enter visual mode (mark region)."
  (interactive)
  (set-mark-command nil))

;;; 光標移動類 (Movement)
;; w: 向前移動一個單字
(defun myvi-key-w ()
  "Move forward by word."
  (interactive)
  (forward-word))

;;; 刪除類 (Delete)
;; x: 刪除游標下字元
(defun myvi-key-x ()
  "Delete character under cursor."
  (interactive)
  (delete-char 1))

;;; 複製類 (Yank/Copy)
;; y: 複製
(defun myvi-key-y ()
  "Copy line or region."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

;;; 摺疊/捲動類 (Fold/Scroll)
;; z: [TODO] 摺疊或捲動
(defun myvi-key-z ()
  "Placeholder for fold/scroll command."
  (interactive)
  (message "Z fold/scroll command not implemented"))

;;; 插入類 (Insert)
;; A: 在行尾插入
(defun myvi-key-A ()
  "Move to the end of line and disable myvi commands."
  (interactive)
  (end-of-line)
  (myvi-disable))


;;; 光標移動類 (Movement)
;; B: 向後移動一個大寫單字
(defun myvi-key-B ()
  "Move backward by WORD (non-word-breaking)."
  (interactive)
  (backward-word))

;;; 變更/修改類 (Change)
;; C: [TODO] 變更
(defun myvi-key-C ()
  "Placeholder for change command."
  (interactive)
  (message "C change command not implemented"))

;;; 刪除類 (Delete)
;; D: 刪除到行尾
(defun myvi-key-D ()
  "Delete from cursor to end of line."
  (interactive)
  (kill-line))

;;; 光標移動類 (Movement)
;; E: 移動到大寫單字結尾
(defun myvi-key-E ()
  "Move to end of WORD (non-word-breaking)."
  (interactive)
  (forward-word)
  (backward-word)
  (forward-word))

;;; 搜尋類 (Search)
;; F: 向後搜尋
(defun myvi-key-F ()
  "Search backward for a character."
  (interactive)
  (call-interactively 'search-backward))

;;; 導航類 (Navigation)
;; G: 跳到文件末尾
(defun myvi-key-G ()
  "Go to last line of buffer."
  (interactive)
  (goto-char (point-max)))

;;; 光標移動類 (Movement)
;; H: 移動到視窗頂部
(defun myvi-key-H ()
  "Move to top of screen."
  (interactive)
  (move-to-window-line 0))

;;; 插入類 (Insert)
;; I: 移到行首並回到一般 emacs 的編輯模式
(defun myvi-key-I ()
  "Move to begin of line and Exit command mode.
Unlike Vi’s insert mode, simply return to normal Emacs editing."
  (interactive)
  (beginning-of-line)
  (myvi-disable))

;;; 光標移動類 (Movement)
;; J: 合併行
(defun myvi-key-J ()
  "Join current line with next line."
  (interactive)
  (join-line -1))

;;; 其他類 (Miscellaneous)
;; K: [TODO] 未定義
(defun myvi-key-K ()
  "Placeholder for K command."
  (interactive)
  (message "K command not implemented"))

;;; 光標移動類 (Movement)
;; L: 移動到視窗底部
(defun myvi-key-L ()
  "Move to bottom of screen."
  (interactive)
  (move-to-window-line -1))

;;; 光標移動類 (Movement)
;; M: 移動到視窗中間
(defun myvi-key-M ()
  "Move to middle of screen."
  (interactive)
  (move-to-window-line nil))

;;; 搜尋類 (Search)
;; N: 反向重複上次搜尋
(defun myvi-key-N ()
  "Repeat last search in opposite direction."
  (interactive)
  (isearch-repeat-backward))

;;; 插入類 (Insert)
;; O: 在上方插入新行
(defun myvi-key-O ()
  "Open a new line above."
  (interactive)
  (forward-line -1)
  (end-of-line)
  (newline-and-indent)
  (myvi-disable))


;;; 貼上類 (Paste)
;; P: 在游標前貼上
(defun myvi-key-P ()
  "Paste before cursor."
  (interactive)
  (yank-pop))

;;; 巨集/錄製類 (Macro)
;; Q: [TODO] 巨集相關
(defun myvi-key-Q ()
  "Placeholder for macro command."
  (interactive)
  (message "Q macro command not implemented"))

;;; 替換類 (Replace)
;; R: 進入替換模式
(defun myvi-key-R ()
  "Enter replace mode."
  (interactive)
  (overwrite-mode 1))

;;; 替換類 (Substitute)
;; S: 替換整行
(defun myvi-key-S ()
  "Substitute entire line."
  (interactive)
  (kill-whole-line)
  (open-line 1))

;;; 傳輸/跳轉類 (Transfer)
;; T: 移動到前一個字元
(defun myvi-key-T ()
  "Move to before previous character."
  (interactive)
  (call-interactively 'zap-to-char))

;;; 復原類 (Undo)
;; U: 復原整行
(defun myvi-key-U ()
  "Redo last action."
  (interactive)
  (if (fboundp 'undo-fu-only-redo)
      (undo-fu-only-redo)
    (user-error "Redo not available. Consider installing undo-fu")))

;;; 視覺選擇類 (Visual)
;; V: [TODO] 整行視覺選擇
(defun myvi-key-V ()
  "Placeholder for line visual mode."
  (interactive)
  (message "V line visual mode not implemented"))

;;; 光標移動類 (Movement)
;; W: 向前移動一個大寫單字
(defun myvi-key-W ()
  "Move forward by WORD (non-word-breaking)."
  (interactive)
  (forward-word))

;;; 刪除類 (Delete)
;; X: 刪除前一個字元
(defun myvi-key-X ()
  "Delete character before cursor."
  (interactive)
  (backward-delete-char 1))

;;; 複製類 (Yank/Copy)
;; Y: 複製整行
(defun myvi-key-Y ()
  "Yank (copy) entire line."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

;;; 摺疊/捲動類 (Fold/Scroll)
;; Z: [TODO] 捲動或摺疊
(defun myvi-key-Z ()
  "Placeholder for scroll/fold command."
  (interactive)
  (message "Z scroll/fold command not implemented"))

;;; 數字類 (Number)
;; 0: 移動到行首
(defun myvi-key-zero ()
  "Move to beginning of line."
  (interactive)
  (move-beginning-of-line nil))

;; 1-9: [TODO] 可能用於行號或其他操作
(defun myvi-key-one ()   "Placeholder for 1 key." (interactive) (message "One key not implemented"))
(defun myvi-key-two ()   "Placeholder for 2 key." (interactive) (message "Two key not implemented"))
(defun myvi-key-three () "Placeholder for 3 key." (interactive) (message "Three key not implemented"))
(defun myvi-key-four ()  "Placeholder for 4 key." (interactive) (message "Four key not implemented"))
(defun myvi-key-five ()  "Placeholder for 5 key." (interactive) (message "Five key not implemented"))
(defun myvi-key-six ()   "Placeholder for 6 key." (interactive) (message "Six key not implemented"))
(defun myvi-key-seven () "Placeholder for 7 key." (interactive) (message "Seven key not implemented"))
(defun myvi-key-eight () "Placeholder for 8 key." (interactive) (message "Eight key not implemented"))
(defun myvi-key-nine ()  "Placeholder for 9 key." (interactive) (message "Nine key not implemented"))

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
  "Placeholder for slash key."
  (interactive)
  (message "Slash key not implemented"))

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


(provide 'myvi-commands)
;;; myvi-commands.el ends here.
