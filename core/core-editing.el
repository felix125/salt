;;; core-editing.el --- Vi-like editing commands -*- lexical-binding: t -*-
;;; Commentary:
;; A simple Vi-like editing mode with single state toggle
;;; Code:

;; Core variables and keymap
(defvar salt-active nil
  "A variable to control the activation of salt keymap.")

(defvar salt-revious-state nil
  "Variable to save the state of salt before entering minibuffer.")

(defvar-keymap salt-map
  :doc "Salt mode keymap"
  ;"c"
  ;"d"
  ;"e"
  ;"f"
  ;"g"
  ;"m"
  ;"o"
  ;; Movement
  "h" #'salt-backward-char
  "j" #'previous-line
  "k" #'next-line
  "l" #'salt-forward-char
  "b" #'backward-word
  "$" #'salt-end-of-line

  ;; undo
  "u" #'undo-fu-only-undo
  "U" #'undo-fu-only-redo

  ;; State control
  "i" #'salt-disable
  "a" #'salt-append
  "A" #'salt-append-line
  "o" #'salt-open-below
  "O" #'salt-open-above

  ;; Editing
  "x" #'delete-char

  ;; Others
  ":" #'salt-command)

(define-key salt-map (kbd "<right>") #'salt-forward-char)
(define-key salt-map (kbd "<left>") #'salt-backward-char)

;; Core functions
;; State control
(defun salt-disable ()
  "Disable Salt commands."
  (interactive)
  (setq salt-active nil
        cursor-type 'bar)
  (message "Salt commands disabled"))

(defun salt-enable ()
  "Enable Salt commands."
  (interactive)
  (setq salt-active t
        cursor-type 'box)
  (unless (bolp)
    (backward-char))
  (message "Salt commands active"))

(defun salt-append ()
  "Move forward and disable Salt commands."
  (interactive)
  (forward-char)
  (salt-disable))

(defun salt-append-line ()
  "Move to the end of line and disable Salt commands."
  (interactive)
  (end-of-line)
  (salt-disable))

(defun ensure-scratch-protected ()
  "Ensure the scratch buffer was locked."
  (unless (buffer-local-value 'emacs-lock-mode (get-buffer "*scratch*"))
    (with-current-buffer "*scratch*"
      (emacs-lock-mode 'kill))))

;; Movement
(defun salt-backward-char ()
  "Move one char backward."
  (interactive)
  (if (bolp)
      (message "Begin of line!")
    (backward-char)))

(defun salt-forward-char ()
  "Move one char forward."
  (interactive)
  (forward-char)
  (if (eolp)
      (progn (backward-char)
             (message "End of line"))))

(defun salt-end-of-line ()
  "Move to end of line."
  (interactive)
  (unless (eolp)
    (progn (end-of-line)
           (backward-char)
           (message "End of line"))))

(defun salt-open-above ()
  "Open a new line above."
  (interactive)
  (forward-line -1)
  (end-of-line)
  (comment-indent-new-line)
  (salt-disable))

(defun salt-open-below ()
  "Open a new line below."
  (interactive)
  (end-of-line)
  (comment-indent-new-line)
  (salt-disable))

;; Others

(defun salt-quit-buffer (&optional force)
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

(defun salt-command ()
  "Enter command mode, similar to Vi's command mode."
  (interactive)
  (let* ((command (read-from-minibuffer ":"))
         (cmd-parts (split-string command " ")))
    (pcase (car cmd-parts)
      ("w" (if (cdr cmd-parts)
               (write-file (cadr cmd-parts))
             (save-buffer)))
      ("q" (salt-quit-buffer))
      ("q!" (salt-quit-buffer t))       ; Force quit.
      ("wq" (progn
              (if (buffer-file-name)
                  (save-buffer)
                (call-interactively #'write-file))
              (salt-quit-buffer)))
      (_ (message "Unknown command: %s" command)))))


;; Minor mode definition
(define-minor-mode salt-mode
  "A simple Vi-like editing mode."
  :init-value nil
  :lighter " Salt"
  (if salt-mode
      (progn
        (ensure-scratch-protected)
        (add-to-list 'emulation-mode-map-alists
                     `((salt-active . ,salt-map)))
        (define-key global-map [escape]
                    (lambda ()
                      (interactive)
                      (if (minibufferp)
                          (keyboard-escape-quit)
                        (salt-enable))))
        (salt-enable))
    (progn
      (setq emulation-mode-map-alists
            (remove `((salt-active . ,salt-map))
                    emulation-mode-map-alists))
      (define-key global-map [escape] nil)
      (salt-disable))))

(defun save-and-disable-salt ()
  "Save the previous state of salt mode and disable it."
    (setq salt-previous-state salt-active)
    (setq salt-active nil))

(defun restore-salt ()
  "Restore the previous state of salt mode."
  (setq salt-active salt-previous-state))

(add-hook 'minibuffer-setup-hook 'save-and-disable-salt)
(add-hook 'minibuffer-exit-hook 'restore-salt)

;; 自動在適當的 buffer 中啟用
(add-hook 'prog-mode-hook #'salt-mode)
(add-hook 'text-mode-hook #'salt-mode)

(provide 'core-editing)
;;; core-editing.el ends here
