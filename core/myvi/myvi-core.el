;;; -*- coding: utf-8; lexical-binding: t -*-
;; myvi-core.el

(defvar myvi-map (make-sparse-keymap)
  "Keymap for myvi commands.")

(defvar myvi-active-p nil
  "A variable to control the activation of myvi keymap.")
(make-variable-buffer-local 'myvi-active-p)

;; (defvar myvi-previous-state nil
;;   "Variable to save the state of myvi before entering minibuffer.")

(defvar myvi-escape-bound nil
  "Buffer-local variable to track whether escape key is bound.")
(make-variable-buffer-local 'myvi-escape-bound)

(defun myvi-setup-escape ()
  "Set up escape key binding for the current buffer."
  (unless (or myvi-escape-bound
              (derived-mode-p 'special-mode))
    (let ((map (current-local-map)))
      (unless map
        (use-local-map (setq map (make-sparse-keymap))))
      (define-key map [escape]
                  (lambda ()
                    (interactive)
                    (if (minibufferp)
                        (keyboard-escape-quit)
                      (myvi-enable))))
      (setq myvi-escape-bound t))))

(defun myvi-remove-escape ()
  "Remove escape key binding from the current buffer."
  (when myvi-escape-bound
    (let ((map (current-local-map)))
      (when map
        (define-key map [escape] nil)))
    (setq myvi-escape-bound nil)))

(defun myvi-enable ()
  "Enable myvi commands."
  (interactive)
  (setq myvi-active-p t
        cursor-type 'box)
  (unless (bolp)
    (backward-char))
  (message "myvi commands active"))

(defun myvi-disable ()
  "Disable myvi commands."
  (interactive)
  (setq myvi-active-p nil
        cursor-type 'bar)
  (message "myvi commands disabled"))

;; (defun save-and-disable-myvi ()
;;   "Save the previous state of myvi mode and disable it."
;;   (setq myvi-previous-state myvi-active-p)
;;   (setq myvi-active-p nil))

;; (defun restore-myvi ()
;;   "Restore the previous state of myvi mode."
;;   (setq myvi-active-p myvi-previous-state))


;; myvi-mode
;;;###autoload
(define-minor-mode myvi-mode
  "A simple Vi-like editing mode."
  :init-value nil
  :lighter " Myvi"
  (if myvi-mode
      (progn
        (setq myvi-active-p nil)
              ;;myvi-previous-state nil)
        (add-to-list 'emulation-mode-map-alists
                     `((myvi-active-p . ,myvi-map)))
        (myvi-setup-escape)
        (myvi-enable))
    (progn
      (setq emulation-mode-map-alists
            (remove `((myvi-active-p . ,myvi-map))
                    emulation-mode-map-alists))
      (myvi-remove-escape)
      (myvi-disable))))


;(add-hook 'minibuffer-setup-hook 'save-and-disable-myvi)
;(add-hook 'minibuffer-exit-hook 'restore-myvi)

(provide 'myvi-core)
;;; myvi-core.el ends here
