;;; -*- coding: utf-8; lexical-binding: t -*-
;;;
(require 'myvi-core)
(require 'myvi-commands)

(defgroup myvi nil
  "A simple Vi-like editing mode."
  :group 'editing)

;; define keybinding
(define-key myvi-map "a" #'myvi-key-a)
(define-key myvi-map "b" #'myvi-key-b)
(define-key myvi-map "c" #'myvi-key-c)
(define-key myvi-map "d" #'myvi-key-d)
(define-key myvi-map "e" #'myvi-key-e)
(define-key myvi-map "f" #'myvi-key-f)
(define-key myvi-map "g" #'myvi-key-g)
(define-key myvi-map "h" #'myvi-key-h)
(define-key myvi-map "i" #'myvi-key-i)
(define-key myvi-map "j" #'myvi-key-j)
(define-key myvi-map "k" #'myvi-key-k)
(define-key myvi-map "l" #'myvi-key-l)
(define-key myvi-map "m" #'myvi-key-m)
(define-key myvi-map "n" #'myvi-key-n)
(define-key myvi-map "o" #'myvi-key-o)
(define-key myvi-map "p" #'myvi-key-p)
(define-key myvi-map "q" #'myvi-key-q)
(define-key myvi-map "r" #'myvi-key-r)
(define-key myvi-map "s" #'myvi-key-s)
(define-key myvi-map "t" #'myvi-key-t)
(define-key myvi-map "u" #'myvi-key-u)
(define-key myvi-map "v" #'myvi-key-v)
(define-key myvi-map "w" #'myvi-key-w)
(define-key myvi-map "x" #'myvi-key-x)
(define-key myvi-map "y" #'myvi-key-y)
(define-key myvi-map "z" #'myvi-key-z)

(define-key myvi-map "A" #'myvi-key-A)
(define-key myvi-map "B" #'myvi-key-B)
(define-key myvi-map "C" #'myvi-key-C)
(define-key myvi-map "D" #'myvi-key-D)
(define-key myvi-map "E" #'myvi-key-E)
(define-key myvi-map "F" #'myvi-key-F)
(define-key myvi-map "G" #'myvi-key-G)
(define-key myvi-map "H" #'myvi-key-H)
(define-key myvi-map "I" #'myvi-key-I)
(define-key myvi-map "J" #'myvi-key-J)
(define-key myvi-map "K" #'myvi-key-K)
(define-key myvi-map "L" #'myvi-key-L)
(define-key myvi-map "M" #'myvi-key-M)
(define-key myvi-map "N" #'myvi-key-N)
(define-key myvi-map "O" #'myvi-key-O)
(define-key myvi-map "P" #'myvi-key-P)
(define-key myvi-map "Q" #'myvi-key-Q)
(define-key myvi-map "R" #'myvi-key-R)
(define-key myvi-map "S" #'myvi-key-S)
(define-key myvi-map "T" #'myvi-key-T)
(define-key myvi-map "U" #'myvi-key-U)
(define-key myvi-map "V" #'myvi-key-V)
(define-key myvi-map "W" #'myvi-key-W)
(define-key myvi-map "X" #'myvi-key-X)
(define-key myvi-map "Y" #'myvi-key-Y)
(define-key myvi-map "Z" #'myvi-key-Z)

(define-key myvi-map ":" #'myvi-key-colon)
(define-key myvi-map "$" #'myvi-key-dollar-sign)


(defcustom myvi-mode-hooks
  '(text-mode-hook
    prog-mode-hook
    conf-mode-hook
    fundamental-mode-hook)
  "Hooks where myvi-mode should be enabled."
  :type '(repeat symbol)
  :group 'myvi)

;;;###autoload
(defun myvi-setup ()
  "Setup myvi mode hooks."
  (interactive)
  (dolist (hook myvi-mode-hooks)
    (add-hook hook #'myvi-mode))
  (message "Myvi mode hooks are set up"))

;;;###autoload
(defun myvi-teardown ()
  "Remove myvi mode hooks."
  (interactive)
  (dolist (hook myvi-mode-hooks)
    (remove-hook hook #'myvi-mode))
  (message "Myvi mode hooks are removed"))


(provide 'myvi)
;;; myvi.el ends here
