;;; sulfur.el --- Vi-like emulator -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 cervelleless
;;
;; Author: cervelleless <http://github.com/cervelleless>
;; Maintainer: cervelleless <cervelleless@gmail.com>
;; Created: November 27, 2020
;; Modified: November 27, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/felix/sulfur
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Vi-like emulator.  It's rudimentary,.
;;
;;; Code:

(require 'bind-key)
(require 'crux)

(defvar sulfur-cmd-map (make-sparse-keymap)
  "Keymap for sulfur-cmd-mode.")

;;;###autoload
(define-minor-mode sulfur-cmd-mode
  "A light version of vi emulator"
  :init-value t
  :lighter " sulfur-cmd-mode"
  :keymap sulfur-cmd-map
  (setq cursor-type 'box)
  (add-to-ordered-list 'emulation-mode-map-alists `((sulfur-cmd-mode . ,sulfur-cmd-map)) 0))

;;;###autoload
(define-globalized-minor-mode global-sulfur-cmd-mode sulfur-cmd-mode sulfur-cmd-mode)

;;;###autoload
(defun enter-sulfur-cmd-mode ()
  "Enter sulfur-cmd-mode."
  (interactive)
  "Enter sulfur-cmd-mode."
  (sulfur-cmd-mode 1))

;;;###autoload
(defun quit-sulfur-cmd-mode ()
  "Quit sulfur-cmd-mode."
  (interactive)
  "Quit sulfur-cmd-mode."
  (sulfur-cmd-mode -1)
  (setq cursor-type 'bar))

(add-hook 'minibuffer-setup-hook 'quit-sulfur-cmd-mode)


(defun sulfur/char-existp (chr)
  "Check the input CHR exist in the sulfur-string table or not."
  (let ((str (split-string "`1234567890-=~!@#$%^&*()_+qwertyuiop[]\\QWERTYUIOP{}|asdfghjkl;'ASDFGHJKL:\"zxcvbnm,./ZXCVBNM<>?" "")))
    (seq-contains-p str (char-to-string chr))))

;; commands
;;;###autoload
(defun sulfur/enter-editting (&optional arg)
  "Quit sulfur-cmd-mode for edit.  If the ARG is not nil, leave the message -- INSERT --."
  (interactive)
  (quit-sulfur-cmd-mode)
  (unless (boundp 'arg)
    (message "-- INSERT --")))

;;;###autoload
(defun sulfur/replace-char ()
  "Replace char."
  (interactive)
  (setq cursor-type '(hbar . 5))
  (let ((key (read-char)))
    (if (sulfur/char-existp key)
        (progn (insert key)
               (delete-char 1)
               (backward-char)
               (setq cursor-type 'box))
      (progn (message "Quit")
             (setq cursor-type 'box)))))


;;;###autoload

;;;###autoload
(defun sulfur/delete-relatives ()
  "Erase text relative commands."
  (interactive)
  (let ((key (read-char)))
    (if (sulfur/char-existp key)
	(cond ((char-equal key ?b) (backward-kill-word))
	      ((char-equal key ?d) (crux-kill-whole-line))
	      ((char-equal key ?w) (progn (forward-word)
					  (backward-word)
					  (kill-word 1)))
	      ((char-equal key ?0) (kill-line 0))
	      ((char-equal key ?$) (kill-line 1))
	      (t (message "d %s is undefined" (char-to-string key))))
      (message "Quit"))))


;; keybinding
(bind-keys :map sulfur-cmd-map
           ("a" . (lambda () (interactive)
		    (progn (unless (eolp)
			     (forward-char))
			   (sulfur/enter-editting))))
	   ("b" . backward-word)
	   ("cb" . (lambda () (interactive)
		     (progn (backward-kill-word 1)
			    (quit-sulfur-cmd-mode))))
	   ("cf" . (lambda () (interactive)
		     (kill-word 1)
		     (quit-sulfur-cmd-mode)))
	   ("cw" . (lambda () (interactive)
		     (progn (forward-word)
			    (backward-word)
			    (kill-word 1)
			    (quit-sulfur-cmd-mode))))
	   ("c$" . (lambda () (interactive)
		     (progn (kill-line 1)
			    (quit-sulfur-cmd-mode))))
	   ("d" . sulfur/delete-relatives)
	   ;; ("db" . backward-kill-word)
	   ;; ("dd" . crux-kill-whole-line)
	   ;; ("dw" . (lambda () (interactive)
	   ;; 	     (progn (forward-word)
	   ;; 		    (backward-word)
	   ;; 		    (kill-word 1))))
	   ;; ("d$" . (kill-line 1))
	   ;; ("d0" . (kill-line 0))
	   ("e" . nil)
	   ("f" . isearch-forward)
	   ("gg" . beginning-of-buffer)
	   ("h" . (lambda () (interactive)
		    (if (bolp)
			(message "Beginning of line")
		      (backward-char))))
	   ("i" . sulfur/enter-editting)
	   ("j" . next-line)
	   ("k" . previous-line)
	   ("l" . (lambda () (interactive)
		    (forward-char)
		    (if (eolp)
			(progn (message "End of line")
			       (backward-char 1)))))
	   ("m" . nil)
	   ("n" . isearch-repeat-forward)
	   ("o" . (lambda () (interactive)
		    (crux-smart-open-line nil)
		    (quit-sulfur-cmd-mode)))
	   ("p" . (lambda () (interactive)
		    (end-of-line)
		    (newline-and-indent)
		    (yank)
		    (delete-char -1)
		    (beginning-of-line)))
	   ("q" . nil)
	   ("r" . sulfur/replace-char)
	   ("sw" . swiper)
	   ("sr" . anzu-query-replace)
	   ("ss" . avy-goto-char-2)
	   ("t" . nil)
	   ("uu" . undo-tree-undo)
	   ("uv" . undo-tree-visualize)
	   ("v" . nil)
	   ("w" . forward-to-word)
	   ("x" . delete-char)
	   ("y" . yank)
	   ("z" . nil)
	   ("A" . (lambda () (interactive)
		    (move-end-of-line nil)
		    (quit-sulfur-cmd-mode)
		    (message "-- INSERT --")))
	   ("B" . nil)
	   ("C" . capitalize-word)
	   ("D" . crux-smart-kill-line)
	   ("E" . nil)
	   ("F" . nil)
	   ("G" . end-of-buffer)
	   ("H" . nil)
	   ("I" . (lambda () (interactive)
		    (crux-move-beginning-of-line nil)
		    (quit-sulfur-cmd-mode)))
	   ("J" . crux-top-join-line)
	   ("K" . nil)
	   ("L" . nil)
	   ("M" . nil)
	   ("N" . isearch-repeat-backward)
	   ("O" . (lambda () (interactive)
		    (crux-smart-open-line-above)
		    (quit-sulfur-cmd-mode)))
	   ("P" . (lambda () (interactive)
		    (beginning-of-line)
		    (yank)))
	   ("Q" . nil)
	   ("R" . nil)
	   ("S" . nil)
	   ("T" . nil)
	   ("U" . upcase-word)
	   ("V" . nil)
	   ("W" . nil)
	   ("X" . nil)
	   ("Y" . nil)
	   ("Z" . nil)
           ("0" . beginning-of-line)
	   (":!" . shell-command)
	   (":ww" . save-buffer)
	   ("-" . er/expand-region)
	   ("^" . (crux-move-beginning-of-line nil))
	   ("$" . (lambda () (interactive)
		    (move-end-of-line nil)
		    (backward-char)))
	   ;; ("," . )
	   ("M-x" . counsel-M-x)
	   ;; ;; ("<right>" sulfur/lambda-l)
	   ("<escape>" . keyboard-escape-quit)
	   ("SPC" . mercury/body))

;; (require 'hydra)
;; ;; (require 'bind-key)
;; ;; sulfur, salt and mercury.
;; (defhydra sulfur (:pre (progn (set-cursor-color "#40e0d0")
;;                               (setq cursor-type 'box))
;;                        :post (progn (setq cursor-type 'bar)
;;                                     (set-cursor-color "#ffffff"))
;;                        :hint nil
;;                        :color pink)
;;   "Salt vi emulator"
;;   ("a" (lambda () (interactive)
;;          (progn (unless (eolp)
;;                   (forward-char))
;;                 (message "-- INSERT --"))) :color blue)
;;   ("b" backward-word)
;;   ("cw" kill-word :color blue )
;;   ("dd" (lambda () (interactive)
;;           (crux-kill-whole-line)
;;           (delete-char 1)))
;;   ("dw" kill-word)
;;   ("d$" (kill-line 1))
;;   ("d0" (kill-line 0))
;;   ("e" nil)
;;   ("ff" avy-goto-char-timer)
;;   ("fj" avy-next)
;;   ("fk" avy-prev)
;;   ("gg" beginning-of-buffer)
;;   ("h" (lambda () (interactive)
;;          (if (bolp)
;;              (message "Beginning of line")
;;            (backward-char))))
;;   ("i" (message "-- INSERT --") :color blue)
;;   ("j" next-line)
;;   ("k" previous-line)
;;   ("l" (lambda () (interactive)
;;          (forward-char)
;;          (if (eolp)
;;              (progn (message "End of line")
;;                     (backward-char 1)))))
;;   ("m" nil)
;;   ("n" nil)
;;   ("o" (lambda () (interactive)
;;          (crux-smart-open-line nil)) :color blue)
;;   ("p" yank)
;;   ("q" nil)
;;   ("r" (lambda () (interactive)
;;          (insert (read-char))
;;          (delete-char 1)
;;          (backward-char)))
;;   ("sw" swiper)
;;   ("sr" anzu-query-replace)
;;   ("ss" avy-goto-char-2)
;;   ("t" nil)
;;   ("uu" undo-tree-undo)
;;   ("uv" undo-tree-visualize :color blue)
;;   ("v" nil)
;;   ("w" forward-word)
;;   ("x" (delete-char 1))
;;   ("y" nil)
;;   ("z" nil)
;;   ("A" (lambda () (interactive)
;;          (move-end-of-line nil)
;;          (message "-- INSERT --")) :color blue)
;;   ("B" nil)
;;   ("C" nil)
;;   ("D" crux-smart-kill-line)
;;   ("E" nil)
;;   ("F" nil)
;;   ("G" end-of-buffer)
;;   ("H" nil)
;;   ("I" crux-move-beginning-of-line :color blue)
;;   ("J" crux-top-join-line)
;;   ("K" nil)
;;   ("L" nil)
;;   ("M" nil)
;;   ("N" nil)
;;   ("O" crux-smart-open-line-above :color blue)
;;   ("P" nil)
;;   ("Q" nil)
;;   ("R" nil)
;;   ("S" nil)
;;   ("T" nil)
;;   ("U" nil)
;;   ("V" nil)
;;   ("W" nil)
;;   ("X" nil)
;;   ("Y" nil)
;;   ("Z" nil)
;;   ("0" beginning-of-line)
;;   ("!" shell-command)
;;   (":ww" save-buffer)
;;   ("-" er/expand-region)
;;   ("^" crux-move-beginning-of-line)
;;   ("$" (lambda () (interactive) (progn (move-end-of-line nil) (backward-char))))
;;   ("M-x" counsel-M-x :color blue)
;;   ;; ("<right>" sulfur/lambda-l)
;;   ("<escape>" keyboard-escape-quit)
;;   ("SPC" mercury/body :color blue))

(define-key global-map [escape] (lambda ()
                                  (interactive)
                                  (if (minibufferp)
                                      (keyboard-escape-quit)
                                    (unless (bolp)
                                      (backward-char))
                                    (enter-sulfur-cmd-mode))))

;; (add-hook 'after-init-hook 'sulfur/body)
;; (add-hook 'minibuffer-setup-hook 'sulfur/nil)
;; (add-hook 'minibuffer-exit-hook 'sulfur/body)
;; (add-hook 'eshell-mode-hook 'sulfur/nil)
;; (add-hook 'undo-tree-visualizer-mode 'sulfur/nil)
;; (advice-add 'undo-tree-visualizer-quit :after 'sulfur/body)
(provide 'sulfur)
;;; sulfur.el ends here
