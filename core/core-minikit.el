(use-package vertico
  :init
  (vertico-mode)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  :config
  (keymap-set vertico-map "<backspace>" #'vertico-directory-delete-char)
  ;;(keymap-set vertico-map "TAB" #'minibuffer-complete)
  )

(use-package savehist
  :init
  (setq savehist-file (expand-file-name "history" salt-local-dir))
  (savehist-mode)
  )


(use-package orderless
  :defer nil
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  )

;;; consult
(use-package consult
  :init
  (defun consult-fd (&optional dir)
    "Search for regexp with fd in DIR."
    (interactive "P")
    (let ((consult-fd-cmd '("fd" "--color=never" "--full-path")))
      (pcase-let ((`(,prompt . ,default-directory) (consult--directory-prompt "fd" dir)))
        (consult--find prompt consult-fd-cmd))))
  :config
  (fset 'multi-occur #'consult-multi-occur))

;;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))


;;; embark
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'core-minikit)
;;; core-minikit.el ends here
