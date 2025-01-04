
(use-package myvi
  :load-path "core/myvi"
  :straight nil
  :hook (after-init . myvi-setup)
  )

(use-package pangu-spacing
  :init
  (global-pangu-spacing-mode 1)
  :config
  (setq word-wrap t)
  (setq word-wrap-by-category t)
  (setq pangu-spacing-real-insert-separtor t))


(provide 'core-editing)
;;; core-editing.el ends here
