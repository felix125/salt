
(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t) ;; 自動彈出補全窗口
  (setq corfu-auto-prefix 1) ;; 在輸入至少一個字符後開始補全
  (setq corfu-auto-delay 0.5) ;; 延遲 0.5 秒後顯示補全窗口
  (setq corfu-cycle t) ;; 循環選擇候選項
  )

(use-package cape
  :defer nil
  :init
  ;; Add useful defaults completion sources from cape
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  )


(use-package emacs
  :config
  (setq dabbrev-check-all-buffers t) ;; 搜索所有打開的緩衝區
  )


(provide 'core-completion)
