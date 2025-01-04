
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;;
  (electric-pair-mode 1)
  ;; Show matching parentheses
  (show-paren-mode 1)
  )


(use-package modus-themes
  :demand t
  :config
  ;; 添加你喜歡的自定義設置
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-region '(bg-only))

  (setq modus-themes-completions
        '((matches . (extrabold))
          (selection . (semibold accented))
          (popup . (accented))))

  (setq modus-themes-common-palette-overrides
      '((fg-main "#D3D3D3")))  ; 把主要文字顏色調整為更暗的灰色


  
  ;; 載入喜歡的主題
  (load-theme 'modus-vivendi t)   ; 載入深色主題

  :bind (("<f5>" . modus-themes-toggle))) ; 設定快捷鍵切換深淺主題


(use-package emacs
    :init
    ;; Remove the startup screen
    (setq inhibit-startup-message t)

    ;; Set the font
    (set-face-attribute 'default nil :font "M+ 1mn Light" :height 180)
    ;; Display line numbers
    (global-display-line-numbers-mode t)

    ;; Highlight the current line
    (global-hl-line-mode t)
    ;; Show matching parentheses
    (show-paren-mode t)
    ;; Disable backup files
    (setq make-backup-files nil)
    ;; Disable Auto Save
    ;;(setq auto-save-default nil)
)

(provide 'core-ui)
