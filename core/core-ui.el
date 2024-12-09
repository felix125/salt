;; Use doom-themes theme
(use-package doom-themes
  :defer nil
  :config
  (setq doom-themes-enable-bold t    ; If set to nil, all bold fonts are disabled
        doom-themes-enable-italic t) ; If set to nil, all italic fonts are disabled
  (load-theme 'doom-one t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

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
