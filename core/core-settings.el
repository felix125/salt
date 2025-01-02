;;; core-settings.el --- The Core setting of Salt Emacs
;;; Commentary:
;;; Code:

;; deal with backup files
(use-package super-save
  :diminish  ; Do not show on modeline
  :config
  ;; Disable backup files
  (setq make-backup-files nil)
  ;; Disable Auto Save
  (setq auto-save-default nil)
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 5)
  (super-save-mode 1))


(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "places" salt-cache-dir))
  (setq save-place-limit 100)
  :config
  (save-place-mode 1))

;; recentf
(use-package recentf
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-save-file (expand-file-name "recentf" salt-cache-dir))
  :config
  (recentf-mode 1))

;; undo
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  ;; 使用之前定義的 salt-cache-dir
  (setq undo-fu-session-directory
        (expand-file-name "undo-fu-session" salt-cache-dir))

  ;; 限制每個檔案的歷史大小
  (setq undo-fu-session-file-limit (* 1024 1024))
  ;; 確保目錄存在
  (unless (file-directory-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory t))
  (global-undo-fu-session-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;;
  (electric-pair-mode 1)
  ;; Show matching parentheses
  (show-paren-mode 1)
  )

;; some settings
(use-package emacs
  :config
  ;; bookmark
  (setq bookmark-default-file (expand-file-name "bookmarks" salt-cache-dir))
  ;; custom.el
  (setq custom-file (expand-file-name "custom.el" salt-cache-dir))
  (when (file-exists-p custom-file)
    (load custom-file))
  ;; y-or-n
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; no tabs
  (setq indent-tabs-mode nil)
  ;; Set up the visible bell
  (setq visible-bell t)


  ;; 防止 *scratch* 被刪除
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  )


(use-package project
  :config
  (setq vc-handled-backends '(Git))
  (setq project-list-file (expand-file-name "projects" salt-cache-dir))
  :commands (projectile-add-known-project)
  :init
 (defun project-add-known-project ()
   (interactive)
   (let* ((dir (read-directory-name "Project root: "))
         (pr (cons 'transient dir)))
    (project-remember-project pr)
    (message "Added %s to projects" dir)))
)

(provide 'core-settings)
;;; core-settings.el ends here
