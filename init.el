;; Optimize garbage collection
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))


(setq default-input-method nil)

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
            (lambda ()
              (message "*** Emacs loaded in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)) )
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
;; 為 use-package 全局設置 :defer
(setq use-package-always-defer t)


;; 加載 packages.el
(load (expand-file-name "packages.el" user-emacs-directory))

;; 加載配置文件的路徑
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar salt-cache-dir (expand-file-name ".cache" user-emacs-directory))
(unless (file-directory-p salt-cache-dir)
  (make-directory salt-cache-dir))


;; 加載核心配置
(require 'core-settings)
(require 'core-ui)
(require 'core-minikit)
(require 'core-completion)
(require 'core-editing)


;; 加載功能模塊
;(require 'web-editing)
(require 'salt-org)


