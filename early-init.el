;; 在 early-init.el 中設置垃圾回收閾值為最大值
(setq gc-cons-threshold most-positive-fixnum)
;; 在初始化後恢復垃圾回收閾值
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; 禁用工具欄和滾動條
;; Prevent package.el loading packages prior to the init-file loading
(setq package-enable-at-startup nil)
(setq initial-frame-alist
      (quote ((fullscreen . maximized))))


(push '(tool-bar-mode . nil) default-frame-alist)
(push '(scroll-bar-mode . nil) default-frame-alist)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
