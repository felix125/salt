;; Prevent package.el loading packages prior to init-file loading
(setq package-enable-at-startup nil)

;; Prevent frame resize flickering
(setq frame-inhibit-implied-resize t)

;; Set initial black background to prevent flash
(custom-set-faces '(default ((t (:background "#000000")))))

;; Frame settings
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (menu-bar-lines . 0)
        (fullscreen . maximized)))


