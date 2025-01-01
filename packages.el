(defvar my-packages
  '(
    ;;; core
    ;; core-setting
    use-package
    super-save
    rainbow-delimiters
    undo-fu
    undo-fu-session
    ;; core-ui
    ;;doom-themes
    ;; doom-modeline
    modus-themes
    all-the-icons
    

    ;; core-minikit, enhance minibuffer
    vertico
    consult
    embark
    marginalia
    orderless
    embark-consult

    ;; core-completion
    corfu
    cape
    tempel

    ;;; modules
    ;; languages
    ruby-mode
    rubocop
    ;; web-editing
    web-mode
    emmet-mode

    ;; text-editing
    markdown-mode
    yaml
    pangu-spacing

    ;; notes & productivity
    org
    org-contrib
    deft
  ))


(dolist (package my-packages)
  (straight-use-package package))

;(macroexp-progn
; (mapcar (lambda (package)
;           `(use-package ,package
;              :straight t))
;         my-packages))

