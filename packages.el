(defvar my-packages
  '(
    ;;; core
    ;; core-setting
    use-package
    super-save
    rainbow-delimiters
    undo-fu
    undo-fu-session
    helpful

    ;; core-ui
    ;;doom-themes
    ;; doom-modeline
    modus-themes
    all-the-icons

    ;; core-editing
    pangu-spacing

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

    ;; notes & productivity
    consult-notes
    org
    org-contrib
    org-super-agenda
  ))


(dolist (package my-packages)
  (straight-use-package package))

;(macroexp-progn
; (mapcar (lambda (package)
;           `(use-package ,package
;              :straight t))
;         my-packages))

