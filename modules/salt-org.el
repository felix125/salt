
(use-package org
  :init
  (setq org-directory "~/Dropbox/org")  ;設置 org 文件的默認目錄
  (load (expand-file-name "capture-templates.el" salt-template-dir))
  :config
  ; enable tempo
  (require 'org-tempo)
  
  (setq org-startup-indented t)           ;自動縮進
  (setq org-startup-folded t)             ;默認折疊所有標題
  (setq org-hide-leading-stars t)         ;隱藏多餘的標題星號
  ;; org-agenda-files
  (with-eval-after-load 'org
    (add-to-list 'org-agenda-files "~/Dropbox/notes/Orgzly")
    (add-to-list 'org-agenda-files "~/Dropbox/notes/Orgzly/journal"))

  
  ;TODO 關鍵字設置
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
          (sequence "HOLD(h@/!)" "REVIEW(v@)" "|" "CANCELLED(c@/!)")
          (sequence "NOTE(n!)" "IDEA(y!)" "|" "USED(u)")))

  (setq org-refile-targets '((nil :maxlevel . 2)  ; 允許 refile 到當前檔案
                             ("~/Dropbox/notes/Orgzly/notes.org" :maxlevel . 2)))

  (setq org-capture-templates capture-templates)

  )

(use-package org-super-agenda
  :after org-agenda
  :commands org-super-agenda-mode
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  ;;
  ;; 自定義分組
  (setq org-super-agenda-groups
        '((:name "Important"
                 :priority "A")
          (:name "Writing"
                 :tag "writing")
          (:name "In Progress"
                 :todo "IN-PROGRESS")
          (:name "On Hold"
                 :todo "HOLD")
          (:name "Review"
                 :todo "REVIEW")
          (:name "Ideas"
                 :todo ("NOTE" "IDEA"))
          (:name "Next Tasks"
                 :todo "TODO"))))

(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (setq consult-notes-file-dir-sources
        '(("Orgzly"   ?o "~/Dropbox/notes/Orgzly")
          ("Personal" ?n "~/Dropbox/notes"))))

(use-package my-journal
  :load-path "modules/my-journal"
  :straight nil
  :demand t
  :custom
  (my-journal-directory "~/Dropbox/notes/Orgzly/journal")
  (my-journal-date-format "* %A, %d %B")
  (my-journal-time-format "%H%M")
  (my-journal-file-format "%Y-%m.org")
  (my-journal-title-format "#+TITLE: Journal %Y-%m")
  )



(provide 'salt-org)
;;; salt-org.el ends here
