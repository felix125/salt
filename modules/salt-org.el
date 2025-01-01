
(use-package org
  :ensure t
  :init
  (setq org-directory "~/Dropbox/org")  ;設置 org 文件的默認目錄
  :config
  ;基礎設置
  (setq org-startup-indented t)           ;自動縮進
  (setq org-startup-folded t)             ;默認折疊所有標題
  (setq org-hide-leading-stars t)         ;隱藏多餘的標題星號
  
  ;TODO 關鍵字設置
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
          (sequence "HOLD(h@/!)" "REVIEW(v@)" "|" "CANCELLED(c@/!)")
          (sequence "NOTE(n)" "IDEA(y)" "|" "USED(u)")))

  (setq org-refile-targets '((nil :maxlevel . 2)  ; 允許 refile 到當前檔案
                             ("~/Dropbox/notes/Orgzly/notes.org" :maxlevel . 2)))


  (setq org-capture-templates
        '(("w" "Web Note" entry
           (file+headline "~/Dropbox/notes/capture/webnotes.org" "Web Notes")
           "* %^{Title} %^g
:PROPERTIES:
:Created: %U
:Source: %^{Source|網頁|PDF|影片|討論區}
:URL: %^{URL}
:END:

%?

%i")))

  )

(use-package org-super-agenda
  :after org-agenda
  :config
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



(provide 'salt-org)
;;; salt-org.el ends here
