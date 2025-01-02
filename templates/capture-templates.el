(setq capture-templates
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

