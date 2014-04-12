;;; Remember

;;; Borrowed from Charles Cave
;(add-to-list 'load-path "L:/elisp/"); KG - don't think I need this
;(add-to-list 'load-path "L:/elisp/remember-2.0/"); KG - don't think I need this

(autoload 'remember "remember" nil t)
(autoload 'remember-region "remember" nil t)

(setq org-default-notes-file "~/org/.notes")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

;; Templates for the USB drive (F)
(setq org-remember-templates
    '(("Todo" ?t "* TODO %? %^g\n %i\n " "F:/GTD/newgtd.org" "Office")
     ("Journal" ?j "\n* %^{topic} %T \n%i%?\n" "L:journal.org")
     ("Book" ?b "\n* %^{Book Title} %t :READING: \n%[l:/booktemp.txt]\n" 
             "L:journal.org")
     ("Private" ?p "\n* %^{topic} %T \n%i%?\n" "F:/gtd/privnotes.org")
     ("Contact" ?c "\n* %^{Name} :CONTACT:\n%[l:/contemp.txt]\n" 
              "F:/gtd/privnotes.org")
     ))
;; Templates on my computer
(setq org-remember-templates
   '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "~/org/newgtd.org" "Tasks")
     ("Journal"   ?j "** %^{Head Line} %^g\n%U\n%i%?"  "~/org/journal.org")
     ("Clipboard" ?c "** %^{Head Line} %U %^g\n%c\n%?"  "~/org/journal.org")
     ("Receipt"   ?r "** %^{BriefDesc} %U %^g\n%?"   "~/org/finances.org")
     ("Book" ?b "** %^{Book Title} %t :BOOK: \n%[~/org/.book_template.txt]\n" 
        "~/org/journal.org")
     ("Film" ?f "** %^{Film Title} %t :FILM: \n%[~/org/.film_template.txt]\n" 
        "~/org/journal.org")
     ("Daily Review" ?a "** %t :COACH: \n%[~/org/.daily_review.txt]\n" 
        "~/org/journal.org")
     ("Someday"   ?s "*** %^{Someday Heading} %U\n%?\n"  "~/org/someday.org")
     ("Vocab"   ?v "** %^{Word?}\n%?\n"  "~/org/vocab.org")
    )
  )

(define-key global-map [f8] 'remember)
(define-key global-map [f9] 'remember-region)
