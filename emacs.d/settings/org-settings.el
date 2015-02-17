;-----------;
;;; Org mode ;;;
;-----------;

(require 'org)
(require 'org-agenda)

(setq org-use-fast-todo-selection t)
(setq org-use-tag-inheritance nil)
(setq org-startup-indented t)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done 'time)

;;_ , Basics for org-mode GTD a la jwiegly
;; http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner/

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(org-defkey org-mode-map "\C-ca" 'org-agenda)

;;;;;;;;;;;;;;;; begin from jwiegley dot-org.el

;;;_ . keybindings

(defvar org-mode-completion-keys
  '((?d . "DONE")
    (?g . "DELEGATED")
    (?n . "NOTE")
    (?r . "DEFERRED")
    (?s . "STARTED")
    (?t . "TODO")
    (?w . "WAITING")
    (?x . "CANCELED")
    (?y . "SOMEDAY")
    ))

(defvar org-todo-state-map nil)
(define-prefix-command 'org-todo-state-map)

(dolist (ckey org-mode-completion-keys)
  (let* ((key (car ckey))
         (label (cdr ckey))
         (org-sym (intern (concat "my-org-todo-" (downcase label))))
         (org-sym-no-logging
          (intern (concat "my-org-todo-" (downcase label) "-no-logging")))
         (org-agenda-sym
          (intern (concat "my-org-agenda-todo-" (downcase label))))
         (org-agenda-sym-no-logging
          (intern (concat "my-org-agenda-todo-"
                          (downcase label) "-no-logging"))))
    (eval
     `(progn
        (defun ,org-sym ()
          (interactive)
          (org-todo ,label))
        (bind-key (concat "C-c x " (char-to-string ,key)) ',org-sym)
        (defun ,org-sym-no-logging ()
          (interactive)
          (let ((org-inhibit-logging t))
            (org-todo ,label)))
        (bind-key (concat "C-c x " (char-to-string ,(upcase key)))
                  ',org-sym-no-logging)
        (defun ,org-agenda-sym ()
          (interactive)
          (let ((org-inhibit-logging
                 (let ((style (org-entry-get
                               (get-text-property (point) 'org-marker)
                               "STYLE")))
                   (and style (stringp style)
                        (string= style "habit")))))
            (org-agenda-todo ,label)))
        (define-key org-todo-state-map [,key] ',org-agenda-sym)
        (defun ,org-agenda-sym-no-logging ()
          (interactive)
          (let ((org-inhibit-logging t))
            (org-agenda-todo ,label)))
        (define-key org-todo-state-map [,(upcase key)]
          ',org-agenda-sym-no-logging)))))

(let ((map org-agenda-mode-map))
  (define-key map "\C-n" 'next-line)
  (define-key map "\C-p" 'previous-line)
  
  (define-key map "g" 'org-agenda-redo)
  (define-key map "f" 'org-agenda-date-later)
  (define-key map "b" 'org-agenda-date-earlier)
  (define-key map "r" 'org-agenda-refile)
  (define-key map " " 'org-agenda-tree-to-indirect-buffer)
  (define-key map "F" 'org-agenda-follow-mode)
  (define-key map "q" 'delete-window)
  (define-key map [(meta ?p)] 'org-agenda-earlier)
  (define-key map [(meta ?n)] 'org-agenda-later)
  (define-key map "x" 'org-todo-state-map)
  
  (define-key map ">" 'org-agenda-filter-by-top-headline)
  
  (define-key org-todo-state-map "z" 'make-bug-link))

;;(unbind-key "M-m" org-agenda-keymap)

;; (defun org-fit-agenda-window ()
;;   "Fit the window to the buffer size."
;;   (and (memq org-agenda-window-setup '(reorganize-frame))
;;        (fboundp 'fit-window-to-buffer)
;;        (fit-window-to-buffer)))

;;_ , TODO
;; (defadvice org-agenda-redo (after fit-windows-for-agenda-redo activate)
;;   "Fit the Org Agenda to its buffer."
;;   (org-fit-agenda-window))

;; (defadvice org-agenda (around fit-windows-for-agenda activate)
;;   "Fit the Org Agenda to its buffer."
;;   (let ((notes (directory-files
;;                 "~/Dropbox/Apps/Drafts/" t "[0-9].*\\.txt\\'" nil)))
;;     (with-current-buffer (find-file-noselect "~/Documents/Tasks/todo.txt")
;;       (save-excursion
;;         (goto-char (point-min))
;;         (re-search-forward "^\\* Inbox$")
;;         (re-search-forward "^:END:")
;;         (forward-line 1)
;;         (dolist (note notes)
;;           (insert
;;            "** TODO "
;;            (with-temp-buffer
;;              (insert-file-contents note)
;;              (goto-char (point-min))
;;              (forward-line)
;;              (unless (bolp))
;;              (insert ?\n)
;;              (insert (format "SCHEDULED: %s\n"
;;                              (format-time-string (org-time-stamp-format))))
;;              (goto-char (point-max))
;;              (unless (bolp)
;;                (insert ?\n))
;;              (let ((uuid (substring (shell-command-to-string "uuidgen") 0 -1))
;;                    (file (file-name-nondirectory note)))
;;                (insert (format (concat ":PROPERTIES:\n:ID: %s\n"
;;                                        ":CREATED: ") uuid))
;;                (string-match
;;                 (concat "\\`\\([0-9]\\{4\\}\\)"
;;                         "-\\([0-9]\\{2\\}\\)"
;;                         "-\\([0-9]\\{2\\}\\)"
;;                         "-\\([0-9]\\{2\\}\\)"
;;                         "-\\([0-9]\\{2\\}\\)"
;;                         "-\\([0-9]\\{2\\}\\)"
;;                         "\\.txt\\'") file)
;;                (let ((year (string-to-number (match-string 1 file)))
;;                      (mon (string-to-number (match-string 2 file)))
;;                      (day (string-to-number (match-string 3 file)))
;;                      (hour (string-to-number (match-string 4 file)))
;;                      (min (string-to-number (match-string 5 file)))
;;                      (sec (string-to-number (match-string 6 file))))
;;                  (insert (format "[%04d-%02d-%02d %s %02d:%02d]\n:END:\n"
;;                                  year mon day
;;                                  (calendar-day-name (list mon day year) t)
;;                                  hour min))))
;;              (buffer-string)))
;;           (delete-file note t)))
;;       (when (buffer-modified-p)
;;         (save-buffer))))
;;   ad-do-it
;;   (org-fit-agenda-window))
;;;;;;;;;;;;;;;; end from jwiegley dot-org.el

(require 'org-capture)

;(add-hook 'remember-mode-hook 'org-remember-apply-template)

(define-key global-map "\C-\M-r" 'org-capture)

(custom-set-variables
 '(org-agenda-files (quote ("~/Documents/Tasks/todo.txt")))
 '(org-default-notes-file "~/Documents/Tasks/notes.txt")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
	   ("c" todo "DONE|DEFERRED|CANCELLED" nil)
	   ("w" todo "WAITING" nil)
	   ("W" agenda "" ((org-agenda-ndays 21)))
	   ("A" agenda ""
	    ((org-agenda-skip-function
	      (lambda nil
		(org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
	     (org-agenda-ndays 1)
	     (org-agenda-overriding-header "Today's Priority #A tasks: ")))
	   ("u" alltodo ""
	    ((org-agenda-skip-function
	      (lambda nil
		(org-agenda-skip-entry-if (quote scheduled) (quote deadline)
					  (quote regexp) "\n]+>")))
	     (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
; '(org-remember-store-without-prompt t)
 '(org-capture-templates
   (quote
    (("t" "Task" entry
      (file+headline "~/Documents/Tasks/todo.txt" "Inbox")
      "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:" :prepend t)
     ("n" "Note" entry
      (file+headline "~/Documents/Tasks/notes.txt" "Notes")
      "* NOTE %?
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:" :prepend t)))))

 ;; '(remember-annotation-functions (quote (org-remember-annotation)))
 ;; '(remember-handler-functions (quote (org-remember-handler))))

(provide 'org-settings)
