;-----------;
;;; Org mode ;;;
;-----------;

(require 'org)
(require 'org-agenda)
(require 'org-timer)
(require 'org-install)
(add-to-list 'org-modules 'org-habit)
(require 'holidays)
(require 'solar)

(setq org-tags-column 0)
(setq org-use-fast-todo-selection t)
(setq org-use-tag-inheritance nil)
(setq org-startup-indented t)
(define-key global-map "\C-cl" 'org-store-link)

;;_ , Org habits settings
(setq org-habit-graph-column 48)
(setq org-habit-following-days 2)

;; record date/time task was marked DONE
(setq org-log-done 'time)

;;_ , Basics for org-mode GTD a la jwiegly
;; http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner/

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key org-mode-map "\C-xar" 'my-org-archive-done-tasks)
(define-key global-map "\C-ca" 'org-agenda)
;(org-defkey org-mode-map "\C-ca" 'org-agenda)

;;;_ , Timer
;; Set alarm for timer done
; not sure if this bit ("require 'play-sound") is necessary
; found at https://github.com/leoliu/play-sound-osx
(unless (and (fboundp 'play-sound-internal)
             (subrp (symbol-function 'play-sound-internal)))
  (require 'play-sound))

(add-hook 'org-timer-done-hook (lambda ()
                                 (play-sound-file "/Users/kgeoghe/Music/iTunes/iTunes Media/Music/Unknown Artist/Unknown Album/DoctorWho-Tardis-TimeMachine.mp3")
                                 (invert-face 'mode-line)
                                 (run-with-timer 0.4 nil 'invert-face 'mode-line)))

;; Default countdown value
(setq org-timer-default-timer 25)

;;;_ , Clocking work time settings
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
; from http://stackoverflow.com/questions/26405415/how-to-locally-unset-org-clock-into-drawer-t
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))     ;; Separate drawers for
;clocking and logs
(setq org-clock-into-drawer t)    ;; Save clock data and state changes and notes in the
;LOGBOOK drawer
(setq org-clock-idle-time 20)
(setq org-clock-modeline-total 'today)
; scan todo.txt by default for clock table info
(setq org-clocktable-defaults '(:scope ("~/Documents/Tasks/todo.txt") :emphasize t))
; I like to have a relative timer going whenever I clock in to a task
(add-hook 'org-clock-in-hook 'org-timer-start)

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
 '(org-agenda-files (quote ("~/org/journal.org" "~/Documents/Tasks/todo.txt")))
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
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:" :prepend t)
     ("n" "Note" entry
      (file+headline "~/Documents/Tasks/notes.txt" "Notes")
      "* NOTE %?
:PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %U
:END:" :prepend t)
          ("w" "Write Something" entry
      (file+headline "~/org/journal.org" "Current Month")
      "* %U :WRITE: \n %?")
          ("e" "Log Exercise" table-line
      (file+olp "~/org/journal.org" "Logs" "Exercise")
      "" :table-line-pos "II-2")
          ("b" "Book" entry
      (file "~/org/journal.org")
      "* %^{Title} %u :BOOK:
:PROPERTIES:
:Title: %\\1%^{Author}p%^{Year}p%^{Publisher}p
:END:")))))

 ;; '(remember-annotation-functions (quote (org-remember-annotation)))
 ;; '(remember-handler-functions (quote (org-remember-handler))))


;;;_ , Added 2015-04-25 from http://emacs.stackexchange.com/questions/10871/programmatically-add-birthdays-holidays-to-agenda-view-in-org-mode
(add-to-list 'org-agenda-custom-commands '(
  "Y" "365 Days -- holidays/birthdays" agenda "Year View" (
  (org-agenda-span 365)
  (org-agenda-time-grid nil)
  (org-agenda--show-holidays-birthdays t) )))

(defcustom org-agenda--show-holidays-birthdays nil
  "When non-`nil`, show holidays/birthdays in the agenda view."
  :group 'holidays)

(defcustom org-agenda--birthday-list (mapcar 'purecopy '(
  (holiday-fixed 1 2 "Jane Doe -- 01/02/1940")
  (holiday-fixed 2 15 "John Doe -- 02/15/1963")
  (holiday-fixed 3 2 "Seymoure Hersh -- 03/03/1999")
  (holiday-fixed 3 3 "Jashua Smith -- 03/03/1964")
  (holiday-fixed 3 5 "Frederick Holmes -- 03/05/1966")
  (holiday-fixed 4 7 "Fannie Mae -- 04/07/1970")
  (holiday-fixed 4 25 "Freddie Mack -- 04/25/1952")
  (holiday-float 5 0 2 "Mother's Day -- the second Sunday in May")
  (holiday-fixed 5 11 "George Lucas -- 05/11/1976")
  (holiday-fixed 5 18 "Harry Potter -- 05/18")
  (holiday-fixed 5 30 "Darth Vader -- 05/30/1972")
  (holiday-fixed 6 7 "Jabba the Hut -- 06/07/2007")
  (holiday-fixed 6 19 "Princess Lea -- 06/19/1983")
  (holiday-fixed 7 14 "Super Man -- 07/14/1970")
  (holiday-fixed 7 18 "Wonder Woman -- 07/18/1993")
  (holiday-fixed 10 3 "Jenifer Lopez (DOB:  10/03/2011)")
  (holiday-fixed 10 8 "Samuel Jacks (10/08/1965)")
  (holiday-fixed 10 25 "C3PO -- 10/25/2007")
  (holiday-fixed 11 14 "R2D2 -- 11/14/1981")
  (holiday-fixed 12 21 "Yoda -- 12/21/1958")
  (holiday-fixed 12 22 "Wookie -- 12/22/1967") ))
  "Birthdays."
  :type 'sexp
  :group 'holidays)

(defcustom org-agenda--holiday-list (mapcar 'purecopy '(
  (holiday-fixed 1 1 "New Year's Day")
  (holiday-float 1 1 3 "Martin Luther King Day")
  (holiday-float 2 1 3 "President's Day")
  (holiday-float 5 1 -1 "Memorial Day")
  (holiday-fixed 7 4 "Independence Day")
  (holiday-float 9 1 1 "Labor Day")
  (holiday-float 10 1 2 "Columbus Day")
  (holiday-fixed 11 11 "Veteran's Day")
  (holiday-float 11 4 4 "Thanksgiving")
  (holiday-fixed 12 25 "Christmas") ))
  "Custom holidays defined by the user."
  :type 'sexp
  :group 'holidays)

(defface org-agenda--holiday-face
  '((t (:foreground "red")))
  "Face for `org-agenda--holiday-face`."
  :group 'org-agenda)

(defface org-agenda--birthday-face
  '((t (:foreground "magenta")))
  "Face for `org-agenda--birthday-face`."
  :group 'org-agenda)

(defun org-agenda-list (&optional arg start-day span with-hour)
  "Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm."
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq arg (car org-agenda-overriding-arguments)
      start-day (nth 1 org-agenda-overriding-arguments)
      span (nth 2 org-agenda-overriding-arguments)))
  (if (and (integerp arg) (> arg 0))
      (setq span arg arg nil))
  (catch 'exit
    (setq org-agenda-buffer-name
    (or org-agenda-buffer-tmp-name
        (if org-agenda-sticky
      (cond ((and org-keys (stringp org-match))
       (format "*Org Agenda(%s:%s)*" org-keys org-match))
      (org-keys
       (format "*Org Agenda(%s)*" org-keys))
      (t "*Org Agenda(a)*")))
        org-agenda-buffer-name))
    (org-agenda-prepare "Day/Week")
    (setq start-day (or start-day org-agenda-start-day))
    (if (stringp start-day)
  ;; Convert to an absolute day number
  (setq start-day (time-to-days (org-read-date nil t start-day))))
    (org-compile-prefix-format 'agenda)
    (org-set-sorting-strategy 'agenda)
    (let* ((span (org-agenda-ndays-to-span
      (or span org-agenda-ndays org-agenda-span)))
     (today (org-today))
     (sd (or start-day today))
     (ndays (org-agenda-span-to-ndays span sd))
     (org-agenda-start-on-weekday
      (if (or (eq ndays 7) (eq ndays 14))
    org-agenda-start-on-weekday))
     (thefiles (org-agenda-files nil 'ifmode))
     (files thefiles)
     (start (if (or (null org-agenda-start-on-weekday)
        (< ndays 7))
          sd
        (let* ((nt (calendar-day-of-week
        (calendar-gregorian-from-absolute sd)))
         (n1 org-agenda-start-on-weekday)
         (d (- nt n1)))
          (- sd (+ (if (< d 0) 7 0) d)))))
     (day-numbers (list start))
     (day-cnt 0)
     (inhibit-redisplay (not debug-on-error))
     (org-agenda-show-log-scoped org-agenda-show-log)
     s e rtn rtnall file date d start-pos end-pos todayp
     clocktable-start clocktable-end filter)
      (setq org-agenda-redo-command
      (list 'org-agenda-list (list 'quote arg) start-day (list 'quote span) with-hour))
      (dotimes (n (1- ndays))
  (push (1+ (car day-numbers)) day-numbers))
      (setq day-numbers (nreverse day-numbers))
      (setq clocktable-start (car day-numbers)
      clocktable-end (1+ (or (org-last day-numbers) 0)))
      (org-set-local 'org-starting-day (car day-numbers))
      (org-set-local 'org-arg-loc arg)
      (org-set-local 'org-agenda-current-span (org-agenda-ndays-to-span span))
      (unless org-agenda-compact-blocks
  (let* ((d1 (car day-numbers))
         (d2 (org-last day-numbers))
         (w1 (org-days-to-iso-week d1))
         (w2 (org-days-to-iso-week d2)))
    (setq s (point))
    (if org-agenda-overriding-header
        (insert (org-add-props (copy-sequence org-agenda-overriding-header)
        nil 'face 'org-agenda-structure) "\n")
      (insert (org-agenda-span-name span)
        "-agenda"
        (if (< (- d2 d1) 350)
      (if (= w1 w2)
          (format " (W%02d)" w1)
        (format " (W%02d-W%02d)" w1 w2))
          "")
        ":\n")))
  (add-text-properties s (1- (point)) (list 'face 'org-agenda-structure
              'org-date-line t))
  (org-agenda-mark-header-line s))
      (while (setq d (pop day-numbers))
  (setq date (calendar-gregorian-from-absolute d)
        s (point))
  (if (or (setq todayp (= d today))
    (and (not start-pos) (= d sd)))
      (setq start-pos (point))
    (if (and start-pos (not end-pos))
        (setq end-pos (point))))
  (setq files thefiles
        rtnall nil)
  (while (setq file (pop files))
    (catch 'nextfile
      (org-check-agenda-file file)
      (let ((org-agenda-entry-types org-agenda-entry-types))
        ;; Starred types override non-starred equivalents
        (when (member :deadline* org-agenda-entry-types)
    (setq org-agenda-entry-types
          (delq :deadline org-agenda-entry-types)))
        (when (member :scheduled* org-agenda-entry-types)
    (setq org-agenda-entry-types
          (delq :scheduled org-agenda-entry-types)))
        ;; Honor with-hour
        (when with-hour
    (when (member :deadline org-agenda-entry-types)
      (setq org-agenda-entry-types
      (delq :deadline org-agenda-entry-types))
      (push :deadline* org-agenda-entry-types))
    (when (member :scheduled org-agenda-entry-types)
      (setq org-agenda-entry-types
      (delq :scheduled org-agenda-entry-types))
      (push :scheduled* org-agenda-entry-types)))
        (unless org-agenda-include-deadlines
    (setq org-agenda-entry-types
          (delq :deadline* (delq :deadline org-agenda-entry-types))))
        (cond
         ((memq org-agenda-show-log-scoped '(only clockcheck))
    (setq rtn (org-agenda-get-day-entries
         file date :closed)))
         (org-agenda-show-log-scoped
    (setq rtn (apply 'org-agenda-get-day-entries
         file date
         (append '(:closed) org-agenda-entry-types))))
         (t
    (setq rtn (apply 'org-agenda-get-day-entries
         file date
         org-agenda-entry-types)))))
      (setq rtnall (append rtnall rtn)))) ;; all entries
  (if org-agenda-include-diary
      (let ((org-agenda-search-headline-for-time t))
        (require 'diary-lib)
        (setq rtn (org-get-entries-from-diary date))
        (setq rtnall (append rtnall rtn))))
  ;; BEGIN -- MODIFICATON
  (when org-agenda--show-holidays-birthdays
    (setq rtn (org-agenda--get-birthdays-holidays))
    (setq rtnall (append rtnall rtn)))
  ;; END -- MODIFICATON
  (if (or rtnall org-agenda-show-all-dates)
      (progn
        (setq day-cnt (1+ day-cnt))
        (insert
         (if (stringp org-agenda-format-date)
       (format-time-string org-agenda-format-date
               (org-time-from-absolute date))
     (funcall org-agenda-format-date date))
         "\n")
        (put-text-property s (1- (point)) 'face
         (org-agenda-get-day-face date))
        (put-text-property s (1- (point)) 'org-date-line t)
        (put-text-property s (1- (point)) 'org-agenda-date-header t)
        (put-text-property s (1- (point)) 'org-day-cnt day-cnt)
        (when todayp
    (put-text-property s (1- (point)) 'org-today t))
        (setq rtnall
        (org-agenda-add-time-grid-maybe rtnall ndays todayp))
        (if rtnall (insert ;; all entries
        (org-agenda-finalize-entries rtnall 'agenda)
        "\n"))
        (put-text-property s (1- (point)) 'day d)
        (put-text-property s (1- (point)) 'org-day-cnt day-cnt))))
      (when (and org-agenda-clockreport-mode clocktable-start)
  (let ((org-agenda-files (org-agenda-files nil 'ifmode))
        ;; the above line is to ensure the restricted range!
        (p (copy-sequence org-agenda-clockreport-parameter-plist))
        tbl)
    (setq p (org-plist-delete p :block))
    (setq p (plist-put p :tstart clocktable-start))
    (setq p (plist-put p :tend clocktable-end))
    (setq p (plist-put p :scope 'agenda))
    (setq tbl (apply 'org-clock-get-clocktable p))
    (insert tbl)))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (unless (and (pos-visible-in-window-p (point-min))
       (pos-visible-in-window-p (point-max)))
  (goto-char (1- (point-max)))
  (recenter -1)
  (if (not (pos-visible-in-window-p (or start-pos 1)))
      (progn
        (goto-char (or start-pos 1))
        (recenter 1))))
      (goto-char (or start-pos 1))
      (add-text-properties (point-min) (point-max)
         `(org-agenda-type agenda
               org-last-args (,arg ,start-day ,span)
               org-redo-cmd ,org-agenda-redo-command
               org-series-cmd ,org-cmd))
      (if (eq org-agenda-show-log-scoped 'clockcheck)
    (org-agenda-show-clocking-issues))
      (org-agenda-finalize)
      (setq buffer-read-only t)
      (message ""))))

(defun org-agenda--get-birthdays-holidays ()
  "Add holidays/birthdays to the agenda view."
  (let* (
      (props (list
        'mouse-face 'highlight
        'org-not-done-regexp org-not-done-regexp
        'org-todo-regexp org-todo-regexp
        'org-complex-heading-regexp org-complex-heading-regexp
        'help-echo "Birthdays and Holidays"))
      (d1 (calendar-absolute-from-gregorian date))
      ee
      res-holidays
      res-birthdays
      (displayed-month (nth 0 date))
      (displayed-year (nth 2 date))
      (holiday-list
        (dolist (p org-agenda--holiday-list res-holidays)
          (let* (h)
           (when (setq h (eval p))
             (setq res-holidays (append h res-holidays))))))
      (birthday-list
        (dolist (p org-agenda--birthday-list res-birthdays)
          (let* (h)
           (when (setq h (eval p))
             (setq res-birthdays (append h res-birthdays)))))) )
    (when org-agenda--show-holidays-birthdays
      (mapcar
        (lambda (x)
          (let ((txt (format "%s -- holiday -- %s" (car x) (car (cdr x)))))
            (when (eq d1 (calendar-absolute-from-gregorian (car x)))
              (org-add-props txt props
                'ts-date d1
                ;; (char-to-string 65) = A; 66 = B; 67 = C; 68 = D; 69 = E
                'priority 65
                'type "holiday"
                'date d1
                'face 'org-agenda--holiday-face
                'org-hd-marker nil
                'org-marker nil
                'warntime nil
                'level nil
                'org-category nil
                'org-category-position nil
                'todo-state nil
                'undone-face nil
                'done-face nil)
              (push txt ee))))
        holiday-list)
      (mapcar
        (lambda (x)
          (let ((txt (format "%s -- birthday -- %s" (car x) (car (cdr x)))))
            (when (eq d1 (calendar-absolute-from-gregorian (car x)))
              (org-add-props txt props
                'ts-date d1
                ;; (char-to-string 65) = A; 66 = B; 67 = C; 68 = D; 69 = E
                'priority 65
                'type "birthday"
                'date d1
                'face 'org-agenda--birthday-face
                'org-hd-marker nil
                'org-marker nil
                'warntime nil
                'level nil
                'org-category nil
                'org-category-position nil
                'todo-state nil
                'undone-face nil
                'done-face nil)
              (push txt ee))))
        birthday-list))
    (nreverse ee)))

(provide 'org-settings)
