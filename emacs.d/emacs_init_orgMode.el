;;; Org Mode and MobileOrg

;; Automatically uses org-mode when opening '*.org' files
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Borrowed from Charles Cave
(custom-set-variables
 '(case-fold-search t)
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(make-backup-files nil)
 '(normal-erase-is-backspace t)
 '(org-agenda-files (quote ("~/org/birthday.org" "~/org/newgtd.org" "~/org/projects/")))
 '(org-agenda-ndays 7)
 '(org-agenda-repeating-timestamp-show-all nil)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up) (todo tag-up))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-todo-ignore-deadlines t)
 '(org-agenda-todo-ignore-scheduled t)
 '(org-agenda-todo-ignore-with-date t)
 '(org-agenda-window-setup (quote other-window))
 '(org-deadline-warning-days 7)
 '(org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">")
 '(org-fast-tag-selection-single-key nil)
 '(org-log-done (quote (done)))
; '(org-refile-targets (quote (("newgtd.org" :maxlevel . 1) ("someday.org" :level . 2))))
 '(org-reverse-note-order nil)
 '(org-tags-column -78)
 '(org-tags-match-list-sublevels nil)
 '(org-time-stamp-rounding-minutes '(0 5))
 '(org-use-fast-todo-selection t)
 '(org-use-tag-inheritance nil)
; '(unify-8859-on-encoding-mode t nil (ucs-tables))
)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Automatically indent lists and similar structures of Org buffers
;; http://tex.stackexchange.com/questions/54205/org-mode-export-to-latex-temptation-or-nuisance
(setq org-startup-indented t); Use virtual indentation for all files

; Not yet tested
;; PDFs visited in Org-mode are opened in Skim (and not in the default choice)
;; NOTE: I'm not expecting this to work. It's adapted from using evince to open PDFs.
;(eval-after-load "org"
;  '(progn
;     ;; Change .pdf association directly within the alist
;     (setcdr (assoc "\\.pdf\\'" org-file-apps) "skim %s")))

(setq org-log-done nil)
(setq org-agenda-include-diary nil)
(setq org-deadline-warning-days 7)
(setq org-timeline-show-empty-dates t)
(setq org-insert-mode-line-in-empty-file t)
; Uncomment following two commands if these are needed globally
;(setq org-todo-keywords '((sequence "TDOD(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|"
;                                    "DONE(d)" "CANCELLED(c)" "DEFERRED(f)")))
;(setq org-tag-alist '((:startgroup . nil) 
;                      ("OFFICE" . ?o) ("HOME" . ?h)
;                      (:endgroup . nil)
;                      ("COMPUTER" . ?c) ("PROJECT" . ?p) ("READING" . ?r)
;                      (:newline . nil)
;                      ("DVD" . ?d) ("LUNCHTIME" . ?l)))

; dont use tabs for indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)

; where org files reside on local machine
(setq org-directory "~/org/")

; More from Charles Cave
(setq org-agenda-exporter-settings
      '((ps-number-of-columns 1)
        (ps-landscape-mode t)
        (htmlize-output-type 'css)))

(setq org-agenda-custom-commands
       '(
; From Charles Cave
         ("D" "Daily Action List"
          (
           (agenda "" ((org-agenda-ndays 1)
                       (org-agenda-sorting-strategy
                        (quote ((agenda time-up priority-down tag-up) )))
                       (org-deadline-warning-days 0)
                       (org-agenda-overriding-header "Today")
                       ))))
         ("P" "Projects"   
          ((tags "PROJECT" ((org-agenda-overriding-header "PROJECT")))))
         ("H" "Office and Home Lists"
          ((agenda)
           (tags-todo "OFFICE" ((org-agenda-overriding-header "OFFICE")))
           (tags-todo "HOME" ((org-agenda-overriding-header "HOME")))
           (tags-todo "COMPUTER" ((org-agenda-overriding-header "COMPUTER")))
           (tags-todo "DVD" ((org-agenda-overriding-header "DVD")))
           (tags-todo "READING" ((org-agenda-overriding-header "READING")))))
         ("M" "Refile MobileOrg File" search "\*"
              ((org-agenda-files '("~/org/from-mobile.org"))
               (org-agenda-text-search-extra-files nil)))
; TODO Implement this agenda view when have time to define org-stuck-projects
;         ("W" "Weekly Review"
;          ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
                                           ;; type "l" in the agenda to review logged items 
;           (stuck "") ;; review stuck projects as designated by org-stuck-projects
;           (todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
;           (todo "MAYBE") ;; review someday/maybe items
;           (todo "WAITING"))) ;; review waiting items 
; From somewhere else on internet
;	 ("w" todo "TODO")
;         ("h" agenda "" ((org-agenda-show-all-dates nil)))
;         ("W" agenda "" ((org-agenda-ndays 21)
;                         (org-agenda-show-all-dates nil)))
;         ("A" agenda ""
;          ((org-agenda-ndays 1)
;           (org-agenda-overriding-header "Today")))
         ))

(defun gtd ()
    (interactive)
    (find-file "~/org/newgtd.org")
)
(global-set-key (kbd "C-c g") 'gtd)

(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; Refile parameters for easy note sorting
; Refiling C-c C-w
(setq org-refile-targets '(("~/org/newgtd.org" :maxlevel . 1)
                           ("~/org/someday.org" :level . 2)
                           ("~/org/vocab.org" :maxlevel . 1)
                           ("~/org/journal.org" :level . 1)
;                           ("~/org/info.org" :maxlevel . 2)
;                           ("~/org/projects.org" :maxlevel . 2)
;                           ("~/org/personal.org" :maxlevel . 2)
                           ))
; This allows for file like pathing for refiling
;  and lets me pick heading and subheading (level <= 2)
; Makes refiling a two-step process for most cases
;(setq org-refile-use-outline-path 1)

;; MobileOrg settings
(setq org-mobile-inbox-for-pull "~/org/from-mobile.org"); file where data is captured from mobile device
(setq org-mobile-directory "/Volumes/kgeoghe/org/"); where files will be synced ('staging' area)
(setq org-mobile-checksum-files "/Volumes/kgeoghe/org/checksums.dat")
(setq org-mobile-files '("~/org/newgtd.org"
                         "~/org/journal.org")); files to be pushed to mobile device
(setq org-mobile-force-id-on-agenda-items nil); makes it so org-mobile won't add property drawer to files

;; Encrypt data on WebDAV server (myDisk)
(setq org-mobile-use-encryption 1); Enable encryption
; Set a password
(setq org-mobile-encryption-password ")y3P;mCtUDjC4ZA>X7U]p]dP6G9Rrny4Xq/7pT4idB{y*V46:]")

;; 2014-03-08 Added auto-archive function from (https://github.com/martialboniou/emacs-revival/blob/master/org-wiegley-ext.el)
;; Refer to org-wiegley-ext.el file
(add-to-list 'load-path "~/.emacs.d")
(require 'org-wiegley-ext)
(add-to-list 'safe-local-variable-values '(after-save-hook . (archive-done-tasks)))

;; Word count for org-mode
(defvar count-words-buffer
  nil
  "*Number of words in the buffer.")

(defun wicked/update-wc ()
  (interactive)
  (setq count-words-buffer (number-to-string (count-words-buffer)))
  (force-mode-line-update))
  
; only setup timer once
(unless count-words-buffer
  ; seed count-words-paragraph
  ; create timer to keep count-words-paragraph updated
  (run-with-idle-timer 1 t 'wicked/update-wc))

; add count words paragraph the mode line
(unless (memq 'count-words-buffer global-mode-string)
  (add-to-list 'global-mode-string "words: " t)
  (add-to-list 'global-mode-string 'count-words-buffer t)) 

; count number of words in current paragraph
(defun count-words-buffer ()
  "Count the number of words in the current paragraph."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
	(forward-word 1)
        (setq count (1+ count)))
      count)))

; KG - I don't think this is working properly
;; Allows TO-DO lists to work with headlines
(defun wicked/org-update-checkbox-count (&optional all)
  "Update the checkbox statistics in the current section.
This will find all statistic cookies like [57%] and [6/12] and update
them with the current numbers.  With optional prefix argument ALL,
do this for the whole buffer."
  (interactive "P")
  (save-excursion
    (let* ((buffer-invisibility-spec (org-inhibit-invisibility)) 
	   (beg (condition-case nil
		    (progn (outline-back-to-heading) (point))
		  (error (point-min))))
	   (end (move-marker
		 (make-marker)
		 (progn (or (outline-get-next-sibling) ;; (1)
			    (goto-char (point-max)))
			(point))))   
	   (re "\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)")
	   (re-box
	    "^[ \t]*\\(*+\\|[-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
	   b1 e1 f1 c-on c-off lim (cstat 0))
      (when all
	(goto-char (point-min))
	(or (outline-get-next-sibling) (goto-char (point-max))) ;; (2)
	(setq beg (point) end (point-max)))
      (goto-char beg)
      (while (re-search-forward re end t)
	(setq cstat (1+ cstat)
	      b1 (match-beginning 0)
	      e1 (match-end 0)
	      f1 (match-beginning 1)
	      lim (cond
		   ((org-on-heading-p)
		    (or (outline-get-next-sibling) ;; (3)
			(goto-char (point-max)))
		    (point))
		   ((org-at-item-p) (org-end-of-item) (point))
		   (t nil))
	      c-on 0 c-off 0)
	(goto-char e1)
	(when lim
	  (while (re-search-forward re-box lim t)
	    (if (member (match-string 2) '("[ ]" "[-]"))
		(setq c-off (1+ c-off))
	      (setq c-on (1+ c-on))))
	  (goto-char b1)
	  (insert (if f1
		      (format "[%d%%]" (/ (* 100 c-on)
					  (max 1 (+ c-on c-off))))
		    (format "[%d/%d]" c-on (+ c-on c-off))))
	  (and (looking-at "\\[.*?\\]")
	       (replace-match ""))))
      (when (interactive-p)
	(message "Checkbox statistics updated %s (%d places)"
		 (if all "in entire file" "in current outline entry")
		 cstat)))))
(defadvice org-update-checkbox-count (around wicked activate)
  "Fix the built-in checkbox count to understand headlines."
  (setq ad-return-value
	(wicked/org-update-checkbox-count (ad-get-arg 1))))

;;; Added 3 Mar 2014 (http://permalink.gmane.org/gmane.emacs.orgmode/43518)
;; Use: Mark all desired events in Agenda and use "M-15 B f my-org-bul<TAB>"
(defun my-org-bulkshift-deadline ()
  "Shift the deadline of marked items in the agenda by 
  n days. Set n via Prefix Arg!"
  (interactive "P")
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 )
    (with-current-buffer buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char marker)
	  (org-back-to-heading t)
	  (when (and (org-entry-get (point) "DEADLINE") ; There is a deadline there
		     (numberp current-prefix-arg))      ; And current-prefix-arg is a number
	    (re-search-forward org-deadline-time-regexp)
	    (org-timestamp-change current-prefix-arg 'day)))))))
