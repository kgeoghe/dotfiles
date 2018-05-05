;----------------------;
;;; Custom Functions ;;;
;----------------------;

(defun lawlist-org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
       (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max)
        (if (eq state 'children)
      (save-excursion (outline-next-heading) (point))
          (org-end-of-subtree t)))))
  (goto-char beg)
  (while (re-search-forward "^.*DEADLINE:.*$\\|^\\*\\* Someday.*$\\|^\\*\\* None.*$\\|^\\*\\* Planning.*$\\|^\\* TASKS.*$\\|^.*:PROJECT:.*$" end t)
     (save-excursion
    (beginning-of-line 1)
    (when (looking-at "^.*DEADLINE:.*$\\|^\\*\\* Someday.*$\\|^\\*\\* None.*$\\|^\\*\\* Planning.*$\\|^\\* TASKS.*$\\|^.*:PROJECT:.*$")
      (let ((b (match-end 0)))
  (if (re-search-forward
       "^[ \t]*:END:"
       (save-excursion (outline-next-heading) (point)) t)
      (outline-flag-region b (point-at-eol) t)
    (user-error ":END: line missing at position %s" b))))))))))

;;;_, promote all subtree subitems on DONE
(defun my-org-after-todo-state-change ()
  (when (string-equal org-state "DONE")
    (show-branches)
    (org-promote-subtree)
    (org-do-demote)))

(require 'org-id)
;;, _ Allows me to easily clock into 'Break' task when timer expires (refer to 'org-settings.el')
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun my-pomodoro ()
  "Start pomodoro, clocking time in selected task"
  (interactive)
  (org-with-point-at (org-clock-select-task)
    (org-clock-in nil)
    (org-timer-set-timer '(16))))

  (let (id D1F660D2-BD02-42CF-877D-A8770B89CC1A)
    (org-with-point-at (org-id-find id 'marker)))
(let* ((name "Malabarba")
       (reputation "good")
       (fs (format-spec-make ?n name ?r reputation)))
  (format-spec "My name is %n, with a %r reputation.  %% is kept." fs))

;;, _ Redefining org-colview.el function to all org-column face attribute modifications
(require 'org-colview)
(defun org-columns-display-here (&optional props dateline)
  "Overlay the current line with column display."
  (interactive)
  (let* ((fmt org-columns-current-fmt-compiled)
	 (beg (point-at-bol))
	 (level-face (save-excursion
		       (beginning-of-line 1)
		       (and (looking-at "\\(\\**\\)\\(\\* \\)")
			    (org-get-level-face 2))))
	 (ref-face (or level-face
		       (and (eq major-mode 'org-agenda-mode)
			    (get-text-property (point-at-bol) 'face))
		       'default))
	 (color (list :foreground (face-attribute ref-face :foreground)))
	 (face (list color 'org-column ref-face))
	 (face1 (list color 'org-agenda-column-dateline ref-face))
	 (cphr (get-text-property (point-at-bol) 'org-complex-heading-regexp))
	 pom property ass width f fc string fm ov column val modval s2 title calc)
    ;; Check if the entry is in another buffer.
    (unless props
      (if (eq major-mode 'org-agenda-mode)
	  (setq pom (or (org-get-at-bol 'org-hd-marker)
			(org-get-at-bol 'org-marker))
		props (if pom (org-entry-properties pom) nil))
	(setq props (org-entry-properties nil))))
    ;; Walk the format
    (while (setq column (pop fmt))
      (setq property (car column)
	    title (nth 1 column)
	    ass (if (equal property "ITEM")
		    (cons "ITEM"
			  ;; When in a buffer, get the whole line,
			  ;; we'll clean it later…
			  (if (derived-mode-p 'org-mode)
			      (save-match-data
				(org-remove-tabs
				 (buffer-substring-no-properties
				  (point-at-bol) (point-at-eol))))
			    ;; In agenda, just get the `txt' property
			    (or (org-get-at-bol 'txt)
				(buffer-substring-no-properties
				 (point) (progn (end-of-line) (point))))))
		  (assoc property props))
	    width (or (cdr (assoc property org-columns-current-maxwidths))
		      (nth 2 column)
		      (length property))
	    f (format "%%-%d.%ds | " width width)
	    fm (nth 4 column)
	    fc (nth 5 column)
	    calc (nth 7 column)
	    val (or (cdr ass) "")
	    modval (cond ((and org-columns-modify-value-for-display-function
			       (functionp
				org-columns-modify-value-for-display-function))
			  (funcall org-columns-modify-value-for-display-function
				   title val))
			 ((equal property "ITEM")
			  (org-columns-cleanup-item
			   val org-columns-current-fmt-compiled
			   (or org-complex-heading-regexp cphr)))
			 (fc (org-columns-number-to-string
			      (org-columns-string-to-number val fm) fm fc))
			 ((and calc (functionp calc)
			       (not (string= val ""))
			       (not (get-text-property 0 'org-computed val)))
			  (org-columns-number-to-string
			   (funcall calc (org-columns-string-to-number
					  val fm)) fm))))
      (setq s2 (org-columns-add-ellipses (or modval val) width))
      (setq string (format f s2))
      ;; Create the overlay
      (org-with-silent-modifications
       (setq ov (org-columns-new-overlay
		 beg (setq beg (1+ beg)) string (if dateline face1 face)))
       (overlay-put ov 'keymap org-columns-map)
       (overlay-put ov 'org-columns-key property)
       (overlay-put ov 'org-columns-value (cdr ass))
       (overlay-put ov 'org-columns-value-modified modval)
       (overlay-put ov 'org-columns-pom pom)
       (overlay-put ov 'org-columns-format f)
       (overlay-put ov 'line-prefix "")
       (overlay-put ov 'wrap-prefix ""))
      (if (or (not (char-after beg))
	      (equal (char-after beg) ?\n))
	  (let ((inhibit-read-only t))
	    (save-excursion
	      (goto-char beg)
	      (org-unmodified (insert " ")))))) ;; FIXME: add props and remove later?
    ;; Make the rest of the line disappear.
    (org-unmodified
     (setq ov (org-columns-new-overlay beg (point-at-eol)))
     (overlay-put ov 'invisible t)
     (overlay-put ov 'keymap org-columns-map)
     (overlay-put ov 'intangible t)
     (overlay-put ov 'line-prefix "")
     (overlay-put ov 'wrap-prefix "")
     (push ov org-columns-overlays)
     (setq ov (make-overlay (1- (point-at-eol)) (1+ (point-at-eol))))
     (overlay-put ov 'keymap org-columns-map)
     (push ov org-columns-overlays)
     (let ((inhibit-read-only t))
       (put-text-property (max (point-min) (1- (point-at-bol)))
			  (min (point-max) (1+ (point-at-eol)))
			  'read-only "Type `e' to edit property")))))

(require 'org-agenda)
(defun org-agenda-get-restriction-and-command (prefix-descriptions)
  "The user interface for selecting an agenda command."
  (catch 'exit
    (let* ((bfn (buffer-file-name (buffer-base-buffer)))
	   (restrict-ok (and bfn (derived-mode-p 'org-mode)))
	   (region-p (org-region-active-p))
	   (custom org-agenda-custom-commands)
	   (selstring "")
	   restriction second-time
	   c entry key type match prefixes rmheader header-end custom1 desc
	   line lines left right n n1)
      (save-window-excursion
	(delete-other-windows)
	(org-switch-to-buffer-other-window " *Agenda Commands*")
	(erase-buffer)
	(insert (eval-when-compile
		  (let ((header
			 "Press key for an agenda command:        
--------------------------------
<   Buffer, subtree/region restriction              >   Remove restriction
a   Agenda for current week or day               e   Export agenda views
t   List of all TODO entries                               T   Entries with special TODO kwd
m   Match a TAGS/PROP/TODO query        M   Like m, but only TODO entries
s   Search for keywords                                   S   Like s, but only TODO entries
L   Timeline for current buffer                         #   List stuck projects (!=configure)
/   Multi-occur                                                     C   Configure custom agenda commands
?   Find :FLAGGED: entries                             *   Toggle sticky agenda views
")
			(start 0))
		    (while (string-match
			    "\\(^\\|   \\|(\\)\\(\\S-\\)\\( \\|=\\)"
			    header start)
		      (setq start (match-end 0))
		      (add-text-properties (match-beginning 2) (match-end 2)
					   '(face bold) header))
		    header)))
	(setq header-end (point-marker))
	(while t
	  (setq custom1 custom)
	  (when (eq rmheader t)
	    (org-goto-line 1)
	    (re-search-forward ":" nil t)
	    (delete-region (match-end 0) (point-at-eol))
	    (forward-char 1)
	    (looking-at "-+")
	    (delete-region (match-end 0) (point-at-eol))
	    (move-marker header-end (match-end 0)))
	  (goto-char header-end)
	  (delete-region (point) (point-max))

	  ;; Produce all the lines that describe custom commands and prefixes
	  (setq lines nil)
	  (while (setq entry (pop custom1))
	    (setq key (car entry) desc (nth 1 entry)
		  type (nth 2 entry)
		  match (nth 3 entry))
	    (if (> (length key) 1)
		(add-to-list 'prefixes (string-to-char key))
	      (setq line
		    (format
		     "%-4s%-14s"
		     (org-add-props (copy-sequence key)
			 '(face bold))
		     (cond
		      ((string-match "\\S-" desc) desc)
		      ((eq type 'agenda) "Agenda for current week or day")
		      ((eq type 'agenda*) "Appointments for current week or day")
		      ((eq type 'alltodo) "List of all TODO entries")
		      ((eq type 'search) "Word search")
		      ((eq type 'stuck) "List of stuck projects")
		      ((eq type 'todo) "TODO keyword")
		      ((eq type 'tags) "Tags query")
		      ((eq type 'tags-todo) "Tags (TODO)")
		      ((eq type 'tags-tree) "Tags tree")
		      ((eq type 'todo-tree) "TODO kwd tree")
		      ((eq type 'occur-tree) "Occur tree")
		      ((functionp type) (if (symbolp type)
					    (symbol-name type)
					  "Lambda expression"))
		      (t "???"))))
	      (if org-agenda-menu-show-matcher
		  (setq line
			(concat line ": "
				(cond
				 ((stringp match)
				  (setq match (copy-sequence match))
				  (org-add-props match nil 'face 'org-warning))
				 ((listp type)
				  (format "set of %d commands" (length type))))))
		(if (org-string-nw-p match)
		    (add-text-properties
		     0 (length line) (list 'help-echo
					   (concat "Matcher: " match)) line)))
	      (push line lines)))
	  (setq lines (nreverse lines))
	  (when prefixes
	    (mapc (lambda (x)
		    (push
		     (format "%s   %s"
			     (org-add-props (char-to-string x)
				 nil 'face 'bold)
			     (or (cdr (assoc (concat selstring
						     (char-to-string x))
					     prefix-descriptions))
				 "Prefix key"))
		     lines))
		  prefixes))

	  ;; Check if we should display in two columns
	  (if org-agenda-menu-two-columns
	      (progn
		(setq n (length lines)
		      n1 (+ (/ n 2) (mod n 2))
		      right (nthcdr n1 lines)
		      left (copy-sequence lines))
		(setcdr (nthcdr (1- n1) left) nil))
	    (setq left lines right nil))
	  (while left
	    (insert "\n" (pop left))
	    (when right
	      (if (< (current-column) 40)
		  (move-to-column 40 t)
		(insert "   "))
	      (insert (pop right))))

	  ;; Make the window the right size
	  (goto-char (point-min))
	  (if second-time
	      (if (not (pos-visible-in-window-p (point-max)))
		  (org-fit-window-to-buffer))
	    (setq second-time t)
	    (org-fit-window-to-buffer))

	  ;; Ask for selection
	  (message "Press key for agenda command%s:"
		   (if (or restrict-ok org-agenda-overriding-restriction)
		       (if org-agenda-overriding-restriction
			   " (restriction lock active)"
			 (if restriction
			     (format " (restricted to %s)" restriction)
			   " (unrestricted)"))
		     ""))
	  (setq c (read-char-exclusive))
	  (message "")
	  (cond
	   ((assoc (char-to-string c) custom)
	    (setq selstring (concat selstring (char-to-string c)))
	    (throw 'exit (cons selstring restriction)))
	   ((memq c prefixes)
	    (setq selstring (concat selstring (char-to-string c))
		  prefixes nil
		  rmheader (or rmheader t)
		  custom (delq nil (mapcar
				    (lambda (x)
				      (if (or (= (length (car x)) 1)
					      (/= (string-to-char (car x)) c))
					  nil
					(cons (substring (car x) 1) (cdr x))))
				    custom))))
	   ((eq c ?*)
	    (call-interactively 'org-toggle-sticky-agenda)
	    (sit-for 2))
	   ((and (not restrict-ok) (memq c '(?1 ?0 ?<)))
	    (message "Restriction is only possible in Org-mode buffers")
	    (ding) (sit-for 1))
	   ((eq c ?1)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction 'buffer))
	   ((eq c ?0)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction (if region-p 'region 'subtree)))
	   ((eq c ?<)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction
		  (cond
		   ((eq restriction 'buffer)
		    (if region-p 'region 'subtree))
		   ((memq restriction '(subtree region))
		    nil)
		   (t 'buffer))))
	   ((eq c ?>)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction nil))
	   ((and (equal selstring "") (memq c '(?s ?S ?a ?t ?m ?L ?C ?e ?T ?M ?# ?! ?/ ??)))
	    (throw 'exit (cons (setq selstring (char-to-string c)) restriction)))
           ((and (> (length selstring) 0) (eq c ?\d))
            (delete-window)
            (org-agenda-get-restriction-and-command prefix-descriptions))

	   ((equal c ?q) (error "Abort"))
	   (t (user-error "Invalid key %c" c))))))))


;; borrowed from http://oremacs.com/2015/01/12/dired-file-size/, which is originally from
; an emacs wiki page
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

;; Borrowed from http://ergoemacs.org/emacs/emacs_make_modern.html
(defun kg-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )

; unfill a paragraph, i.e., make it so the text does not wrap in the
; paragraph where the cursor is
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

; unfill a region, i.e., make is so the text in that region does not
; wrap
(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

(defun make-plugin-path (plugin)
  (expand-file-name
   (concat plugin-path plugin)))

(defun include-plugin (plugin)
  (add-to-list 'load-path (make-plugin-path plugin)))

(defun make-elget-path (plugin)
  (expand-file-name
   (concat elget-path plugin)))

(defun include-elget-plugin (plugin)
  (add-to-list 'load-path (make-elget-path plugin)))

(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file)
  (org-map-entries 'org-archive-subtree "/CANCELLED" 'file))

(provide 'custom-functions)
