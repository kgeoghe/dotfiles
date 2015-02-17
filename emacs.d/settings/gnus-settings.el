;-----------;
;;; GNUS ;;;
;-----------;

(autoload 'gmail2bbdb-import-file "gmail2bbdb" "" t)

;; borrowed from https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
;-*- Lisp -*-
(require 'nnir)

;;@see http://www.emacswiki.org/emacs/GnusGmail#toc1
(setq gnus-select-method '(nntp "news.gmane.org")) ;; if you read news groups 

;; ask encyption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq smtpmail-auth-credentials "~/.authinfo.gpg")

;;@see http://gnus.org/manual/gnus_397.html
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnimap-authinfo-file "~/.authinfo.gpg")
                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+gmail:Archives")
                      (nnmail-expiry-wait immediate)
                      ))

(setq gnus-parameters
      '(("INBOX"
         (auto-expire . t)
         (expiry-wait . 2))))

(setq nnmail-expiry-wait-function
           (lambda (group)
            (cond ((string= group "nnimap+gmail:@ Action")
                   never)
                  ((string= group "nnimap+gmail:[Gmail]/All Mail")
                   never)
                  ((string= group "nnimap+gmail:INBOX")
                    immediate)
                  )))

(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)

;; split works--tested 15-02-16
;; (setq nnimap-inbox "INBOX")
;; (setq nnimap-split-methods
;;  '(("INBOX.kevin" "^From:.*Kevin Geoghegan")))

;; (setq nnmail-expiry-target 'nnmail-fancy-expiry-target
;;             nnmail-fancy-expiry-targets
;;             '((to-from "boss" "nnfolder:Work")
;;               ("subject" "IMPORTANT" "nnfolder:IMPORTANT.%Y.%b")
;;               ("from" ".*" "nnfolder:Archive-%Y")))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-most-recent-date))

; NO 'passive
(setq gnus-use-cache t)

;; BBDB: Address list
(require 'bbdb-loaddefs "~/.emacs.d/lisp/bbdb/lisp/bbdb-loaddefs.el")
(bbdb-initialize 'message 'gnus 'mail)
(setq bbdb-file "~/.emacs.d/bbdb") ;; OPTIONAL, because I'm sharing my ~/.emacs.d
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(bbdb-mua-auto-update-init 'message)
(setq bbdb-mua-auto-update-p 'query)
;;;_ , this auto creates entries from /all/ received mail
;; (setq bbdb/mail-auto-create-p t
;;       bbdb/news-auto-create-p t)

;; auto-complete emacs address using bbdb's own UI
(add-hook 'message-mode-hook
          '(lambda ()
             (flyspell-mode t)
             (local-set-key "<TAB>"  'bbdb-complete-mail)))

;; Fetch only part of the article if we can.  I saw this in someone
;; else's .gnus
(setq gnus-read-active-file 'some)

;; Tree view for groups.  I like the organisational feel this has.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first
;; message.  'gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;; Personal Information
(setq user-full-name "Kevin Geoghegan"
      user-mail-address "kgeoghe@gmail.com")

(setq gnus-posting-styles
      '(((header "to" "kgeoghe@gmail.com")
         (address "kgeoghe@gmail.com"))
	((header "to" "keving@ou.edu")
         (address "keving@ou.edu"))
	((header "cc" "kgeoghe@gmail.com")
         (address "kgeoghe@gmail.com"))
	((header "cc" "keving@ou.edu")
         (address "keving@ou.edu"))))

;; You need install the command line browser 'w3m' and Emacs plugin 'w3m'
(setq mm-text-html-renderer 'w3m)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "kgeoghe@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")
;; http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
(setq gnus-use-correct-string-widths nil)

;; borrowed from TomRauchenwald at http://emacswiki.org/emacs/TomRauchenwald
;; eye candy
(copy-face 'font-lock-variable-name-face 'gnus-face-6)
(setq gnus-face-6 'gnus-face-6)
(copy-face 'font-lock-constant-face 'gnus-face-7)
(setq gnus-face-7 'gnus-face-7)
(copy-face 'gnus-face-7 'gnus-summary-normal-unread)
(copy-face 'font-lock-constant-face 'gnus-face-8)
(set-face-foreground 'gnus-face-8 "gray50")
(setq gnus-face-8 'gnus-face-8)
(copy-face 'font-lock-constant-face 'gnus-face-9)
(set-face-foreground 'gnus-face-9 "gray70")
(setq gnus-face-9 'gnus-face-9)
(setq gnus-summary-make-false-root 'dummy)
(setq gnus-summary-make-false-root-always nil)
(defun oxy-unicode-threads () 
  (interactive)
  (setq gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{│%}%) %6{□%}  %S\n"
	gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{│%} %6{%B%} %s\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "■ "
	gnus-sum-thread-tree-false-root "□ "
	gnus-sum-thread-tree-single-indent "▣ "
	gnus-sum-thread-tree-leaf-with-other "├─▶ "
	gnus-sum-thread-tree-vertical "│"
	gnus-sum-thread-tree-single-leaf "└─▶ "))

(defun oxy-unicode-threads-heavy () 
  (interactive)
  (setq gnus-summary-line-format "%8{%4k│%}%&user-date;%8{│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
	gnus-summary-dummy-line-format "    %8{│%}                %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
        gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "┏● " 
	gnus-sum-thread-tree-false-root " ○ "
	gnus-sum-thread-tree-single-indent " ● "
	gnus-sum-thread-tree-leaf-with-other "┣━━❯ " 
	gnus-sum-thread-tree-vertical "┃"
	gnus-sum-thread-tree-single-leaf "┗━━❯ "))

(oxy-unicode-threads-heavy)

(provide 'gnus-settings)
