;--------------------------------;
;;; General or Global Settings ;;;
;--------------------------------;

; set PATH, because we don't load .bashrc
; function from https://gist.github.com/jakemcc/3887459
(defun set-exec-path-from-shell-PATH ()
  ;(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))) ; don't think this is necessary
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo -n $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
 
(if window-system(set-exec-path-from-shell-PATH))

; language
(setq current-language-environment "English")

; don't show the startup screen
(setq inhibit-startup-screen 1)
; don't show the menu bar
(menu-bar-mode 1)
; don't show the tool bar
(require 'tool-bar)
(tool-bar-mode 0)
; don't show the scroll bar
(if window-system (scroll-bar-mode 0))

; when paging down, keep point in same relative position, vs moving it to top
(setq scroll-preserve-screen-position t)

;;;_ , Nicer bell settings
; found at http://www.emacswiki.org/emacs/AlarmBell#toc10
(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
;; (defun my-configure-visible-bell ()
;;   "Use a nicer visual bell in terminals."
;;   (if window-system
;;       (setq visible-bell t
;;             ring-bell-function nil)
;;     (setq visible-bell nil
;;           ring-bell-function 'my-terminal-visible-bell)))
(setq visible-bell t
      ring-bell-function 'my-terminal-visible-bell)

; first day of the week is Monday
(setq calendar-week-start-day 1)

; turn on mouse wheel support for scrolling
(require 'mwheel)
(mouse-wheel-mode 1)

; set command key to be meta instead of option
(if (system-is-mac)
    (setq ns-command-modifier 'meta))

; number of characters until the fill column 
(setq-default fill-column 80) ; was set to 70

; disabled by Kevin G on 31 Jan 2015
; each line of text gets one line on the screen (i.e., text will run
; off the left instead of wrapping around onto a new line)
(setq-default truncate-lines 0)
; truncate lines even in partial-width windows
(setq truncate-partial-width-windows 0)

; Kevin G prefers 'visual' line wrapping
(global-visual-line-mode 1)
; note: to disable this for any mode, add a mode hook
; e.g., (add-hook 'org-mode-hook (lambda ()
                                        ; (visual-line-mode -1)))
; also get rid of fringe - works better with visual line mode this way
(set-fringe-mode '(0 . 0))

; default window width and height
(defun custom-set-frame-size ()
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 178)))
(custom-set-frame-size)
(add-hook 'before-make-frame-hook 'custom-set-frame-size)

; window modifications
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
;; (global-set-key (kbd "S-C-<up>") 'enlarge-window)

; make end and home keys go to the start/end of buffers
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])

; always use spaces, not tabs, when indenting
(setq-default indent-tabs-mode nil)
; indentation styles
(setq c-basic-offset 8)
(setq c-default-style (quote (
    (c-mode . "bsd") 
    (java-mode . "java") 
    (awk-mode . "awk") 
    (other . "gnu"))))

; fixes silly indentation that yasnippet likes to replace spaces with
(setq yas-indent-line (quote none))

; ignore case when searching
(setq-default case-fold-search 1)

; set the keybinding so that you can use f4 for goto line
(global-set-key [f4] 'goto-line)

;; Show line numbers only when jumping there:
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

; require final newlines in files when they are saved
(setq require-final-newline nil)
; add a new line when going to the next line
(setq next-line-add-newlines t)

; show the current line and column numbers in the stats bar as well
(line-number-mode 1)
(column-number-mode 1)

; don't blink the cursor
(blink-cursor-mode 0)

; make sure transient mark mode is enabled (it should be by default,
; but just in case)
(transient-mark-mode 0) ; was formerly 1

; highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode 1)

; text decoration
(require 'font-lock)
;(setq font-lock-maximum-decoration 1)
(global-font-lock-mode 1)
(global-hi-lock-mode nil)
(setq jit-lock-contextually 1)
(setq jit-lock-stealth-verbose 1)

; if there is size information associated with text, change the text
; size to reflect it
(size-indication-mode 1)

; disable backup
(setq backup-inhibited t)
; disable auto save
(setq auto-save-default nil)

; Easy PG
(require 'epa-file)
(epa-file-enable)

;;;_ , Dired Settings (not quite enough for it's own elisp file yet)
(add-hook 'dired-mode-hook (lambda()
                             (local-set-key "z" 'dired-get-size)))

;; easy spell check
(global-set-key (kbd "<f9>") 'ispell-word)
(global-set-key (kbd "C-S-<f9>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f9>") 'flyspell-region)
(global-set-key (kbd "C-M-S-<f9>") 'flyspell-buffer)
(global-set-key (kbd "C-<f9>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f9>") 'flyspell-check-next-highlighted-word)

(provide 'general-settings)
