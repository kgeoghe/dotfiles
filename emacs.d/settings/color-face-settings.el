;-----------;
;;; Color Theme and Face ;;;
;-----------;

(require 'color-theme)
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/el-get/custom-themes/emacs-color-theme-solarized/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defun set-gui-theme()
  (load-theme 'solarized 1)
  (setq solarized-termcolors 256)
  (set-frame-parameter (next-frame) 'background-mode 'dark)
  (enable-theme 'solarized))

(defun set-term-theme()
  (color-theme-matrix))

; use dark them by default for terminal
(if (display-graphic-p)
    (set-gui-theme)
  (set-term-theme))

(require 'faces)
(if (system-is-mac)
    (set-face-attribute 'default nil
			:foundry "apple" 
			:family "DejaVu_Sans_Mono"))

;;;_ , Org allows
; Mode proper alignment of Org Mode tables
(set-face-attribute 'org-table nil :foreground "Dark Gray"  :inherit 'fixed-pitch)
(set-face-attribute 'org-date nil :foreground "#d33682" :inherit 'fixed-pitch)
(set-face-attribute 'org-link nil :foreground "#d33682" :underline t :inherit 'fixed-pitch)

; default behavior is to have the org clock time part of the mode line 'highlighted' in every
; window--I like the whole mode line to have the same active/inactive color face settings
(set-face-attribute 'org-mode-line-clock nil :inherit nil)

; fixed-with font for org-volumn view--custom function defined to allow this (org-colview)
(set-face-attribute 'org-column nil :family "Lucida Console")
(set-face-attribute 'org-column-title nil :family "Lucida Console")

; don't use the hideous default red in level 4 org headings
(set-face-attribute 'org-level-4 nil :foreground "#859900")
; TODO better 'TODO' colors--might be nice, but currently my solarized theme overrides
;; (set-face-attribute 'org-todo nil
;;                     :weight 'bold :box '(:line-width 1 :color "#D8ABA7")
;;                     :foreground "#D8ABA7" :background "#FFE6E4")

;; org agenda
; all scheduled events should still be green, even if they were scheduled in the past
(set-face-attribute 'org-scheduled-previously nil :foreground "darkgreen")

;;;_ , Other Modes
;; Use variable width font faces in current buffer
 (defun my-buffer-face-mode-variable ()
   "Set font to a variable width (proportional) fonts in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "Symbola" :height 100 :width semi-condensed))
   (buffer-face-mode))

 ;; Use monospaced font faces in current buffer
 (defun my-buffer-face-mode-fixed ()
   "Sets a fixed width (monospace) font in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "Lucida Console"));:family "Inconsolata" :height 100))
   (buffer-face-mode))

 ;; Set default font faces for Info, ERC, and a few other modes
(add-hook 'gnus-summary-mode-hook 'my-buffer-face-mode-fixed)
(add-hook 'dired-mode-hook 'my-buffer-face-mode-fixed)
(add-hook 'one-key-mode-hook 'my-buffer-face-mode-fixed)
(add-hook 'calendar-mode-hook 'my-buffer-face-mode-fixed)
(add-hook 'help-mode-hook 'my-buffer-face-mode-fixed)
(add-hook 'buffer-menu-mode-hook 'my-buffer-face-mode-fixed)
(add-hook 'org-agenda-mode-hook 'my-buffer-face-mode-fixed)

(provide 'color-face-settings)
