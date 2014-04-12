;;; from Brent.Longborough's .emacs
(global-visual-line-mode 1); Proper line wrapping
(global-hl-line-mode 1); Highlight current row
(show-paren-mode 1); Matches parentheses and such in every mode
(set-fringe-mode '(0 . 0)); Disable fringe because I use visual-line-mode
(transient-mark-mode 0); Disable transient mark mode
(set-face-background hl-line-face "#f2f1f0"); Same color as greyness in gtk
(setq inhibit-splash-screen t); Disable splash screen
(setq visible-bell t); Flashes on error
(setq calendar-week-start-day 1); Calender should start on Monday
;(add-to-list 'default-frame-alist '(height . 54)); Default frame height.
;(add-to-list 'default-frame-alist '(width . 110)); Default frame width.
;(add-to-list 'initial-frame-alist '(top . 0))
;(add-to-list 'initial-frame-alist '(left . 100))

;;; from Charles Cave
(global-font-lock-mode t); already on by default
;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(set-variable 'confirm-kill-emacs 'yes-or-no-p)

;;; My additions
(tool-bar-mode -1); Hide toolbar by default

;; Set color theme on startup
(package-initialize)
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
;(color-theme-solarized-dark)
(color-theme-aalto-light)

;; Directs list-packages command to the MELPA repository (most packages and most up-to-date)
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; Allows use of 'recentf-mode' - builds a list of recently opened files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Allows use of 'C-x C-f' then 'C-tab' complete any recent filename from anywhere
(eval-after-load
       "filecache"
       '(progn
 	    (message "Loading file cache...")
 	    (file-cache-add-directory-using-find "~/Documents/Oklahoma/Thesis/Docs/OU-Thesis")
 	   ))

;; Use workgroups
;(require 'workgroups)
(add-to-list 'load-path "~/.emacs.d/elpa/workgroups2-20140225.1510")
(require 'workgroups2)
(setq wg-use-default-session-file nil)
;;Change prefix key (before activating WG)
(setq wg-prefix-key (kbd "C-c z"))
;;Change workgroups session file
(setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")
;;Set your own keyboard shortcuts to reload/save/switch WG:
(global-set-key (kbd "<pause>")     'wg-reload-session)
(global-set-key (kbd "C-S-<pause>") 'wg-save-session)
(global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
(global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)



;;;
;;; Load other elisp files
;;;

(load "~/.emacs.d/emacs_init_TeX"); AUCTeX & latexmk
(load "~/.emacs.d/emacs_init_orgMode"); Org Mode
(load "~/.emacs.d/emacs_init_remember"); Remember

;;;
;;; Load other elisp files
;;;



;;; Not wanted now
;;;

;; Extensible VI Layer (evil) for emacs - this should be before any custom-set-variables
;(add-to-list 'load-path "~/.emacs.d/evil") ; only without ELPA/el-get
;(require 'evil)
;(evil-mode 1)

;; Uses La Carte to access the recently opened files list
; (Note: Not a fan of these methods for locating recently opened files)
; Turn on 'icicles-mode' to use 'S-tab' command for enhanced functionality
;(require 'lacarte)
;(global-set-key [?\e ?\M-x] 'lacarte-execute-command)

;;;
;;; Not wanted now



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list (quote (("LatexMk" "latexmk %t" TeX-run-latexmk nil (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMk") ("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber") ("View" "open -a Skim.app %s.pdf" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(ansi-color-names-vector ["#0a2832" "#c60007" "#728a05" "#a57705" "#2075c7" "#c61b6e" "#259185" "#52676f"])
 '(background-color "#042028")
 '(background-mode dark)
 '(cursor-color "#708183")
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color "#708183")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ))

(workgroups-mode 1)
