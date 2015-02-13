;-----------;
;;; Color Theme and Face ;;;
;-----------;

(require 'color-theme)
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/el-get/custom-themes/emacs-color-theme-solarized/"))

; initiate dark solarized
(defun set-gui-theme()
  (load-theme 'solarized 1)
  (setq solarized-termcolors 256)
  (add-hook 'after-make-frame-functions (lambda (frame)
                                        (set-frame-parameter frame-background-mode 'dark)
                                        (enable-theme 'solarized))))

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

; allows proper alignment of Org Mode tables
(set-face-attribute 'org-table nil :foreground "Dark Gray"  :inherit 'fixed-pitch)

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

 ;; Set default font faces for Info and ERC modes
(add-hook 'gnus-summary-mode-hook 'my-buffer-face-mode-fixed)


(provide 'color-face-settings)
