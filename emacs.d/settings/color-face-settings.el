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
                                        (set-frame-parameter frame-background-mode 'light)
                                        (enable-theme 'solarized))))

(defun set-term-theme()
  (color-theme-matrix))
  ;; (add-hook 'after-make-frame-functions (setq frame-background-mode 'dark)))
  ;;           (enable-theme 'solarized)))

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

(provide 'color-face-settings)
