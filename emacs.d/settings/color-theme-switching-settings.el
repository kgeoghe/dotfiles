;-----------;
;;; Color Theme Switcher ;;;
;-----------;

(defun toggle-custom-color-theme ()
      "Switch to/from dark color scheme."
      (interactive)
      (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
          (set-frame-parameter (next-frame) 'background-mode 'light)
        (set-frame-parameter  (next-frame) 'background-mode 'dark))
      (enable-theme 'solarized))
    
(global-set-key [f5] 'toggle-custom-color-theme)

(provide 'color-theme-switching-settings)

; this is the script I originally tried to use, but this really works better when changing actual color themes from the color-theme package instead of individual attributes of a custom color theme
;; (require 'color-theme)
;; (load "~/.emacs.d/el-get/custom-themes/emacs-color-theme-solarized/color-theme-solarized.el")
 
;; (setq my-color-themes (list 'color-theme-solarized
;;                             'color-theme-aalto-light
;; ;                            'color-theme-dark-laptop
;; ;                            'color-theme-sitaramv-solaris
;; ;                            'color-theme-sitaramv-nt
;;                               ))
 
;; (defun my-theme-set-default () ; Set the first row
;;       (interactive)
;;       (setq theme-current my-color-themes)
;;       (funcall (car theme-current)))
 
;; (defun my-describe-theme () ; Show the current theme
;;       (interactive)
;;       (message "%s" (car theme-current)))
 
;; ; Set the next theme (fixed by Chris Webber - thanks)
;; (defun my-theme-cycle ()
;;       (interactive)
;;       (setq theme-current (cdr theme-current))
;;       (if (null theme-current)
;;           (setq theme-current my-color-themes))
;;       (funcall (car theme-current))
;;       (message "%S" (car theme-current)))
 
;; (setq theme-current my-color-themes)
;; (setq color-theme-is-global nil) ; Initialization
;; (my-theme-set-default)
;; (global-set-key [f5] 'my-theme-cycle)


