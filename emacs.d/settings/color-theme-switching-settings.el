;-----------;
;;; Color Theme Switcher ;;;
;-----------;

(require 'color-theme)
(color-theme-initialize)
 
(setq my-color-themes (list 'color-theme-solarized
                            'color-theme-aalto-light
;                            'color-theme-dark-laptop
;                            'color-theme-sitaramv-solaris
;                            'color-theme-sitaramv-nt
                              ))
 
(defun my-theme-set-default () ; Set the first row
      (interactive)
      (setq theme-current my-color-themes)
      (funcall (car theme-current)))
 
    (defun my-describe-theme () ; Show the current theme
      (interactive)
      (message "%s" (car theme-current)))
 
   ; Set the next theme (fixed by Chris Webber - thanks)
    (defun my-theme-cycle ()
      (interactive)
      (setq theme-current (cdr theme-current))
      (if (null theme-current)
      (setq theme-current my-color-themes))
      (funcall (car theme-current))
      (message "%S" (car theme-current)))
 
    (setq theme-current my-color-themes)
    (setq color-theme-is-global nil) ; Initialization
    (my-theme-set-default)
    (global-set-key [f5] 'my-theme-cycle)

(provide 'color-theme-switching-settings)
