;-----------;
;;; Color Theme and Face ;;;
;-----------;

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/el-get/custom-themes/emacs-color-theme-solarized/"))
(load-theme 'solarized 1)
(setq solarized-termcolors 256)
; initiate dark solarized
(add-hook 'after-make-frame-functions (lambda (frame)
                                        (set-frame-parameter frame-background-mode 'light)
                                        (enable-theme 'solarized)))


(require 'faces)
(if (system-is-mac)
    (set-face-attribute 'default nil
			:foundry "apple" 
			:family "DejaVu_Sans_Mono"))

(provide 'color-face-settings)
