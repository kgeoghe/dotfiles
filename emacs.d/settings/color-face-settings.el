;-----------;
;;; Color Theme and Face ;;;
;-----------;

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/el-get/custom-themes/"))
(load-theme 'solarized 1)
(setq solarized-termcolors 256)

(require 'faces)
(if (system-is-mac)
    (set-face-attribute 'default nil
			:foundry "apple" 
			:family "DejaVu_Sans_Mono"))

(provide 'color-face-settings)
