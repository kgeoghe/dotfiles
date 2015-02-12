;-----------;
;;; Fill Column Indicator ;;;
;-----------;

(require 'fill-column-indicator)

; this section would enable fci-mode for all buffers as a minor mode
;;(define-globalized-minor-mode
;; global-fci-mode fci-mode (lambda () (fci-mode 1)))
;;(global-fci-mode t)

;alternatively, I like to enable fci-mode for specific file types so I can keep writing buffers
;(e.g., TeX) fci free
(add-hook 'prog-mode-hook 'fci-mode)

(provide 'fill-column-indicator-settings)
