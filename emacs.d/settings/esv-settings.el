;-----------;
;;; ESV ;;;
;-----------;

(require 'esv)

; the following keys should be mapped to whatever works best for
; you:
; C-c e looks up a passage and displays it in a pop-up window
(define-key global-map [(control c) ?e] 'esv-passage)
; C-c i inserts an ESV passage in plain-text format at point
(define-key global-map [(control c) ?i] 'esv-insert-passage)
; If you don't want to use customize, you can set this for casual
; usage (but read http://www.esvapi.org/ for license):
(setq esv-key "IP")

(provide 'esv-settings)
