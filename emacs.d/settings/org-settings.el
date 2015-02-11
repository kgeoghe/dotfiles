;-----------;
;;; Org mode ;;;
;-----------;

(setq org-use-fast-todo-selection t)
(setq org-use-tag-inheritance nil)
;(setq org-startup-indented t)
(define-key global-map "\C-cl" 'org-store-link)

(provide 'org-settings)
