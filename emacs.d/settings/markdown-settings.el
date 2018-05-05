;-------------------;
;;; Markdown mode ;;;
;-------------------;

(include-plugin "markdown-mode")
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (append 
       (list '("\\.text" . markdown-mode) 
	     '("\\.md" . markdown-mode) 
	     '("\\.markdown" . markdown-mode) 
	     )
       auto-mode-alist))

(custom-set-variables
 '(markdown-command "/usr/local/bin/pandoc"))

(provide 'markdown-settings)
