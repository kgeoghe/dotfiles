;-----------;
;;; LaTeX ;;;
;-----------;

;;;_, allows table of contents viewing and navigating in AucTeX via 'C-c ='
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

; add texbin to env path so emac.app knows where to find TeX goodies
(getenv "PATH")
 (setenv "PATH"
(concat
 (getenv "PATH")
 ":" "/usr/texbin"
))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(include-plugin "auctex")
(load "auctex.el" -1 1 1)
(load "preview-latex.el" -1 1 1)

(if (system-is-mac)
    (progn
      (require 'tex-site)
      (require 'font-latex)
      (setq TeX-view-program-list
	    (quote 
	     (("Skim" 
	       (concat "/Applications/Skim.app/Contents/SharedSupport/displayline"
		       " %n %o %b")))))
      (setq TeX-view-program-selection 
	    (quote (((output-dvi style-pstricks) "dvips and gv") 
		    (output-dvi "xdvi") 
		    (output-pdf "Skim")
		    (output-html "xdg-open")))))

  (if (system-is-linux)
      (setq TeX-view-program-selection 
	     (quote (((output-dvi style-pstricks) "dvips and gv") 
		     (output-dvi "xdvi") 
		     (output-pdf "evince")
		     (output-html "xdg-open"))))))

; always start the server for inverse search
(setq-default TeX-source-correlate-start-server 0)

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (tex-pdf-mode 1)
            (TeX-source-correlate-mode 1)
            (setq fill-column 85)))

(require 'auctex-latexmk)
(auctex-latexmk-setup)

(defun run-latexmk ()
  (interactive)
  (save-buffer)
  (TeX-command "LatexMk" 'TeX-master-file 0)
  (if (plist-get TeX-error-report-switches (intern (TeX-master-file)))
      (next-error))) ;; 0 -> suppress confirmation

(define-key TeX-mode-map (kbd "C-0") 'run-latexmk)

(provide 'latex-settings)
