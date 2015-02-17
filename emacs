;; path where settings files are kept
(add-to-list 'load-path "~/.emacs.d/settings")
;; path to where plugins are kept
(setq plugin-path "~/.emacs.d/el-get/")
;; path to downloaded lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; define various custom functions
(require 'custom-functions)

;; configure general settings
(require 'general-settings)

;; install dependencies with el-get
(require 'el-get-settings)

;; TODO would like to get the melpa repo working for those packages that are not in the el-get repo
;(require 'melpa-settings)

;; added by Kevin G on 30 Jan 2015
;; configure eshell settings
(require 'eshell-settings)

;; configure org mode/jekyll settings (for blogging)
(require 'org-jekyll-settings)

;; use-package package by jwiegley
(require 'use-package)

;---------------;
;;; Utilities ;;;
;---------------;

;; Git
(include-plugin "magit")
(require 'magit)

;; Popup
(include-elget-plugin "popup")
(require 'popup)

;; Websocket
(include-plugin "websocket")
(require 'websocket)

;; Request
(include-plugin "request")
(require 'request)

;; yasnippet
(require 'yasnippet-settings)

;; Auto complete
(require 'auto-complete-settings)

;; Camelcase functions
(require 'camelcase-settings)

;; Helm
(require 'helm-settings)

;; added by Kevin G on 30 Jan 2015
;; Recent Files
(require 'recentf-settings)

;; added by Kevin G on 31 Jan 2015
;; Fill Column Indicator
(include-plugin "fill-column-indicator")
(require 'fill-column-indicator-settings)

;; added by Kevin G on 31 Jan 2015
;; Colors and Faces
(require 'color-face-settings)

;; added by Kevin G on 31 Jan 2015
;; Color theme switching
(require 'color-theme-switching-settings)

(require 'one-key-settings)

;-----------;
;;; Modes ;;;
;-----------;

;; Ido mode
(require 'ido)
(ido-mode 1)

;; MuMaMo
(require 'mumamo-settings)

;; Markdown mode
(require 'markdown-settings)

;; Python mode 
(require 'python-settings)

;; LaTeX and Auctex
(require 'latex-settings)

;; SCSS Mode
(require 'scss-settings)

;; Matlab mode
(require 'matlab-settings)

;; Javascript
(require 'js-settings)

;; Nyancat mode!
(nyan-mode 0)

;; Org mode
(require 'org-settings)

;; Gnus
(require 'gnus-settings)

;; ESV Mode
(require 'esv)



;---------------------------------------------------------------------
;; Put auto 'custom' changes in a separate file (this is stuff like
;; custom-set-faces and custom-set-variables)
(load 
 (setq custom-file (expand-file-name "settings/custom.el" user-emacs-directory))
 'noerror)





