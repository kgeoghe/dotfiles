;;;;---------------;;;;
;;;;    MELPA   ;;;;
;;;;---------------;;;;

;;;_ , 15-02-17 Melpa repo package installs not working. Had to manually install bbdb (below), and bbdb-ext is not curretnly installed.
;;;_ , TODO make this fresh-install-loadable like el-get is

;;;_ , Begin  borrowed from https://github.com/dertuxmalwieder/My-Emacs-config/blob/master/.emacs
;; Add the MELPA repository:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))


;; Automatically install the packages I need:
(setq package-list '(
                     ;; Gnus extensions:
                     bbdb; Big Brother database
                     bbdb-ext; ... with extras
                     ))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))


;; Quick access to the package updater:
(defun update-packages ()
  (interactive)
  (package-refresh-contents)
  (package-list-packages)
  (package-menu-mark-upgrades)
  (package-menu-execute)
  (byte-compile-init-dir))
;;;_ , End  borrowed from https://github.com/dertuxmalwieder/My-Emacs-config/blob/master/.emacs

(provide 'melpa-settings)








