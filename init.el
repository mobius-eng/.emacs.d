;; * Load individual settings
(add-to-list 'load-path "~/.emacs.d/alexey/")
(load-library "general")
(load-library "org-settings")
;; (load-library "haskell-settings")
(load-library "lisp-settings")
;; (load-library "cdlatex")
;;; Theme
(load-theme 'smyx t)
;; One day I will set it up
;; (setq
;;    backup-by-copying t      ; don't clobber symlinks
;;    backup-directory-alist
;;     '(("." . "~/backup"))    ; don't litter my fs tree
;;    delete-old-versions t
;;    kept-new-versions 6
;;    kept-old-versions 2
;;    version-control t)       ; use versioned backups
