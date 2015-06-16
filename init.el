;; * Load individual settings
(add-to-list 'load-path "~/.emacs.d/alexey/")
(load-library "general")
(load-library "org-settings")
;; (load-library "haskell-settings")
(load-library "lisp-settings")
(load-library "cdlatex")
;;; Theme
(load-theme 'material t)
;; One day I will set it up
;; (setq
;;    backup-by-copying t      ; don't clobber symlinks
;;    backup-directory-alist
;;     '(("." . "~/backup"))    ; don't litter my fs tree
;;    delete-old-versions t
;;    kept-new-versions 6
;;    kept-old-versions 2
;;    version-control t)       ; use versioned backups
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1c57936ffb459ad3de4f2abbc39ef29bfb109eade28405fa72734df1bc252c13" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
