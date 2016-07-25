;; * Load individual settings

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/alexey/")
(load-library "general")
;; (load-library "haskell-settings")
(load-library "lisp-settings")
(load-library "scheme-settings")
(load-library "clojure-settings")
(load-library "org-settings")
;; (load-library "cdlatex")
;;; Theme
;; (load-theme 'solarized-dark t)
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
    ("fa11f855b5f606f84e50106a7360c72aac88fee5f6fb8084aa4329009b61c5a2" "d3df47c843c22d8f0177fe3385ae95583dc8811bd6968240f7da42fd9aa51b0b" "54159ea82516378faa7c4d25fb549b843effb1eb932f0925dce1348de7a659ba" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(org-agenda-files
   (quote
    ("~/Dropbox/PhD/year-poroposal-2016/proposal-2016.org")))
 '(package-selected-packages
   (quote
    (popwin org-plus-contrib geiser w3m solarized-theme smyx-theme smooth-scrolling smex smartparens smart-mode-line-powerline-theme scheme-complete rainbow-delimiters ox-reveal ox-impress-js outshine orglue org2blog org-bullets org-ac material-theme magit ido-yes-or-no ido-ubiquitous idle-highlight-mode elisp-slime-nav ebib ctable company clojure-mode-extra-font-locking cider-eval-sexp-fu cider cdlatex auctex ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
