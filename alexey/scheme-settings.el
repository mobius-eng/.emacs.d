;; * MIT-Scheme with SLIME: does not work - keep for future
;; (defun mit-scheme-init (file encoding)
;;   (format "%S\n\n"
;; 	  `(begin
;; 	    (load-option 'format)
;; 	    (load-option 'sos)
;; 	    (eval 
;; 	     '(construct-normal-package-from-description
;; 	       (make-package-description '(swank) '(()) 
;; 					 (vector) (vector) (vector) false))
;; 	     (->environment '(package)))
;; 	    (load ,(expand-file-name 
;; 		    ".../contrib/swank-mit-scheme.scm" ; <-- insert your path
;; 		    slime-path)
;; 		  (->environment '(swank)))
;; 	    (eval '(start-swank ,file) (->environment '(swank))))))

;; (defun mit-scheme ()
;;   (interactive)
;;   (slime 'mit-scheme))

;; (defun find-mit-scheme-package ()
;;   (save-excursion
;;     (let ((case-fold-search t))
;;       (and (re-search-backward "^[;]+ package: \\((.+)\\).*$" nil t)
;; 	   (match-string-no-properties 1)))))

;; (setq slime-find-buffer-package-function 'find-mit-scheme-package)
;; (add-hook 'scheme-mode-hook (lambda () (slime-mode 1)))



;; * Using MIT-Scheme:
;; ** MIT-Scheme specific commands and settings
;; (require 'xscheme)
;; ** Create ElDoc argument lists:
;; (require 'scheme-complete)
;; (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
;; ** Hokes
;; *** SCHEME-MODE
;; (setq geiser-active-implementations '(racket chicken))

(add-hook 'scheme-mode-hook
          (lambda ()
            ;; (make-local-variable 'eldoc-documentation-function)
            ;; (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            ;; (eldoc-mode)
            (rainbow-delimiters-mode t)
            (smartparens-strict-mode t)
            (idle-highlight-mode t)
            (company-mode t)
            (new-warning-words)))
;; *** INFERIOR-SCHEME-MODE
;; (add-hook 'inferior-scheme-mode-hook
;;           (lambda ()
;;             ;; (make-local-variable 'eldoc-documentation-function)
;;             ;; (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
;;             (rainbow-delimiters-mode t)
;;             (new-warning-words)))

;; ** SICM
;; (defun run-mechanics ()
;;   (interactive)
;;   (run-scheme 
;;     "/usr/local/scmutils/mit-scheme/bin/scheme --library /usr/local/scmutils/mit-scheme/lib -emacs"
;;     ))

;; (defun start-scheme ()
;;   (interactive)
;;   (run-scheme
;;    "C:\\Users\\alexe_000\\bin\\mit-scheme-9.2\\bin\\mit-scheme.exe -emacs"))


;; * Others: for future reference
;;; Gambit-C
(defun init-gambit ()
  (interactive)
  (add-to-list 'load-path "/usr/local/Gambit/share/emacs/site-lisp/gambc/")
  (require 'gambit)
  (setq scheme-program-name "gsc -:d-")
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

;; ;;; Checken Scheme
;; (defun init-chicken ()
;;   (interactive)
;;   (add-to-list 'load-path "/usr/local/Cellar/chicken/4.9.0.1/lib/chicken/7/")
;;   (autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
;;   (add-hook 'scheme-mode-hook
;;             (lambda ()
;;               (slime-mode t))))


;; (defun init-bigloo ()
;;   (interactive)
;;   (add-to-list 'load-path "/usr/local/share/emacs/bigloo")
;;   (autoload 'bdb "bdb" "bdb mode" t)
;;   (autoload 'bee-mode "bee-mode" "bee mode" t)
;;   (setq auto-mode-alist
;;         (append '(("\\.scm$" . bee-mode)
;;                   ("\\.sch$" . bee-mode)
;;                   ("\\.scme$" . bee-mode)
;;                   ("\\.bgl$" . bee-mode)
;;                   ("\\.bee$" . bee-mode))
;;                 auto-mode-alist)))
