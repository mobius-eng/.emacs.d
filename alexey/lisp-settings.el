;; SMARTPARENS
(sp-pair "(" ")" :wrap "M-(")
(sp-pair "[" "]" :wrap "M-[")
(sp-pair "{" "}" :wrap "M-{")
;; (define-key smartparens-mode-map (kbd "C-(") 'sp-forward-slurp-sexp)
(sp-use-paredit-bindings)
;; Set LISP SBCL:
;; mit-scheme does not work with SLIME
(setq slime-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix)
        (ccl ("ccl64"
              "-K" "utf-8"))))
;; Might work with slime, but still didn't figure out how
;; perhabs the support is outdated...
;; (mit-scheme ("mit-scheme") :init mit-scheme-init)


;; in theory, SLIME can support multiple implementations
;; in practice: only one at the time
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (load (expand-file-name "~/.ccl-quicklisp/slime-helper.el"))

;;; Autocomplete
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

(require 'ac-slime)


(add-hook 'lisp-mode-hook (lambda ()
                            (rainbow-delimiters-mode t)
                            (smartparens-strict-mode t)
                            ; (hl-sexp-mode t)
                            (idle-highlight-mode t)
                            (auto-complete-mode)
                            (font-lock-remove-keywords nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)" 1 font-lock-warning-face t)))
                            (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)" 1 font-lock-warning-face t)))))

(add-hook 'slime-mode-hook (lambda ()
                             (set-up-slime-ac)
                             (auto-complete-mode)
                             (when (string= (buffer-name) "*slime-scratch*")
                               (message "Defining s-e for scratch")
                               (local-set-key (kbd "s-e") #'slime-eval-print-last-expression))))

(add-hook 'slime-repl-mode-hook (lambda ()
                                  (rainbow-delimiters-mode t)
                                  (smartparens-mode t)
                                  (set-up-slime-ac)
                                  (auto-complete-mode)))

(defun slime-new-scratch ()
  (interactive)
  (slime-scratch)
  (local-set-key (kbd "C-j") #'slime-eval-print-last-expression))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; (setq ac-auto-show-menu 0.3)

;; To use with infix-math reader macro
;; treat [ and ] as ( )
(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)


;; Local HyperSpec
(setq common-lisp-hyperspec-root
      (concat "file:"
              (expand-file-name
               "~/Dropbox/References/Programming/Lisp/HyperSpec-7-0/HyperSpec/")))

;;; MIT-Scheme with SLIME:
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



;;; Using MIT-Scheme:
;; MIT-Scheme specific commands and settings
(require 'xscheme)
;; Create ElDoc argument lists:
(require 'scheme-complete)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            (eldoc-mode)
            (rainbow-delimiters-mode t)
            (font-lock-remove-keywords nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)" 1 font-lock-warning-face t)))
            (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)" 1 font-lock-warning-face t)))))

(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            (rainbow-delimiters-mode t)
            (font-lock-remove-keywords nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)" 1 font-lock-warning-face t)))
            (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)" 1 font-lock-warning-face t)))))

;; SICM
(defun run-mechanics ()
  (interactive)
  (run-scheme 
    "/usr/local/scmutils/mit-scheme/bin/scheme --library /usr/local/scmutils/mit-scheme/lib --emacs"
  ))

;;; Racket
(require 'racket-mode)

(add-hook 'racket-mode-hook
          (lambda ()
            (paredit-mode t)
            (rainbow-delimiters-mode t)
            (company-mode t)
            (font-lock-remove-keywords nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)" 1 font-lock-warning-face t)))
            (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)" 1 font-lock-warning-face t)))))

;;; Gambit-C
(defun init-gambit ()
  (interactive)
  (add-to-list 'load-path "/Library/Gambit-C/current/share/emacs/site-lisp/")
  (require 'gambit)
  (setq scheme-program-name "gsc -:d-")
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

;;; Checken Scheme
(defun init-chicken ()
  (interactive)
  (add-to-list 'load-path "/usr/local/Cellar/chicken/4.9.0.1/lib/chicken/7/")
  (autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
  (add-hook 'scheme-mode-hook
            (lambda ()
              (slime-mode t))))


(defun init-bigloo ()
  (interactive)
  (add-to-list 'load-path "/usr/local/share/emacs/bigloo")
  (autoload 'bdb "bdb" "bdb mode" t)
  (autoload 'bee-mode "bee-mode" "bee mode" t)
  (setq auto-mode-alist
        (append '(("\\.scm$" . bee-mode)
                  ("\\.sch$" . bee-mode)
                  ("\\.scme$" . bee-mode)
                  ("\\.bgl$" . bee-mode)
                  ("\\.bee$" . bee-mode))
                auto-mode-alist)))
