;; * Inferior LISP: SBCL is default
(setq slime-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix)
        (ccl ("ccl64"
              "-K" "utf-8"))))

;; * QUICKLISP SLIME helper
(defvar quicklisp-directories
  '("~/sbcl-quicklisp/"
    "~/.sbcl-quicklisp/"
    "~/ccl-quicklisp/"
    "~/.ccl-quicklisp/"
    "~/quicklisp/")
  "Possible locations of QUICKLISP")

(let ((continue-p t)
      (dirs quicklisp-directories))
  (while continue-p
    (cond ((null dirs) (message "Cannot find slime-helper.el"))
          ((file-directory-p (expand-file-name (car dirs)))
           (message "Loading slime-helper.el from %s" (car dirs))
           (load (expand-file-name "slime-helper.el" (car dirs)))
           (setq continue-p nil))
          (t (setq dirs (cdr dirs))))))

;; * Indentation
(eval-after-load "cl-indent"
  (progn
    (put 'make-instance 'common-lisp-indent-function 1)
    (put 'defconstant 'common-lisp-indent-function 3)))
;; * Autocomplete
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; (setq ac-auto-show-menu 0.3)

;; * Hooks
;; ** Utils: fixing FIXME TODO etc.
(defun new-warning-words ()
  "Remove FIX on its own from warning words"
  (font-lock-remove-keywords
   nil
   '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)" 1 font-lock-warning-face t)))
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)" 1 font-lock-warning-face t))))

;; ** LISP-MODE
(add-hook 'lisp-mode-hook (lambda ()
                            (rainbow-delimiters-mode t)
                            (smartparens-strict-mode t)
                            (idle-highlight-mode t)
                            (auto-complete-mode)
                            (new-warning-words)))
;; ** SLIME-MODE
(add-hook 'slime-mode-hook (lambda ()
                             (set-up-slime-ac)
                             (auto-complete-mode)))
;; ** SLIME-REPL
(add-hook 'slime-repl-mode-hook (lambda ()
                                  (rainbow-delimiters-mode t)
                                  (smartparens-strict-mode t)
                                  (set-up-slime-ac)
                                  (auto-complete-mode)))


;; * Square brackets
;; To use with infix-math reader macro
;; treat [ and ] as ( )
(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)



;; * Local HyperSpec
(setq common-lisp-hyperspec-root
      (concat "file:"
              (expand-file-name
               "~/Dropbox/References/Programming/Lisp/HyperSpec-7-0/HyperSpec/")))

