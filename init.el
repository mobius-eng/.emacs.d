;; Setup MELPA
(require 'package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; manage packages via use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; Global settings
;; Operating on windows
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c f") 'make-frame)

;; *** Blink cursor - disable
(blink-cursor-mode -1)

;; *** Disable toolbar and menu-bar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; *** Disable visual feedback on selection
(transient-mark-mode 0)

;; *** Improve the title
(setq frame-title-format "%b - emacs")

;; *** No scroll bars
(scroll-bar-mode -1)

;; *** File encoding
;; Always use UNIX encoding
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


(setq ring-bell-function 'ignore)

(global-set-key (kbd  "C-c C-;") 'comment-region)
(global-set-key (kbd  "C-c M-;") 'uncomment-region)

;; *** Spaces only (no tabs)
(setq-default indent-tabs-mode nil)

;; *** Always show column number
(setq-default column-number-mode t)

(show-paren-mode t)

;; Fira works better with Matrix theme, but may need more tweaking
(set-frame-font "Fira Code Medium:spacing=100:size=12" nil t)
;; (set-frame-font "Cascadia Code:spacing=100:size=16" nil t)

;; *** Text auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq fill-column 80)
;; Something to do with autofill...
(add-hook 'text-mode-hook 'text-mode-hook-identify)

(defun is-digit-p (c)
  (and (>= c ?0)
       (<= c ?9)))

(defun get-zettl-num (x)
  (let ((i 0)
        (n (length x)))
    (while (and (< i n) (is-digit-p (elt x i)))
      (setq i (+ i 1)))
    (cons (substring x 0 i) (substring x i))))

(defun zettl-lessp (x1 x2)
  (let* ((r1 (get-zettl-num x1))
         (r2 (get-zettl-num x2))
         (n1 (string-to-number (car r1)))
         (n2 (string-to-number (car r2)))
         (y1 (cdr r1))
         (y2 (cdr r2))
         (ly1 (length y1))
         (ly2 (length y2)))
    (if (not (= n1 n2))
        (< n1 n2)
      (if (zerop ly1)
          t
        (if (zerop ly2)
            nil
          (let ((c1 (elt y1 0))
                (c2 (elt y2 0)))
            (if (= c1 c2)
                (zettl-lessp (substring y1 1) (substring y2 1))
              (= c1 ?.))))))))

(defun zettl-greaterp (x1 x2) (zettl-lessp x2 x1))

(defun display-inline-images-no-error ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))


(defun get-string-from-file (file-path)
  "Return file-path's file content"
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

;; Packages

(use-package gnu-elpa-keyring-update)

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

(use-package smart-mode-line
  :config
  (use-package smart-mode-line-powerline-theme))

(use-package bibtex)

(use-package helm
  :bind ("M-x" . helm-M-x)
  ;; :config
  ;; (use-package helm-bibtex)
)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-idle-delay 0.45
        company-minimum-prefix-length 3
        company-tooltip-limit 10))

(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 5))

(use-package ess
  :config
  (use-package ess-R-data-view)
  (use-package  ess-r-insert-obj)
  (use-package ess-smart-equals)
  ;; (use-package ess-smart-underscore)
  (use-package ess-view
    :config
    (use-package ess-view-data))
  (setq inferior-ess-r-program "R"))

(use-package stan-mode
  :config
  (use-package company-stan
    :hook (stan-mode . company-stan-setup))
  (use-package eldoc-stan
    :hook (stan-mode . eldoc-stan-setup)))


(use-package org
  :config
  (add-hook 'org-mode-hook
            #'(lambda ()
                (turn-on-font-lock)
                (setq org-use-speed-commands t)
                (setq org-src-preserve-identation t)
                (visual-line-mode 1)
                (org-modern-mode 1)
                (org-display-inline-images)
                (org-latex-preview 1)))
  (use-package org-tempo)
  (setq org-log-done 'time)
  (setq org-adapt-indentation nil)
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook
            'display-inline-images-no-error
            'append)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t) (C . t)))
  (add-to-list 'org-babel-default-header-args:R
             '(:session . "*R*"))
  ;; (add-to-list 'org-babel-default-header-args:R
  ;;            '((:width . 640) (:height . 640)))
  (setq org-babel-R-command "R --slave --no-save")
  (use-package org-modern
    :config
    (setq org-modern-star
          '("◉" " ○" "  ◈" "   ◇" "    ✳")))
  (use-package org-ref
    :bind (:map org-mode-map
                ("C-c ]" . org-ref-insert-link)
                ("C-c b" . helm-bibtex)))
  ;; (require 'org-ref-helm)
  (use-package org-tempo
    :config
    (tempo-define-template
     "org-header"
     '("#+TITLE: " n p
       "#+STARTUP: latexpreview" n
       "#+SETUPFILE: ~/.emacs.d/latex_header.org" n
       "#+PROPERTY: header-args :colnames yes :height 3 :width 5 :session *R*" n
       )
     "<t"
     "Insert a typical header"
     'org-tempo-tags))
  ;; LaTeX export settings
  (let ((default-directory "~/.emacs.d/"))
    (setq alexey-org-latex-preambule-memoir
          (get-string-from-file (expand-file-name "org-memoir.tex")))
    (setq alexey-org-latex-preambule-article
          (get-string-from-file (expand-file-name "org-article.tex"))))
  (setq org-latex-classes nil)
  (add-to-list 'org-latex-classes
               (list "memoir"
                     alexey-org-latex-preambule-memoir
                     '("\\chapter{%s}" . "\\chapter*{%s}")
                     '("\\section{%s}" . "\\section*{%s}")
                     '("\\subsection{%s}" . "\\subsection*{%s}")
                     '("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  ;; *** Article
  (add-to-list 'org-latex-classes
               (list "article"
                     alexey-org-latex-preambule-article
                     '("\\section{%s}" . "\\section*{%s}")
                     '("\\subsection{%s}" . "\\subsection*{%s}")
                     '("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  ;; *** Memoir is default
  (setq org-latex-default-class "memoir")
  (setq org-latex-listings 'minted)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org0iswitchb)))

(use-package dir-treeview
  :bind ("C-c d" . dir-treeview)
  :config
  (setq dir-treeview-compare-filenames-function 'zettl-greaterp)
  (setq dir-treeview-show-hidden-files nil)
  (setq dir-treeview-show-backup-files nil)
  (setq dir-treeview-default-root "~/")
  (setq dir-treeview-show-in-side-window t))



;; make R evals in ORG using session

;; uncomment if Emacs complains
;; (setq package-check-signature nil)
(setq package-list
      '(
        gnu-elpa-keyring-update
        company
	htmlize
        helm
        helm-bibtex
	; idle-highlight-mode - requires Emacs 29
	ido-completing-read+ ; formerly ido-ubiquitous
	ido-yes-or-no
	org
	; org-ac
	org-bullets
        ; org-latex-impatient
	; org-plus-contrib
        org-ref
	smex
	smyx-theme
	smooth-scrolling
	smart-mode-line
	smart-mode-line-powerline-theme
	material-theme
	solarized-theme
        the-matrix-theme
	ess
	ess-R-data-view
	ess-r-insert-obj
	ess-smart-equals
	ess-smart-underscore
	ess-view
	ess-view-data
	python-mode
        ; emacs-treeview
        dir-treeview))

;; Enable for package installation
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))



;; (require 'helm)

;; (global-set-key (kbd "M-x") 'helm-M-x)

;; ** General settings for Emacs




;; ** Load theme
; (load-theme 'solarized-dark t)
; (load-theme 'the-matrix t)

; (setq sml/no-confirm-load-theme t)
; (sml/setup)


;; ** ORG

; (require 'org-brain)

; needs tex2svg - how on Windows?
;; (require 'org-latex-impatient)

; (setq org-brain-path "~/zettel")

;; (setq org-agenda-files (directory-files-recursively "~/Dropbox/research2" "\\.org$"))

;; ** ESS

;; (setq inferior-ess-r-program "/C/Program\ Files/R/R-4.2.2/bin/R.exe")
;; (setq inferior-ess-r-program "R")
;; C:\Users\Cherkaeva\AppData\Local\Programs\Julia-1.10.0\bin\julia.exe
;; (setq inferior-julia-program "C:/Users/Cherkaeva/AppData/Local/Programs/Julia-1.10.0/bin/julia.exe")

;; (define-key bibtex-mode-map (kbd "C-c b") 'org-ref-bibtex-hydra/body)



;; ORG-REF settings -- uncomment

;; set bibliography
;; (setq bibtex-completion-bibliography '("~/Dropbox/research2/literature/library.bib"))
;; (setq bibtex-completion-library-path '("~/Dropbox/research2/literature/texts/"))
;; (setq bibtex-completion-notes-path "~/Dropbox/research2/literature/notes/")
;; (setq bibtex-completion-additional-search-fields '(keywords))
;; adjust bibtex-completion-notes-template-multiple-files for
;; ORG note file template

;; adjust bibtex-completion-display-formats if necessary

;; TODO: set bibtex-completion-pdf-open-function
;; (setq bibtex-completion-pdf-open-function
;;       (lambda (fpath)
;;         (call-process "~/Dropbox/research2/sioyek/sioyek.exe" nil 0 nil fpath)))


;; or 'org-ref-insert-link-hydra/body
;; C-c ] insert citation
;; C-u C-c ] insert cross-reference
;; C-u C-u C-c ] insert a label


;;;;;;;;;;;;;;;  Unused stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun is-letter-p (c)
;;   (or (and (>= c ?a)
;;            (<= c ?z))
;;       (and (>= c ?A)
;;            (<= c ?Z))))

;; (defun is-sep-p (c)
;;   (or (= c ?-)
;;       (= c ?_)
;;       (= c ?+)))


;; (defun dired-zettl (dir)
;;   "Run dired on DIR; display entries in proper order."
;;   (interactive "DDirectory: ")
;;   (dired (cons
;;           dir
;;           (sort (directory-files dir nil "[0-9.-]+\\.org$")
;;                 'zettl-lessp)) ;; see first posting
;;          "-lU")
;;   (dired-omit-mode 1)
;;   (dired-hide-details-mode 1))
                       





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(company-stan eldoc-stan stan-mode use-package org-modern dir-treeview python-mode ess-view-data ess-view ess-smart-underscore ess-smart-equals ess-r-insert-obj ess-R-data-view ess the-matrix-theme solarized-theme material-theme smart-mode-line-powerline-theme smart-mode-line smooth-scrolling smyx-theme smex org-ref org-bullets ido-yes-or-no ido-completing-read+ idle-highlight-mode helm-bibtex helm htmlize company gnu-elpa-keyring-update cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
