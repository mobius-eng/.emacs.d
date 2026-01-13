;; * Basic setup: MELPA & use-package
(require 'package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
;; uncomment if Emacs complains
(setq package-check-signature nil)

(setq package-list
  '(
	;; company-stan                   
	;; dir-treeview                   
	;; eldoc-stan                     
	;; ess-R-data-view                
	;; ess-r-insert-obj
	;; ess-smart-equals               
	;; ess-view                      
	;; ess-view-data                 
	;; gnu-elpa-keyring-update
	;; helm
	helm-bibtex                    
	;; helm-bibtexkey                 
	helm-org                       
	material-theme                 
	mindre-theme                   
	org-cite-overlay          
	org-modern          
	smart-mode-line-powerline-theme
	smooth-scrolling               
	solarized-theme                
	the-matrix-theme
  outshine))

;; Enable for package installation
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))


;; manage packages via use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; * Main vanila Emacs settings

;; ** Keys bindings
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c f") 'make-frame)
(global-set-key (kbd "C-c M-f") 'delete-frame)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd  "C-c C-;") 'comment-region)
(global-set-key (kbd  "C-c M-;") 'uncomment-region)


;; ** Interface

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

;; *** No ring bells
(setq ring-bell-function 'ignore)

;; *** Always show column number
(setq-default column-number-mode t)

(show-paren-mode t)

;; *** Font
;; Fira works better with Matrix theme, but may need more tweaking
(set-frame-font "Fira Code:spacing=100:size=14" nil t)
;; (set-frame-font "Cascadia Code:spacing=100:size=16" nil t)

;; (set-frame-font "Century Old Style Std:size=14" nil t)

;; *** Text auto-fill
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq fill-column 80)
;; Something to do with autofill...
(add-hook 'text-mode-hook 'text-mode-hook-identify)


;; ** File encoding

;; Always use UNIX encoding
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)


;; prevent Warnings to pop up
(setq warning-minimum-level :error)

;; ** Spaces only (no tabs)
(setq-default indent-tabs-mode nil)

;; ** C setup
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "k&r")))

;; * For ORG: insert image from clipboard

(defun display-inline-images-no-error ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))


(defun get-string-from-file (file-path)
  "Return file-path's file content"
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))


(defun org-insert-image-from-clipboard ()
  (interactive)
  (let* ((the-dir (file-name-directory buffer-file-name))
     (attachments-dir (file-name-as-directory (concat the-dir "media")))
     (png-file-name (format-time-string "%a%d%b%Y_%H%M%S.png"))
     (full-png-path (concat attachments-dir png-file-name))
     (rel-png-path (concat (file-name-as-directory "media") png-file-name))
     (script-path (concat (expand-file-name (file-name-as-directory user-emacs-directory)) "imgrab.py")))
    (if (not (file-exists-p attachments-dir))
        (make-directory attachments-dir)) 
    (call-process "python" nil t nil script-path rel-png-path full-png-path)
    ;; no need for that: the script writes into org directly
    ;; (insert (concat "[[./media/" png-file-name "]]"))
    (org-display-inline-images)
    ))

;; key C-c p is defined in `use-package org`
;; (define-key org-mode-map (kbd "C-S-v") 'org-insert-image-from-clipboard)

;; * Packages

;; ** GNU keyring -- never seems to work for me
;; (use-package gnu-elpa-keyring-update)


;; ** Themes
;; Uncomment which theme should be loaded

;; (use-package solarized-theme
;;   :config
;;   (load-theme 'solarized-dark t))

;; (use-package mindre-theme
;;     ; :ensure t
;;     ;; :custom
;;     ;; (mindre-use-more-bold nil)
;;     ;; (mindre-use-faded-lisp-parens t)
;;     :config
;;     (load-theme 'mindre t))

(load-theme 'leuven-dark)


;; (use-package the-matrix-theme
;;   :config
;;   (load-theme 'the-matrix t))


(use-package smart-mode-line
  :config
  (use-package smart-mode-line-powerline-theme))

;; ** bibtex
(use-package bibtex)


;; ** helm (better M-x)
(use-package helm
  :bind ("M-x" . helm-M-x)
  :config
  (use-package helm-bibtex)
)


;; ** company
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-idle-delay 0.45
        company-minimum-prefix-length 3
        company-tooltip-limit 10))

;; ** smooth-scrolling
(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 5))

;; ** ess
(use-package ess
  :config
  (use-package ess-R-data-view)
  (use-package  ess-r-insert-obj)
  (use-package ess-smart-equals)
  ;; (use-package ess-smart-underscore)
  (use-package ess-view
    :config
    (use-package ess-view-data))
  ;; (setq inferior-ess-r-program "R")
  ;; (setq inferior-ess-r-program "/C/Program\\ Files/R/R-4.4.1/bin/R.exe")
  ;; (setq inferior-ess-r-program "C:\\Program Files\\R\\R-4.4.1\\bin\\R.exe")
  (setq inferior-ess-r-program "/usr/bin/R")
  )

;; ** stan-mode
(use-package stan-mode
  :config
  (use-package company-stan
    :hook (stan-mode . company-stan-setup))
  (use-package eldoc-stan
    :hook (stan-mode . eldoc-stan-setup)))

;; ** outshine (org in source files)
;; outshine messes up R SRC blocks somehow
(use-package outshine
  :init
  (progn
    (defvar outline-minor-mode-prefix "\M-#"))
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'outshine-mode)
    (add-hook 'c-mode-hook 'outshine-mode))
  )



;; ** org

;; for non-breakable space in org
(defun insert-nbsp (&rest args)
  (interactive)
  (insert "\\nbsp{}"))



(use-package org
  :config
  (add-hook 'org-mode-hook
            #'(lambda ()
                (turn-on-font-lock)
                (variable-pitch-mode 1)
                (setq org-use-speed-commands t)
                (setq org-src-preserve-identation t)
                (visual-line-mode 1)
                (org-display-inline-images)
                (org-latex-preview 1)
                (setq org-hide-emphasis-markers t)
                (org-modern-mode 1)
                (setq fill-column 80)
                (olivetti-mode 1)
                (setq org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))
                ))
  (setq org-log-done 'time)
  (setq org-adapt-indentation nil)
  ;; use fixed-pitch for tables
  ;; it is set now in custom-theme-set-faces
  ;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
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
          '("◈" " ◉" "  ○" "   ◇" "    ✳")))
  (use-package olivetti)
  ;; new functionality for citations
  (use-package helm-org)
  (require 'oc)
  ;(setq org-cite-global-bibliography '("~/research/03-literature/lib.bib"))
  (use-package citar
    :ensure t
    :init
    (setq org-cite-insert-processor 'citar
          org-cite-follow-processor 'citar
          org-cite-activate-processor 'citar
          citar-bibliography org-cite-global-bibliography
          ; citar-notes-paths '("~/research/03-literature/notes/")
          )
    )
  (use-package vertico
    :ensure t
    :config
    (vertico-mode))

  (use-package orderless
    :ensure t
    :init
    (setq completion-styles '(orderless basic)
          completion-category-overrides
          '((file (styles basic partial-completion)))))
  
  (use-package embark
    :after vertico
    :ensure t)

  (use-package marginalia
    :after vertico
    :ensure t
    :config
    (marginalia-mode))

  (use-package citeproc
    :ensure t)

  (use-package org-download
    :config
    (progn
      (setq org-download-method 'directory)
      (setq org-download-image-org-width 600)
      (setq org-download-link-format "[[file:%s]]\n"
            org-download-abbreviate-filename-function #'file-relative-name)
      (setq org-download-link-format-function #'org-download-link-format-function-default)
      ))
  
  (require 'org-tempo)
  ; (setq lit-path (expand-file-name "~/research/03-literature"))
  (tempo-define-template
   "org-header"
   '("#+TITLE: " n p
     "#+STARTUP: latexpreview" n
     "#+SETUPFILE: ~/.emacs.d/latex_header.org" n
     "#+PROPERTY: header-args :colnames yes :height 3 :width 5 :session *R*" n
     "#+LATEX_CLASS: article" n
     "#+AUTHOR: Alexey V. Cherkaev" n
     (format "#+BIBLIOGRAPHY: %s/lib.bib" lit-path) n
     (format
      "#+CITE_EXPORT: csl %s/ieee.csl"
      lit-path) n
     )
   "<t"
   "Insert a typical header"
   'org-tempo-tags)
  (tempo-define-template
   "r-plot"
   '("#+begin_src R :results graphics file :file media/file.svg :exports both" n
     "" n p
     "" n
     "#+end_src" n
     )
   "<rp"
   "Insert R for plot output"
   'org-tempo-tags)
  (tempo-define-template
   "r-table"
   '("#+begin_src R :results table :exports both" n
     "" n p
     "" n
     "#+end_src" n
     )
   "<rt"
   "Insert R for table output"
   'org-tempo-tags)
  ;; LaTeX export settings
  (setq org-preview-latex-image-directory "media/")
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
  (setq org-latex-pdf-process
        ;; -output-directory=%o/pdf
        '("latexmk -shell-escape -f -pdf -%latex -interaction=nonstopmode -output-directory=pdf/%o %f"))
  ;; Custom link (unused)
  (org-link-set-parameters
   "myfile"
   :follow 'my-org-open-file
   :complete 'org-file-complete-link)

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c p" . org-insert-image-from-clipboard)
         ("C-c i" . citar-insert-citation)
         ("C-c n" . citar-open-notes)
         ("C-~"   . insert-nbsp)
         )
  

  ) ;; org



;; ** dir-treeview (disabled; superceeded by treemacs)
;; (use-package dir-treeview
;;   :bind ("C-c d" . dir-treeview)
;;   :config
;;   (setq dir-treeview-compare-filenames-function 'zettl-greaterp)
;;   (setq dir-treeview-show-hidden-files nil)
;;   (setq dir-treeview-show-backup-files nil)
;;   (setq dir-treeview-default-root "~/OneDrive/Documents/Research/")
;;   (setq dir-treeview-show-in-side-window t))

;; ** treemacs
;; *** load package
(use-package treemacs
  :bind ("C-c t" . 'treemacs-select-window)
  ;; :config
  )


;; ** multiple-cursors
(use-package multiple-cursors
  :bind ("C-S-c C-S-c" . mc/edit-lines)
  )

;; *** main settings (cannot be in use-package for some reason)
;; (progn
;;   (require 'treemacs)
;;   (setf
;;    (treemacs-current-workspace)
;;    (treemacs-workspace->create!
;;     :name "Research2"
;;     :projects
;;     (list
;;      (treemacs-project->create!
;;       :name "Research2"
;;       :path (expand-file-name "~/research/")
;;       :path-status 'local-readable)
;;      )))
;;   (treemacs--persist))

;; ** Fix for org-ctags
;;; work-around  for org-ctags obnoxious behavior
(with-eval-after-load 'org-ctags (setq org-open-link-functions nil))


;; * Custom theme settings (for variable pitch font)
;; "Calibri"
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Century Schoolbook" :height 100 :slant normal))))
 '(fixed-pitch ((t (:family "CMU Typewriter Text" :height 90))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-block ((t (:inherit variable-pitch :slant italic))))
 '(org-code ((t (:inherit variable-pitch :slant italic))))
 '(org-level-1 ((t (:inherit variable-pitch :height 1.5))))
 '(org-level-2 ((t (:inherit variable-pitch :height 1.3))))
 '(org-level-3 ((t (:inherit variable-pitch :slant italic :height 1.2))))
 )

;; :foundry "outline" 
 ;; (custom-theme-set-faces
 ;;   'user
 ;;   '(org-block ((t (:inherit fixed-pitch))))
 ;;   '(org-code ((t (:inherit (shadow fixed-pitch)))))
 ;;   '(org-document-info ((t (:foreground "dark orange"))))
 ;;   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 ;;   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 ;;   '(org-link ((t (:foreground "royal blue" :underline t))))
 ;;   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 ;;   '(org-property-value ((t (:inherit fixed-pitch))) t)
 ;;   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 ;;   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 ;;   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 ;;   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))


;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(fixed-pitch ((t (:family "Fira Code" :height 80))))
;;  '(org-block ((t (:slant italic))))
;;  '(org-code ((t (:slant italic))))
;;  '(org-table ((t (:inherit fixed-pitch))))
;;  '(variable-pitch ((t (:family "Calibri" :height 100 :slant normal)))))

 

;; * Other/automatic

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex multiple-cursors pdf-tools cmake-ide magit cmake-mode vertico treemacs smooth-scrolling smart-mode-line-powerline-theme outshine org-modern org-download orderless olivetti mindre-theme marginalia helm-org helm-bibtex ess-view-data ess-view ess-smart-equals ess-r-insert-obj ess-R-data-view embark eldoc-stan company-stan citar)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "CMU Typewriter Text" :height 90))))
 '(org-block ((t (:inherit variable-pitch :slant italic))))
 '(org-code ((t (:inherit variable-pitch :slant italic))))
 '(org-level-1 ((t (:inherit variable-pitch :height 1.5))))
 '(org-level-2 ((t (:inherit variable-pitch :height 1.3))))
 '(org-level-3 ((t (:inherit variable-pitch :slant italic :height 1.2))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(variable-pitch ((t (:family "Century Schoolbook" :height 100 :slant normal)))))
