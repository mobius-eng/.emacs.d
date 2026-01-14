;; * Basic setup: MELPA & use-package
(require 'package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/") t)


(package-initialize)
;; uncomment if Emacs complains
(setq package-check-signature nil)


;; manage packages via use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; manage package installation via use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)



;; * Main vanila Emacs settings

;; ** Prevent Warnings to pop up
(setq warning-minimum-level :error)

;; ** Stop writing custom-set-variables to this file
(setq custom-file (expand-file-name "~/.emacs.d/custom-vars.el"))

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
     (attachments-dir (file-name-as-directory (concat the-dir ".media")))
     (png-file-name (format-time-string "%a%d%b%Y_%H%M%S.png"))
     (full-png-path (concat attachments-dir png-file-name))
     (rel-png-path (concat (file-name-as-directory ".media") png-file-name))
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


;; ** Packages config
;; ** Themes

(use-package smart-mode-line
  :config
  (use-package smart-mode-line-powerline-theme))

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
  (setq inferior-ess-r-program "R")
  ;; (setq inferior-ess-r-program "/C/Program\\ Files/R/R-4.4.1/bin/R.exe")
  ;; (setq inferior-ess-r-program "C:\\Program Files\\R\\R-4.4.1\\bin\\R.exe")
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

;; ** BibTeX and BibLaTeX management
(use-package ebib
  :custom
  (ebib-file-search-dirs '("~/org/04-lt/01-bib/"))
  (ebib-file-associations '(("pdf" . "xdg-open")))
  )
;; TODO: configure

;; ** citar -- insert citation
(use-package citar
  :custom
  (citar-bibliography '("~/org/04-lt/01-bib/01-main.bib"))
  (citar-notes-paths '("~/org/04-lt/03-ann/"))
  ;; I don't think this is necessary...
  ;; :hook
  ;; (org-mode . citar-capf-setup)
  )

;; ** citeproc -- process CSL citations
;; I suspect org uses it internally
(use-package citeproc)
  
;; ** helm (better M-x)
;; While other alternatives exist, helm seems to be the most advanced
;; (although maybe buggy) and other packages use helm
;; so, let's stick with it
(use-package helm
  :bind ("M-x" . helm-M-x)
  :config
  (use-package helm-bibtex))

;; ** olivetti -- distraction free writing mode
;; the main use is within org
(use-package olivetti)

;; ** vertico -- better UI for completion
(use-package vertico
  :init
  (vertico-mode))

;; ** orderless -- completion style, used with vertico
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring


;; ** embark -- choose a command to run based on what is near point
;; other package included that work better with embark, e.g. marginalia

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))


(use-package embark
  :after vertico
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ** treemacs
(use-package treemacs
  :bind ("C-c t" . 'treemacs-select-window)
  ;; :config
  )

;; ** multiple-cursors
(use-package multiple-cursors
  :bind ("C-S-c C-S-c" . mc/edit-lines)
  )

;; ** org


;; for non-breakable space in org
(defun insert-nbsp (&rest args)
  (interactive)
  (insert "\\nbsp{}"))

;; attempt to fix italic font appearance
(defun my/org-remove-italic-underline ()
  (face-remap-reset-base 'org-italic)
  ;; (setq-local face-remapping-alist
  ;;             (assq-delete-all 'org-italic face-remapping-alist))
  ;; (face-remap-add-relative 'org-italic '(:inherit variable-pitch :slant italic :underline nil))
  (face-remap-add-relative
   'italic
   '(:slant italic :underline nil))
  )



(use-package org
  :config
  (add-hook 'org-mode-hook
            #'(lambda ()
                (turn-on-font-lock)
                (setq org-use-speed-commands t)
                (setq org-src-preserve-identation t)
                (visual-line-mode 1)
                (org-display-inline-images)
                (org-latex-preview 1)
                (setq org-hide-emphasis-markers t)
                (org-modern-mode 1)
                (setq fill-column 80)
                (olivetti-mode 1)
                (variable-pitch-mode 1)
                (my/org-remove-italic-underline)
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
  ;; new functionality for citations
  (use-package helm-org)
  (require 'oc)
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)
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
  (setq lit-path (car citar-bibliography))
  (tempo-define-template
   "org-header"
   ;; note: figure height and width set for PNG, for SVG use 3 and 5
   '("#+TITLE: " n p
     "#+STARTUP: latexpreview" n
     "#+SETUPFILE: ~/.emacs.d/latex_header.org" n
     "#+PROPERTY: header-args :colnames yes :height 300 :width 500 :session *R*" n
     "#+LATEX_CLASS: article" n
     "#+AUTHOR: Alexey V. Cherkaev" n
     (format "#+BIBLIOGRAPHY: %s" lit-path) n
     ;; (format
     ;;  "#+CITE_EXPORT: csl %s/ieee.csl"
     ;;  lit-path) n
     "#+CITE_EXPORT: csl ~/.emacs.d/ieee.csl" n
     )
   "<t"
   "Insert a typical header"
   'org-tempo-tags)
  (tempo-define-template
   "r-plot"
   '("#+begin_src R :results graphics file :file .media/file.png :exports both" n
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
  (setq org-preview-latex-image-directory ".media/")
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
        '("latexmk -shell-escape -f -pdf -%latex -interaction=nonstopmode -output-directory=.build/ %f"
          "mkdir -p .pdf"
          "cp .build/%b.pdf .pdf/%b.pdf"
          "mv %b.tex .build/%b.tex"))
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

;; ** Fix for org-ctags
;;; work-around  for org-ctags obnoxious behavior
(with-eval-after-load 'org-ctags (setq org-open-link-functions nil))

;; ** pdf-tools -- better PDF viewer

(use-package pdf-tools
  ;; :defer t
  :demand t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  :bind (:map pdf-view-mode-map
              ("\\" . hydra-pdftools/body)
              ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
              ("g"  . pdf-view-first-page)
              ("G"  . pdf-view-last-page)
              ("l"  . image-forward-hscroll)
              ("h"  . image-backward-hscroll)
              ("j"  . pdf-view-next-page)
              ("k"  . pdf-view-previous-page)
              ("e"  . pdf-view-goto-page)
              ("u"  . pdf-view-revert-buffer)
              ("al" . pdf-annot-list-annotations)
              ("ad" . pdf-annot-delete)
              ("aa" . pdf-annot-attachment-dired)
              ("am" . pdf-annot-add-markup-annotation)
              ("at" . pdf-annot-add-text-annotation)
              ("y"  . pdf-view-kill-ring-save)
              ("i"  . pdf-misc-display-metadata)
              ("s"  . pdf-occur)
              ("b"  . pdf-view-set-slice-from-bounding-box)
              ("r"  . pdf-view-reset-slice)))

;; unavailable?
;; (use-package org-pdfview
;;   :config 
;;   (add-to-list 'org-file-apps
;;                '("\\.pdf\\'" . (lambda (file link)
;;                                  (org-pdfview-open link)))))


;; ** auctex
(use-package auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil))


;; * Custom theme settings (for variable pitch font)
;; "Calibri"
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "TeX Gyre Schola" :height 100 :slant normal))))
 ;; '(variable-pitch ((t (:family "DejaVu Sans" :height 100 :slant normal))))
 ;; '(fixed-pitch ((t (:family "TeX Gyre Cursor" :height 90))))
 '(fixed-pitch ((t (:family "Fira Code" :height 90))))
 ;; '(org-table ((t (:inherit fixed-pitch))))
 '(org-block ((t (:inherit variable-pitch :slant italic))))
 ;; '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit variable-pitch :slant italic))))
 ;; '(org-code ((t (:inherit fixed-pitch))))
 '(org-level-1 ((t (:inherit variable-pitch :height 1.5))))
 '(org-level-2 ((t (:inherit variable-pitch :height 1.3))))
 '(org-level-3 ((t (:inherit variable-pitch :slant italic :height 1.2))))
 ;; org-verbatim
 ;; now, will it create a problem everywhere else?
 ;; '(italic ((t (:inherit variable-pitch :slant italic))))
 
 )
