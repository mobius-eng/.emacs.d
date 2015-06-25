;;; Setting for ORG mode
(require 'org)
;; (require 'org-latex) -- does not exist?
(require 'ox-latex)
;; remove ' from forbidden border symbols
(setf (caddr org-emphasis-regexp-components)
      (delete ?\' (caddr org-emphasis-regexp-components)))
;; we must reload org after changin org-emphasis-regexp-components
(org-reload)

(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when
             ; global-font-lock-mode is on

(add-hook 'org-mode-hook (lambda ()
                           (setq org-use-speed-commands t)
                           (setq org-src-preserve-identation t)))
(setq org-log-done 'time)
;; (setq org-log-done 'note)
;; recommended to have access to ORG from anywhere
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Evaluate common lisp code
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t) (emacs-lisp . t) (ditaa . t) (scheme . t) (haskell . t) (clojure . t)
   (python . t)))

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;; (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")


(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      '(("mobius"
         :url "https://mobiusengineering.wordpress.com/xmlrpc.php"
         :username "mobiusphoto"
         :default-title "Post"
         :default-categories ("lisp" "programming")
         :tags-as-categories nil)))

;; (defun url-cookie-expired-p (cookie)
;;   "Return non-nil if COOKIE is expired."
;;   (let ((exp (url-cookie-expires cookie)))
;;     (and (> (length exp) 0)
;;      (condition-case ()
;;          (> (float-time) (float-time (date-to-time exp)))
;;        (error nil)))))


(setq org2blog/wp-use-sourcecode-shortcode nil)
;; removed light="true"
(setq org2blog/wp-sourcecode-default-params nil)
;; target language needs to be in here
(setq org2blog/wp-sourcecode-langs
      '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
        "erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
        "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
        "vb" "xml"
        "sh" "emacs-lisp" "lisp" "lua"))
 
;; this will use emacs syntax higlighting in your #+BEGIN_SRC
;; <language> <your-code> #+END_SRC code blocks.
(setq org-src-fontify-natively t)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(setq alexey-org-latex-preambule
      (get-string-from-file "~/.emacs.d/alexey/org-latex.tex"))

;;; Adding LaTeX memoir class support
(setq org-latex-classes nil)
(add-to-list 'org-latex-classes
             (list "memoir"
                   alexey-org-latex-preambule
                   '("\\chapter{%s}" . "\\chapter*{%s}")
                   '("\\section{%s}" . "\\section*{%s}")
                   '("\\subsection{%s}" . "\\subsection*{%s}")
                   '("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(setq org-latex-default-class "memoir")

;; mac open pdfs in ebib
(when is-mac
 (setq ebib-file-associations '(("pdf" . "open") ("ps" . "open"))))

(defun my-org-mode-setup ()
 (when (and (buffer-file-name)
            (file-exists-p (buffer-file-name)))
  (load-library "reftex")
  (and (buffer-file-name)
        (file-exists-p (buffer-file-name))
        (reftex-parse-all))
  (reftex-set-cite-format
   '((?p . "[[citep:%l]]")
     (?t . "[[citet:%l]]")
     (?n . "[[note::%l]]")
     )))
 (define-key org-mode-map "\C-c\C-g" 'reftex-citation)
 (define-key org-mode-map "\C-c(" 'org-mode-reftex-search))

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[note:%s]]" (first (reftex-citation t)))))


(add-hook 'org-mode-hook 'my-org-mode-setup)

(setq ebib-preload-bib-files '("~/Documents/ebib-library.bib"))

(org-add-link-type 
 "citep" (lambda (path) (ebib (car ebib-preload-bib-files) path))
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citep:" desc)))
         (format "\\citep{%s}" path)
       (format "\\citep[%s]{%s}" desc path))))))

(org-add-link-type 
 "citet" (lambda (path) (ebib (car ebib-preload-bib-files) path))
 (lambda (path desc format)
   (cond
      ((eq format 'latex)
       (if (or (not desc) (equal 0 (search "citet:" desc)))
           (format "\\citet{%s}" path)
         (format "\\citet[%s]{%s}" desc path))))))

(org-add-link-type 
 "citealt" (lambda (path) (ebib (car ebib-preload-bib-files) path))
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citealt:" desc)))
         (format "\\citealt{%s}" path)
       (format "\\citealt[%s]{%s}" desc path))))))

(org-add-link-type 
 "citealp" (lambda (path) (ebib (car ebib-preload-bib-files) path))
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citealp:" desc)))
         (format "\\citealp{%s}" path)
       (format "\\citealp[%s]{%s}" desc path))))))

(org-add-link-type 
 "citealt*" (lambda (path) (ebib (car ebib-preload-bib-files) path))
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citealt*:" desc)))
         (format "\\citealt*{%s}" path)
       (format "\\citealt*[%s]{%s}" desc path))))))

(org-add-link-type 
 "citealp*" (lambda (path) (ebib (car ebib-preload-bib-files) path))
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citealp*:" desc)))
         (format "\\citealp*{%s}" path)
       (format "\\citealp*[%s]{%s}" desc path))))))

(org-add-link-type 
 "citep*" (lambda (path) (ebib (car ebib-preload-bib-files) path))
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citep*:" desc)))
         (format "\\citep*{%s}" path)
       (format "\\citep*[%s]{%s}" desc path))))))

(org-add-link-type 
 "citet*" (lambda (path) (ebib (car ebib-preload-bib-files) path))
   (lambda (path desc format)
     (cond
      ((eq format 'latex)
       (if (or (not desc) (equal 0 (search "citet*:" desc)))
           (format "\\citet*{%s}" path)
             (format "\\citet*[%s]{%s}" desc path))))))

;; TODO: insert bibtex support
;; but this might need to be customised on per project basis
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
        "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))

;; Adapted from
;; http://emacs.stackexchange.com/questions/3374/set-the-background-of-org-exported-code-blocks-according-to-theme
(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the background
of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s; overflow: scroll;} body { max-width: 600pt; margin: auto} </style>\n"
                my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)


;;; Automatically change parent note to DONE

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;;; Toggle UTF8 symbols in ORG
(setq org-pretty-entities t)
;;; CDLaTeX-light for ORG
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
;;; Better source code appearence in LaTeX
(setq org-latex-listings 'minted)
;; Common Lisp support for minted
(add-to-list 'org-latex-minted-langs '(lisp "common-lisp"))
(add-to-list 'org-latex-minted-langs '(emacs-lisp "common-lisp"))
;; Use imagemagic to create formulas (since we use PDF output)
(setq org-latex-create-formula-image-program 'imagemagick)

;;;; Org Babel for Clojure: does not quite work: cannot find CIDER
;;;; connection
;; (require 'ob)
;; (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
;; (defvar org-babel-default-header-args:clojure 
;;   '((:results . "silent")))
;; (defun org-babel-execute:clojure (body params)
;;   "Execute a block of Clojure code with Babel."
;;   (nrepl-interactive-eval body))
;; (add-hook 'org-src-mode-hook
;;           '(lambda ()
;;              (set (make-local-variable 'nrepl-buffer-ns) 
;;                   (with-current-buffer 
;;                       (overlay-buffer org-edit-src-overlay)
;;                     nrepl-buffer-ns))))
;; (provide 'ob-clojure)
