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



;; Custom links in org
(defcustom my-org-path nil
  "List of paths to try with `my-org-open-file'.
If the LINK argument for `my-org-open-file' is a relative path
and a file with that path does not exist
try successively each of the directory entries of this list
as `default-directory' for expanding the path.
The directory entries can be given relative or absolute."
  :type '(repeat directory)
  :group 'org-link-follow)

(defun my-org-open-file (link)
  "Open LINK with `org-open-file'.
If LINK is given as relative file path also try to find file
with prefixes from `my-org-path'."
  (let* ((el (car (org-element-parse-secondary-string (format "[[file:%s]]" link) '(link))))
     (option (org-element-property :search-option el))
     (path (org-element-property :path el))
     (app (org-element-property :application el))
     (org-path (org-property-or-variable-value 'my-org-path t)))
    (when (stringp org-path)
      (setq org-path (list org-path)))
    (setq path (substitute-in-file-name path))
    (unless (file-name-absolute-p path)
      (cl-dolist (path-prefix org-path)
    (let ((try-path (expand-file-name path path-prefix)))
      (when (file-exists-p try-path)
        (setq path try-path)
        (cl-return)))))
    (apply #'org-open-file
       path
       (cond ((equal app "emacs") 'emacs)
         ((equal app "sys") 'system))
       (cond ((not option) nil)
         ((string-match-p "\\`[0-9]+\\'" option)
          (list (string-to-number option)))
         (t (list nil option))))))





