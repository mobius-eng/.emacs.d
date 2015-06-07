;;; ob-scheme.el --- org-babel functions for Scheme

;; Copyright (C) 2010-2014 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	    Michael Gauland
;; Keywords: literate programming, reproducible research, scheme
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Now working with SBCL for both session and external evaluation.
;;
;; This certainly isn't optimally robust, but it seems to be working
;; for the basic use cases.

;;; Requirements:

;; - a working scheme implementation
;;   (e.g. guile http://www.gnu.org/software/guile/guile.html)
;;
;; - for session based evaluation geiser is required, which is available from
;;   ELPA.

;;; Code:
(require 'ob)
(require 'xscheme)
; (require 'geiser nil t)
;(defvar geiser-repl--repl)             ; Defined in geiser-repl.el
;(defvar geiser-impl--implementation)   ; Defined in geiser-impl.el
;(defvar geiser-default-implementation) ; Defined in geiser-impl.el
;(defvar geiser-active-implementations) ; Defined in geiser-impl.el


;(declare-function run-geiser "geiser-repl" (impl))
;(declare-function geiser-mode "geiser-mode" ())
;(declare-function geiser-eval-region "geiser-mode" (start end &optional and-go raw nomsg))
;(declare-function geiser-repl-exit "geiser-repl" (&optional arg))

(defvar org-babel-default-header-args:scheme '()
  "Default header arguments for scheme code blocks.")

(defun org-babel-expand-body:scheme (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
    (if (> (length vars) 0)
        (concat "(let ("
                (mapconcat
                 (lambda (var) (format "%S" (print `(,(car var) ',(cdr var)))))
                 vars "\n      ")
                ")\n" body ")")
      body)))

(defvar org-babel-scheme-repl-map (make-hash-table :test 'equal)
  "Map of scheme sessions to session names.")

(defun org-babel-scheme-cleanse-repl-map ()
  "Remove dead buffers from the REPL map."
  (maphash
   (lambda (x y)
     (when (not (buffer-name y))
       (remhash x org-babel-scheme-repl-map)))
   org-babel-scheme-repl-map))

(defun org-babel-scheme-get-session-buffer (session-name)
  "Look up the scheme buffer for a session; return nil if it doesn't exist."
  (org-babel-scheme-cleanse-repl-map) ; Prune dead sessions
  (gethash session-name org-babel-scheme-repl-map))

(defun org-babel-scheme-set-session-buffer (session-name buffer)
  "Record the scheme buffer used for a given session."
  (puthash session-name buffer org-babel-scheme-repl-map))

;;; we actually do not need this!
;; (defun org-babel-scheme-get-buffer-impl (buffer)
;;   "Returns the scheme implementation geiser associates with the buffer."
;;   (with-current-buffer (set-buffer buffer)
;;     geiser-impl--implementation))

(defun org-babel-scheme-get-repl (name)
  "Switch to a scheme REPL, creating it if it doesn't exist:"
  (let ((buffer (org-babel-scheme-get-session-buffer name)))
    (or buffer
	(progn
          (run-scheme (xscheme-default-command-line))
	  (if name
	      (progn
		(rename-buffer name t)
		(org-babel-scheme-set-session-buffer name (current-buffer))))
	  (current-buffer)))))

(defun org-babel-scheme-make-session-name (buffer name)
  "Generate a name for the session buffer.

For a named session, the buffer name will be the session name.

If the session is unnamed (nil), generate a name.

If the session is 'none', use nil for the session name, and
org-babel-scheme-execute-with-geiser will use a temporary session."
  (let ((result
	 (cond ((not name)
		(concat buffer " REPL"))
	       ((string= name "none") nil)
	       (name))))
    result))

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))


(defun org-babel-scheme-execute-with-mit-scheme (code output repl)
  "Execute code in specified REPL. If the REPL doesn't exist, create it
using the given scheme implementation.

Returns the output of executing the code if the output parameter
is true; otherwise returns the last value."
  (message "------------MIT-Scheme execution ORG----------------------")
  (message "Code: %s" code)
  (message "Output: %s" output)
  (message "Current buffer: %s" (current-buffer))
  (let ((code-to-execute (if output
                             (format "(with-output-to-string (lambda () %s))" code)
                           code))
        (result nil)
        (repl-buffer (xscheme-process-buffer)))
    (when (null repl-buffer)
      (save-current-buffer (run-scheme (xscheme-default-command-line)))
      (setq repl-buffer (xscheme-process-buffer)))
    (with-current-buffer repl-buffer
      (message "Erasing scheme buffer...")
      (delete-region (point-min) (point-max)))
    (xscheme-send-string code-to-execute)
    (xscheme-wait-for-process)
    (sleep-for 1) ; neeed to wait: buffer does not update immidiately
    (message "Scheme: done!")
    (with-current-buffer (xscheme-process-buffer)
      (let ((repl-content (buffer-substring-no-properties (point-min) (point-max))))
        (message "REPL: %s" repl-buffer)
        (message "Buffer: %s" repl-content)
        (setq result (s-trim repl-content))
        (message "Result: %s" result)
        (message "--------------------------------")))
    result))

(defun org-babel-execute:scheme (body params)
  "Execute a block of Scheme code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((source-buffer (current-buffer))
	 (source-buffer-name (replace-regexp-in-string ;; zap surrounding *
			      "^ ?\\*\\([^*]+\\)\\*" "\\1"
			      (buffer-name source-buffer))))
    (save-excursion
      (org-babel-reassemble-table
       (let* ((result-type (cdr (assoc :result-type params)))
	      (session (org-babel-scheme-make-session-name
			source-buffer-name (cdr (assoc :session params))))
	      (full-body (org-babel-expand-body:scheme body params)))
	 (org-babel-scheme-execute-with-mit-scheme
	  full-body			 ; code
	  (string= result-type "output") ; output?
	  (and (not (string= session "none")) session))) ; session
       (org-babel-pick-name (cdr (assoc :colname-names params))
			    (cdr (assoc :colnames params)))
       (org-babel-pick-name (cdr (assoc :rowname-names params))
			    (cdr (assoc :rownames params)))))))

(provide 'ob-scheme)

;;; ob-scheme.el ends here
