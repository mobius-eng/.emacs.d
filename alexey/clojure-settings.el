;; -*- Mode: Emacs-Lisp; -*-
(require 'clojure-mode)
(require 'cider)
(require 'cider-eval-sexp-fu)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq nrepl-log-messages nil)
(setq cider-popup-stacktraces nil)
(setq cider-repl-print-length 20)
(setq cider-repl-result-prefix ";;=> ")
(setq cider-interactive-eval-result-prefix ";; => ")
(setq nrepl-hide-special-buffers t)
(setq cider-stacktrace-fill-column 80)
(setq cider-ovelays-use-font-lock t)

(add-hook 'cider-mode-hook
          (lambda ()
            (eldoc-mode t)
            (company-mode t)))



(add-hook 'cider-repl-mode-hook '(lambda ()
                                   (rainbow-delimiters-mode t)
                                   (smartparens-strict-mode t)
                                   (company-mode t)))

(add-hook 'clojure-mode-hook '(lambda ()
                                (smartparens-strict-mode t)
                                (rainbow-delimiters-mode t)))

(define-key cider-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; ;; Cycle between () {} []

(defun live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun live-cycle-clj-coll ()
  "convert the coll at (point) from (x) -> {x} -> [x] -> (x) recur"
  (interactive)
  (let* ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "(" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal "{" (buffer-substring-no-properties (point) (+ 1 (point)))))
                (not (equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal "(" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "{" (substring (live-delete-and-extract-sexp) 1 -1) "}"))
     ((equal "{" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "[" (substring (live-delete-and-extract-sexp) 1 -1) "]"))
     ((equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "(" (substring (live-delete-and-extract-sexp) 1 -1) ")"))
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake.")))
    (goto-char original-point)))

(define-key clojure-mode-map (kbd "C-~") 'live-cycle-clj-coll)

(provide 'clojure-settings)

