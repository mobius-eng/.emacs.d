;; -*- Mode: Emacs-Lisp; -*-
(require 'clojure-mode)
(require 'cider)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces nil)
(setq cider-repl-print-length 20)
(setq cider-repl-result-prefix ";;=> ")
(setq cider-interactive-eval-result-prefix ";; => ")
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook '(lambda ()
                                   (setq rainbow-delimiters-mode t)
                                   (setq paredit-mode t)))
(add-hook 'clojure-mode-hook '(lambda ()
                                (setq paredit-mode t)
                                (setq rainbow-delimiters-mode t)
                                (setq cider-turn-on-eldoc-mode t)))


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

