;; -*- Mode: emacs-lisp; -*-

;; * General Settings

;; ** Identify operating system
(setq mac-p (equal system-type 'darwin))
(setq win-p (eql system-type 'windows-nt))
(setq lin-p (eql system-type 'gnu/linux))

;; ** Setup packages
(require 'package)

;; *** Start with GNU: remove other entries if set by default
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))

;; *** MELPA
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; *** ORG
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; *** Fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; *** Pakage list
(setq package-list
      '(magit                    ; git interface
        auto-complete            ; auto complete (SLIME)
        auto-complete-pcmp       ; programmable completion
        company                  ; another auto complete (CIDER)
        ctable                   ; Table component for Emacs Lisp ?
        elisp-slime-nav          ;  M-. M-, nav and C-c C-d d to
                                 ;  describe 
        htmlize                  ; Pretty HTML export (ORG)
        idle-highlight-mode      ; highlight words in programming
                                 ; buffers
        ido-completing-read+     ; extra IDO
        ido-yes-or-no            ; y for yes and n for no
        ido-ubiquitous           ; IDO everywhere
        auctex                   ; LaTeX
        cdlatex                  ; Some clever LaTeX functionality
        ebib                     ; Emacs BibTeX
        org                      ; ORG mode
        org-ac                   ; auto-complete for ORG
        org-bullets              ; UTF8 bullets in Org captions
        org-plus-contrib         ; extra functionality of ORG (babel)
        org2blog                 ; export ORG to blog
        orglue                   ; ???
        outorg                   ; ORG-like lit programming
        outshine                 ; ORG-like lit programming
        ox-impress-js            ; export IMPRESS presentations
        ox-reveal                ; export REVEAL presentations
        rainbow-delimiters       ; highlight parenthesis
        ac-slime                 ; auto-complete for slime
        slime                    ; SLIME (Common Lisp IDE)
        ;; scheme-complete          ; Extra for Scheme (MIT-Scheme)
        smex                     ; IDO for M-x command
        smyx-theme               ; nice dark theme
        smooth-scrolling         ; smooth scrolling
        smart-mode-line          ; better mode line
        smart-mode-line-powerline-theme
                                        ; better look for mode line
        material-theme           ; nice light and dark themes
        solarized-theme          ; yet another theme
        cider                    ; Clojure support
        cider-eval-sexp-fu       ; Highlight evaled sexp (use eval-sexp-fu without Clojure)
        clojure-mode
        clojure-mode-extra-font-locking
        smartparens             ; Help with many parens
        ))

;; *** Mac-specific package
(when mac-p
  (add-to-list 'package-list 'org-mac-link)
  (add-to-list 'package-list 'exec-path-from-shell))

;; *** W3M web-browser only for non-windows OSs
(unless win-p
  (add-to-list 'package-list 'w3m))

;; *** install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ** General settings for Emacs

;; *** Text auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq fill-column 80)
;; Something to do with autofill...
(add-hook 'text-mode-hook 'text-mode-hook-identify)

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

;; *** Font setting
(set-frame-font
 (cond (win-p "InputMonoCondensed 10")
       (lin-p "InputMonoCondensed 9")
       (mac-p "InputMonoNarrow 12")))
;; (set-frame-font "Inconsolata 9")
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; Some fonts might require extra spacing, uncomment if necessary
;; (setq-default line-spacing 3)

;; *** On some platforms it is not home...
;; TODO: check if it works on Windows
(setq default-directory (expand-file-name "~/"))

;; *** No bells
(setq ring-bell-function 'ignore)

;; *** Comments
(global-set-key (kbd  "C-x C-;") 'comment-region)
(global-set-key (kbd  "C-x M-;") 'uncomment-region)

;; *** Spaces only (no tabs)
(setq-default indent-tabs-mode nil)

;; *** Always show column number
(setq-default column-number-mode t)

;; ** Platform specific

;; *** Mac specific
(when mac-p
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (menu-bar-mode 1))

;; *** Win specific
;; Might need to add more sofisticated search for MSYS/MinGW
(when win-p
  (if (file-directory-p "C:/msys64/")
      (setq shell-file-name "C:/msys64/msys2_shell.bat")))

;; ** Parenthesis
(show-paren-mode t)
(require 'smartparens-config)
(sp-use-paredit-bindings)
(sp-pair "(" ")" :wrap "M-(")
(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-S-<left>") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-}") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-{") 'sp-backward-barf-sexp)

(sp-pair "(" ")" :wrap "M-(")
(sp-pair "[" "]" :wrap "M-[")
(sp-pair "{" "}" :wrap "M-{")

;; ** IDO
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-yes-or-no-mode t)
(ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order
      '(".lisp" ".el" ".org" ".tex"  ".md" ".txt"))

;; *** IDO on M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; ** Smooth scrolling
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;; ** w3m browsing in Emacs
(setq browse-url-browser-function 'w3m-browse-url)
;; (setq browse-url-browser-function 'eww-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
;; (setq w3m-use-cookies t)

;; ** Emacs lisp
;; *** ELDOC
;; (setq eldoc-documentation-function
;;       (lambda ()
;;         (when (or (eql last-command 'new-line) (eql last-command-event 32))
;;           (let (eldoc-documentation-function)
;;             (eldoc-print-current-symbol-info)))))
(setq eldoc-idle-delay 0)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (rainbow-delimiters-mode t)
            (eldoc-mode t)
            (auto-complete-mode t)
            (smartparens-strict-mode t)))
 
;; *** Setting up outorg/outshine
(defvar outline-minor-mode-prefix "\M-#")
(require 'outorg)
(require 'outshine)
;; let's try this for Haskell:
;; (setq outshine-preserve-delimiter-whitespace t)

(add-hook 'outline-minor-mode-hook (lambda ()
                                     (require 'outshine)
                                     (outshine-hook-function)
                                     (setq outshine-use-speed-commands t)))
(add-hook 'prog-mode-hook 'outline-minor-mode)

;; ** Load theme
(load-theme 'material t)

;; ** Smart mode line
; (require 'rich-minority)
(setq sml/no-confirm-load-theme t)
(sml/setup)
;; (sml/apply-theme 'dark)
;; (sml/customize )
;; remove magit warning

;; ** Magit settings
(setq magit-last-seen-setup-instructions "1.4.0")

;; ** Server mode
(require 'server)
(unless (server-running-p)
  (server-mode))
