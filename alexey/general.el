;; * General settings for Emacs
;; Text auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq fill-column 80)
;; Need to have it early on to function properly
(defvar outline-minor-mode-prefix "\M-#")
;; Something to do with autofill...
(add-hook 'text-mode-hook 'text-mode-hook-identify)
;; Disable toolbar and menu-bar
;; On macs: keep menu bar
(setq is-mac (equal system-type 'darwin))
(tool-bar-mode 0)
(if is-mac
    (menu-bar-mode 1)
  (menu-bar-mode 0))
;; Disable visual feedback on selection
(transient-mark-mode 0)
;; improve the title
(setq frame-title-format "%b - emacs")
;; show matching parentheses
(show-paren-mode t)
;; font setting
;; (set-default-font "DejaVu Sans Mono 9")
(set-default-font "InputMonoNarrow 12")
;; Some fonts might require extra spacing, uncomment if necessary
;; (setq-default line-spacing 3)
;; On some platforms it is not home...
(setq default-directory "~/")
;; no bells
(setq ring-bell-function 'ignore)
;; Comments
(global-set-key (kbd  "C-x C-;") 'comment-region)
(global-set-key (kbd  "C-x M-;") 'uncomment-region)
;; Spaces only (no tabs)
(setq-default indent-tabs-mode nil)
;; ALways show column number
(setq-default column-number-mode t)
;;; Packages:
(require 'package)
;; start with GNU: remove other entries if set by default
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
;; MELPA
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; ORG
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))
;; (perhabs incomplete) list of packages
(setq package-list
      '(
        ; starter-kit
        ; starter-kit-bindings
        ; starter-kit-eshell
        ; starter-kit-lisp
        auctex
        cdlatex
        magit
        auto-complete
        auto-complete-pcmp
        company
        ctable
        elisp-slime-nav
        htmlize
        ido-ubiquitous
        org
        org-ac
        org-bullets
        org-mac-link
        org-magit
        org-octopress
        org-plus-contrib
        org2blog
        orglue
        outorg
        outshine
        ox-impress-js
        ox-reveal
        ; paredit
        rainbow-delimiters
        slime
        scheme-complete
        racket-mode
        smex
        smyx-theme
        w3m
        smooth-scrolling
        smart-mode-line
        smart-mode-line-powerline-theme
        material-theme))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
;; IDO
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; Paredit settings:
(require 'smartparens)
(require 'smartparens-config)
;; sometimes these keys do not work out of the box
;; (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
;; (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
;; (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
;; w3m browsing in Emacs
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)
;; Emacs lisp: usual setup works, just add rainbow-delimiters
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;; ???
;; (add-hook 'find-file-hooks
;; 	  '(lambda ()
;; 	     (setq mode-line-buffer-identification 'buffer-file-truename)))
;; Setting up outorg/outshine
;; (require 'outorg)
;; (require 'outshine)

;; let's try this for Haskell:
;; (setq outshine-preserve-delimiter-whitespace t)

(add-hook 'outline-minor-mode-hook (lambda ()
                                     (require 'outshine)
                                     (outshine-hook-function)
                                     (setq outshine-use-speed-commands t)
                                     ))
(add-hook 'prog-mode-hook 'outline-minor-mode)

;; smart-mode-line (powerline)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)
;; (sml/customize )
;; remove magit warning
(setq magit-last-seen-setup-instructions "1.4.0")

;; Mac specific
(when is-mac
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
(tool-bar-mode 0)
(if is-mac
    (menu-bar-mode 1)
  (menu-bar-mode 0))
;; Enable server-mode
(require 'server)
(unless (server-running-p)
  (server-mode))
