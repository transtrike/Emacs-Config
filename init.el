;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)
		    ))
	   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(require 'package)
;; Set repositories to fetch packages from
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Init use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(eval-when-compile (require 'use-package))

;; Set user's directory for config files
;; (setq user-emacs-directory "/opt/emacs-config")

(use-package no-littering)
;; Keep auto-save files away, so they don't trash the current directory
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  ;;(auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Windows inspired. No asking, just doing. Lmao
(add-hook 'auto-package-update-before-hook
	  (lambda () (message "I will update packages now")))

;; Remove startup message and scratch buffer
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

;; Smooth scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


;; Start every Emacs frame(instance) maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil
		    :font "Fira Code Retina"
		    :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
		    :font "Fira Code Retina"
		    :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
		    :font "Cantarell"
		    :height efs/default-variable-font-size :weight 'regular)

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package doom-modeline
:init (doom-modeline-mode 1)
:custom ((doom-modeline-height 15)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "t"  '(:ignore t :which-key "toggles")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;;(setq evil-want-C-u-scroll t) ;; Scroll with C-u
  (setq evil-want-C-i-jump nil)
  :config
  ;; Exit any state when something goes wrong with C-g
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Define Evil undo system
(use-package undo-tree
  :init
  ;;(undo-tree-mode)
  (global-undo-tree-mode))

;;(setq evil-undo-system 'undo-redo)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3) ;; How long until the tooltip shows
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")) ;; Show special keys as more then 1 char
  (setq which-key-sort-order 'which-key-key-order-alpha) ;; Order alphabetically
  (setq which-key-popup-type 'side-window) ;; Popup config
  (setq which-key-side-window-location 'bottom) ;; Appear at the bottom
  (which-key-mode))

;; Setup special keys to show as symbols
;;(add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
;;(add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
;;(add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
;;(add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))

;; Prefer UTF-8
(prefer-coding-system 'utf-8)

;; Mark matching pairs of parentheses
(show-paren-mode t)
(setq show-paren-delay 0.0)

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Ensure files end with a new line
(setq require-final-newline t)

(use-package smartparens
  :init
  (smartparens-global-mode)
  ;; Enable strict mode(don't enable it for a config file like this one)
  ;; (smartparens-strict-mode)
  )

(defconst efs/org-special-pre "^\s*#[+]")
(defun efs/org-2every-src-block (fn)
  "Visit every Source-Block and evaluate `FN'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward (concat help/org-special-pre "BEGIN_SRC") nil t)
	(let ((element (org-element-at-point)))
	  (when (eq (org-element-type element) 'src-block)
	    (funcall fn element)))))
    (save-buffer)))

					;(define-key org-mode-map (kbd "s-]") (lambda () (interactive)
					;  (efs/org-2every-src-block
					;    'org-babel-remove-result)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("cs" . "src csharp"))

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-edit-src-content-indentation 0)

;;
(org-babel-do-load-languages
 'org-babel-load-lanaguages
 '(
   (emacs-lisp . t)
   (python . t)
   ))

(setq org-confirm-babel-evaluate nil)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :init
  (global-set-key (kbd "M-x") 'counsel-M-x) ;; Enchanced M-x
  (global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; Enchanced Find File

)

(use-package swiper
  :after counsel
  :init
  (evil-global-set-key 'normal "/" 'swiper) ;; Bind "/", in normal mode, to swiper
  :config
  (add-to-list 'ivy-height-alist '(swiper . 5)) ;; Make swiper's hight to 5
  )

(use-package company
  :init
  (evil-global-set-key 'insert (kbd "M-.") 'company-complete)
  :config
  (add-hook 'after-init-hook 'global-comapny-mode)
  (company-mode))

(defun efs/csharp-mode-setup ()
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t))

(add-hook 'csharp-mode-hook 'efs/csharp-mode-setup t)
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit company counsel ivy smartparens which-key undo-tree evil-collection evil general doom-modeline all-the-icons doom-themes auto-package-update no-littering use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
