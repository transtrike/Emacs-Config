;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1024 1024))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)
		    ))
	   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq straight-fix-flycheck t)
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy.
(setq use-package-always-defer t)

(straight-register-package 'org)
(straight-register-package 'org-contrib)

(eval-when-compile (require 'use-package))

(use-package bind-key
	     :demand t)

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

(scroll-bar-mode -1)        ; Disabled visible scrollbar
(tool-bar-mode -1)          ; Disabled the toolbar
(tooltip-mode -1)           ; Disabled tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disabled the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

;; Smooth scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Debug errors, if you mess up the config
(setq debug-on-error t)

;; Start every Emacs frame(instance) maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disabled line numbers for some modes
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

(bind-key "<escape>" 'keyboard-escape-quit)

(use-package general
  :demand t
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "t"  '(:ignore t :which-key "toggles")))

(use-package evil
  :demand t
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
  :demand t
  :init
  ;;(undo-tree-mode)
  (global-undo-tree-mode))

;;(setq evil-undo-system 'undo-redo)

(use-package which-key
  :demand t
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

;; (defun copy-to-clipboard ()
;;   (interactive)
;;   (if (display-graphic-p)
;; 	(progn
;; 	  (message "Yanked region to x-clipboard!")
;; 	  (call-interactively 'clipboard-kill-ring-save)
;; 	  )
;;     (if (region-active-p)
;; 	  (progn
;; 	    (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
;; 	    (message "Yanked region to clipboard!")
;; 	    (deactivate-mark))
;; 	(message "No region active; can't yank to clipboard!")))
;;   )

;; (evil-define-command paste-from-clipboard()
;;   (if (display-graphic-p)
;; 	(progn
;; 	  (clipboard-yank)
;; 	  (message "graphics active")
;; 	  )
;;     (insert (shell-command-to-string "xsel -o -b"))
;;     )
;;   )

(use-package smartparens
  :demand t
  :init
  (smartparens-global-mode)
  ;; Enable strict mode(don't enable it for a config file like this one)
  ;; (smartparens-strict-mode)
  )

(use-package evil-nerd-commenter
  :demand t
  :bind
  (("C-/" . evilnc-comment-or-uncomment-lines)
  ("M-;" . evilnc-comment-or-uncomment-lines)))

(setq user-init-file (expand-file-name "config.org" user-emacs-directory))
;;(bind-key ("C-c c" . (lambda() (interactive)(find-file "~/.emacs.d/config.org"))))

(defconst efs/org-special-pre "^\s*#[+]")
(defun efs/org-2every-src-block (fn)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward (concat help/org-special-pre "BEGIN_SRC") nil t)
	(let ((element (org-element-at-point)))
	  (when (eq (org-element-type element) 'src-block)
	    (funcall fn element)))))
    (save-buffer)))

;;(define-key org-mode-map (kbd "C-c C-v y") (lambda () (interactive)
;;					   (efs/org-2every-src-block
;;					    'org-babel-remove-result)))

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

(use-package org-auto-tangle
  :demand t
  :hook (org-mode . org-auto-tangle-mode))

(use-package magit
  :demand t)

(use-package ivy
  :demand t
  :config
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :bind (
	 ("M-x" . counsel-M-x) ;; Enchanced M-x
	 ("C-x C-f" . counsel-find-file) ;; Enchanced Find File
	 )
  )

(use-package swiper
  :after counsel
  :bind (:map evil-normal-state-map
	      ("/" . swiper)) ;; Bind "/", in normal mode, to swiper
  :config
  (add-to-list 'ivy-height-alist '(swiper . 5)) ;; Make swiper's hight to 5
  )

(use-package company
  :demand t
  :bind
  (:map evil-normal-state-map
	("M-." . company-complete))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (company-mode))

(use-package flycheck
  :demand t
  :init
  (global-flycheck-mode))

(use-package selectrum
  :disabled t
  :init
  (selectrum-mode +1)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1)
  :bind(
  ("C-x C-z") . selectrum-repeat) ;; Reperat last command
  )

(use-package helpful
  :bind (
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h C" . helpful-command))
)

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
	'("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar"))
  (setq projectile-globally-ignored-directories
	'(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  )

(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Default shell of choise: eshell
;; (use-package eshell)

;; (bind-key* "M-1" 'eshell evil-normal-state-map)

(use-package csharp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

  (defun my-csharp-mode-hook-config ()
    ;; enable the stuff you want for C# here
    (electric-pair-mode 1)       ;; Emacs 24
    (electric-pair-local-mode 1) ;; Emacs 25
    (add-hook 'csharp-mode-hook 'flycheck-mode)
    (add-hook 'csharp-mode-map 'yas-minor-mode)
    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
    (add-to-list 'company-backends 'company-omnisharp)

    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t))

  (use-package omnisharp
    :straight `(omnisharp
		   :type git
		   :host github
		   :repo "OmniSharp/omnisharp-emacs"
		   :after company)
    :bind (:map csharp-mode-map
	    ("M-3" . omnisharp-solution-errors)
	    ("." . omnisharp-auto-complete)
	    ("<C-SPC>" . omnisharp-auto-complete)
	    ("<f12>" . omnisharp-go-to-definition)
	    ("g u" . omnisharp-find-usages)
	    ("g I" . omnisharp-find-implementations)
	    ("g o" . omnisharp-go-to-definition-other-window)
	    ("g r" . omnisharp-run-code-action-refactoring)
	    ("g f" . omnisharp-fix-code-issue-at-point)
	    ("g F" . omnisharp-fix-usings)
	    ("g R" . omnisharp-rename)
	    (", i" . omnisharp-current-type-information)
	    (", I" . omnisharp-current-type-documentation)
	    ("." . omnisharp-add-dot-and-auto-complete)
	    (", n t" . omnisharp-navigate-to-current-file-member)
	    (", n s" . omnisharp-navigate-to-solution-member)
	    (", n f" . omnisharp-navigate-to-solution-file-then-file-member)
	    (", n F" . omnisharp-navigate-to-solution-file)
	    (", n r" . omnisharp-navigate-to-region)))

  (setq omnisharp-server-executable-path (expand-file-name "config/omnisharp" user-emacs-directory))
  (setq omnisharp-auto-complete-want-documentation nil) ;; If docs fetching is a problem, comment this
  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook-config)
