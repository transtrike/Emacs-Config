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

(add-hook 'emacs-startup-hook 'efs/display-startup-time)

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

(straight-use-package 'use-package)
(setq straight-always-use-package-ensure t)

(straight-register-package 'org)
(straight-register-package 'org-contrib)

;;(eval-when-compile (require 'use-package))

(use-package bind-key
  :demand t)

;; Set user's directory for config files
;; (setq user-emacs-directory "/opt/emacs-config")

(use-package no-littering
  :demand t)
;; Keep auto-save files away, so they don't trash the current directory
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

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

(defun efs/set-font-faces ()
  (message "Setting fonts!")
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
		      :height efs/default-variable-font-size :weight 'regular))

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda(frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame (efs/set-font-faces))))
  (efs/set-font-faces))

(setq-default ibuffer-saved-filter-groups
	      `(("Default"
		 ("Temporary" (name . "\*.*\*"))
		 ("Magit" (name . "^magit")))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "Default")))

(setq ibuffer-show-empty-filter-groups nil
      ibuffer-expert t)

(defun fw/vsplit-last-buffer ()
  "Split the selected window into two vertical windows."
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (switch-to-next-buffer))

(defun fw/hsplit-last-buffer ()
  "Split the selected window into two horizontal windows."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-next-buffer))

(defun fw/kill-this-buffer ()
  "Kills this buffer and removes this window when split."
  (interactive)
  (kill-this-buffer)
  (when (> (length (window-list)) 1)
    (delete-window)))

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  ;;(doom-themes-neotree-config) ;; Load a custom theme for neotree
  (doom-themes-org-config)) ;; Improve org mode's native fontification

(use-package all-the-icons
  :after doom-modeline)

(use-package doom-modeline
  :demand t
  :config (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-lsp t))

;; ESC quit prompts(like in VIM)
(bind-key "<escape>" 'keyboard-escape-quit)

;; Save ma'h FILEEEEE
(bind-key* "C-s" 'save-buffer)

(global-auto-revert-mode t)

(use-package general
  :demand t
  :after evil
  :config
  ;; Define leaders keys to use later on
  (general-create-definer efs/buffer-keys
    :keymaps '(normal insert visual emacs)
    :prefix "C-b")

  ;;(efs/leader-keys
  ;; "t"  '(:ignore t :which-key "toggles"))
  )

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
;; (use-package undo-tree
;;   :demand t
;;   :after evil
;;   :init
;;   ;;(undo-tree-mode)
;;   (global-undo-tree-mode))

;;(setq evil-undo-system 'undo-redo)

(use-package which-key
  :defer 1
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
  :defer 1
  :init
  (smartparens-global-mode)
  ;; Enable strict mode(don't enable it for a config file like this one)
  ;; (smartparens-strict-mode)
)

(use-package evil-nerd-commenter
  :defer 1
  :bind
  (("C-/" . evilnc-comment-or-uncomment-lines)
   ("M-;" . evilnc-comment-or-uncomment-lines)))

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(setq user-init-file (expand-file-name "config.org" user-emacs-directory))
(bind-key "C-c c" (lambda() (interactive)(find-file "~/.emacs.d/config.org")))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("cs" . "src csharp"))

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-edit-src-content-indentation 0)

(org-babel-do-load-languages
 'org-babel-load-lanaguages
 '(
   (emacs-lisp . t)
   (python . t)
   ))

(setq org-confirm-babel-evaluate nil)

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

;; This is nessessary if you want to restore sessions
;; Useful when emacs crashes in daemon mode, because of
;; X11(GTK) connection issues
;; (desktop-save-mode 1)

(use-package magit
  :commands (magit-status))

;; Spellcheck the commits
(add-hook 'git-commit-mode-hook 'flyspell-mode)

(use-package ivy
  :bind (:map ivy-minibuffer-map
	      ("TAB" . ivy-alt-done)
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line)
	      :map ivy-switch-buffer-map
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line)
	      ("TAB" . ivy-done)
	      ("C-d" . ivy-switch-buffer-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x) ;; Enchanced M-x
	 ("C-x C-f" . counsel-find-file))) ;; Enchanced Find File

(efs/buffer-keys
  "b" 'counsel-switch-buffer)

(use-package swiper
  :after ivy
  :bind (:map evil-normal-state-map
	      ("/" . swiper)) ;; Bind "/", in normal mode, to swiper
  :config
  (add-to-list 'ivy-height-alist '(swiper . 5))) ;; Make swiper's hight to 5

(use-package company
  :bind
  (:map evil-normal-state-map
	("M-." . company-complete))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (company-mode))

(use-package flycheck
  :defer 1
  :init
  (global-flycheck-mode))

;; (use-package vertico
;;   :bind (:map vertico-map
;; 		("C-j" . vertico-next)
;; 		("C-k" . vertico-previous)
;; 		("C-f" . vertico-exit)
;; 		:map minibuffer-local-map
;; 		("M-h" . backward-kill-word))
;;   :custom
;;   (vertico-cycle t)
;;   :init
;;   (vertico-mode))

;; (use-package savehist
;;   :after vertigo
;;   :init
;;   (savehist-mode))

;; (use-package marginalia
;;   :after vertico
;;   :custom
;;   (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
;;   :init
;;   (marginalia-mode))

(use-package helpful
  :defer 1
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h C" . helpful-command)))

(use-package projectile
  :demand t
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
	'("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar"))
  (setq projectile-globally-ignored-directories
	'(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store")))

(use-package yasnippet
  :defer 1
  :config
  (yas-global-mode 1))

;; Default shell of choise: eshell
;; (use-package eshell)

;; (bind-key* "M-1" 'eshell evil-normal-state-map)

(use-package perspective
    :demand t
    :bind (("C-M-k" . persp-switch)
	   ("C-M-n" . persp-next)
	   ("C-x k" . persp-kill-buffer*))
    :custom

(persp-initial-frame-name "Main")
    :config
    (unless (equal persp-mode t)
      (persp-mode)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-sideline-show-diagnostic)
  (lsp-ui-sideline-code-actions t)
  (lsp-ui-sideline-update-mode 'line))

;; Integrate lsp and company
(use-package company-lsp
  :config
  (setq company-lsp-cache-candidates 'auto)
  (setq company-lsp-async t)
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-enable-recompletion t))

(use-package dap-mode
  :config
  (dap-auto-configure-mode)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

;; Add Rust config
(dap-register-debug-template "Rust::GDB Run Configuration"
			   (list :type "gdb"
				 :request "launch"
				 :name "GDB::Run"
			 :gdbpath "rust-gdb"
				 :target nil
				 :cwd nil))

(use-package csharp-mode
  :hook lsp-mode
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
  :hook lsp-mode
  :straight `(omnisharp
	      :type git
	      :host github
	      :repo "OmniSharp/omnisharp-roslyn"
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
	      (", n r" . omnisharp-navigate-to-region))
:config
(setq omnisharp-server-executable-path (expand-file-name "config/omnisharp" user-emacs-directory))
(setq omnisharp-auto-complete-want-documentation nil) ;; If docs fetching is a problem, comment this
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook-config))
