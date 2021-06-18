
# Table of Contents

1.  [Welcome to my Emacs config](#org0caf6ca)
2.  [Startup](#org9f9a646)
3.  [UI improvements](#orgda21a53)
4.  [UI Configuration](#org83a6d7a)
5.  [Keybinding Configuration](#org0db209a)
6.  [Editor & Files improvements](#org342fd07)
7.  [Packages](#orgc3efb0f)
8.  [Navigation](#org7be96d4)
9.  [Languages](#orgfd8ffbc)



<a id="org0caf6ca"></a>

# Welcome to my Emacs config

Some bla bla stuff goes here


<a id="org9f9a646"></a>

# Startup

\*\* Initial setup


    ;; You will most likely need to adjust this font size for your system!
    (defvar efs/default-font-size 120)
    (defvar efs/default-variable-font-size 120)

    ;; Make frame transparency overridable
    (defvar efs/frame-transparency '(90 . 90))

\*\* Performace


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

\*\* Package initialization


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

\*\* Required packages for config comfort or nessessaty


    (use-package bind-key
      :demand t)

\*\* Keep folders clean


    ;; Set user's directory for config files
    ;; (setq user-emacs-directory "/opt/emacs-config")

    (use-package no-littering
      :demand t)
    ;; Keep auto-save files away, so they don't trash the current directory
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

\*\* Emacs as a daemon
This is really a section to explain that a user can pay the cost of emacs initialization only once
Linux: use systemd to load emacs at startup and connect with a client
As user run in a shell \`systemctl enable emacs &#x2013;user\` and connect to emacs with \`emacsclient -c\`


<a id="orgda21a53"></a>

# UI improvements


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

\*\* Font Configuration
I am using the [Fira Code](https://github.com/tonsky/FiraCode) and [Cantarell](https://fonts.google.com/specimen/Cantarell) fonts for this configuration which will more than likely need to be installed on your machine.  Both can usually be found in the various Linux distro package managers or downloaded from the links above.


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

\*\* IBuffer


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

\*\* Window
By default, \`split-window-vertically\` and \`split-window-horizontally\`\` display the current buffer twice.
Most of the time I’ll change the buffer in the second window, which is why this snippet looks really handy:


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


<a id="org83a6d7a"></a>

# UI Configuration

\*\* Color theme
Using the doom-themes package, since those themes are lit af
[Doom themes screenshots](![img](https://github.com/hlissner/emacs-doom-themes/blob/screenshots/doom-laserwave.png))
Cool themes doom-\*:

-   city-lights, dracula, horizon, lazerwave, mineral, moonlight, nord, one, spacegray


    (use-package doom-themes
      :demand t
      :config
      (setq doom-themes-enable-bold t
    	doom-themes-enable-italic t)
      (load-theme 'doom-one t)
      ;;(doom-themes-neotree-config) ;; Load a custom theme for neotree
      (doom-themes-org-config)) ;; Improve org mode's native fontification

**\* Better modeline
\*NOTE**: You have to run \`M-x all-the-icons-install-fonts\` so that mode
line icons are loaded and installed correctly


    (use-package all-the-icons
      :after doom-modeline)

    (use-package doom-modeline
      :demand t
      :config (doom-modeline-mode 1)
      :custom
      (doom-modeline-height 15)
      (doom-modeline-lsp t))


<a id="org0db209a"></a>

# Keybinding Configuration

\*\* One line keybindings


    ;; ESC quit prompts(like in VIM)
    (bind-key "<escape>" 'keyboard-escape-quit)

    ;; Save ma'h FILEEEEE
    (bind-key* "C-s" 'save-buffer)

\*\* File reloading
Reload file, if it was changed on the disk


    (global-auto-revert-mode t)

\*\* Evil mode
This configuration uses [evil-mode](https://evil.readthedocs.io/en/latest/index.html) for a Vi-like modal editing experience.  [general.el](https://github.com/noctuid/general.el) is used for easy keybinding configuration that integrates well with which-key.  [evil-collection](https://github.com/emacs-evil/evil-collection) is used to automatically configure various Emacs modes with Vi-like keybindings for evil-mode.
The "C-<something>" stands for Ctrl <something>
The "M-<something>" stands for Meta(also called Alt) <something>
Example: "C-g" is clicking at the same time "Ctrl" and "g"


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

Configure evil-undo-system
**TODO**: Fix, 'cuz it's not working properly
**Note**: Disabled


    ;; Define Evil undo system
    ;; (use-package undo-tree
    ;;   :demand t
    ;;   :after evil
    ;;   :init
    ;;   ;;(undo-tree-mode)
    ;;   (global-undo-tree-mode))

    ;;(setq evil-undo-system 'undo-redo)

\*\* Which key
[which-key](https://github.com/justbur/emacs-which-key) is a useful UI panel that appears when you start pressing any key binding in Emacs to offer you all possible completions for the prefix.
Example: Pressing "C-c" will show a panel at the bottom of the frame displaying all of the bindings under that prefix and which command they run.


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


<a id="org342fd07"></a>

# Editor & Files improvements

\*\* Basic file config


    ;; Prefer UTF-8
    (prefer-coding-system 'utf-8)

    ;; Mark matching pairs of parentheses
    (show-paren-mode t)
    (setq show-paren-delay 0.0)

    ;; Delete trailing whitespaces
    (add-hook 'before-save-hook 'delete-trailing-whitespace)

    ;; Ensure files end with a new line
    (setq require-final-newline t)

\*\* Copy/paste to system clipboard
Use these functions, if there are any problems copying and pasting.
Make sure to bind them to something other then C-c & C-v


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

\*\* Smartparens
Smartly dealing with parentheses


    (use-package smartparens
      :defer 1
      :init
      (smartparens-global-mode)
      ;; Enable strict mode(don't enable it for a config file like this one)
      ;; (smartparens-strict-mode)
    )

\*\* Smarter comments


    (use-package evil-nerd-commenter
      :defer 1
      :bind
      (("C-/" . evilnc-comment-or-uncomment-lines)
       ("M-;" . evilnc-comment-or-uncomment-lines)))

\*\* Auto-Saving Changed Files


    (use-package super-save
      :defer 1
      :diminish super-save-mode
      :config
      (super-save-mode +1)
      (setq super-save-auto-save-when-idle t))

\*\* Org mode

**\*** Jump to config file
The curent file you're looking at&#x2026;


    (setq user-init-file (expand-file-name "config.org" user-emacs-directory))
    (bind-key "C-c c" (lambda() (interactive)(find-file "~/.emacs.d/config.org")))

**\*** Add templates for code blocks


    (require 'org-tempo)

    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("cs" . "src csharp"))

**\*** Improve Org mode's source code blocks


    (setq org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-src-window-setup 'current-window
          org-edit-src-content-indentation 0)

**\*** Highlighting and code evaluation
The ability to evaluate code and have highlighting in the code blocks
Commands:

-   evaluate-last-expression -> Place on the last parenthesis and click "C-x C-e" to

execute that particular expression

-   org-ctrl-c-ctrl-c-hook -> Cursor placed inside a code block and clicking "C-c C-c"

evaluates the code block as if the config is reloaded


    (org-babel-do-load-languages
     'org-babel-load-lanaguages
     '(
       (emacs-lisp . t)
       (python . t)
       ))

    (setq org-confirm-babel-evaluate nil)

\*\* Tangle org files on save


    (use-package org-auto-tangle
      :hook (org-mode . org-auto-tangle-mode))

\*\* Save session


    ;; This is nessessary if you want to restore sessions
    ;; Useful when emacs crashes in daemon mode, because of
    ;; X11(GTK) connection issues
    ;; (desktop-save-mode 1)


<a id="orgc3efb0f"></a>

# Packages

\*\* Magit
The famous magit!


    (use-package magit
      :commands (magit-status))

    ;; Spellcheck the commits
    (add-hook 'git-commit-mode-hook 'flyspell-mode)

\*\* Ivy & Counsel
Generic completion mechanism, paired with ivy-enchanced versions of common Emacs commands


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

\*\* Swiper
Fuzzy searching, Ivy-enchanced version of Isearch


    (use-package swiper
      :after ivy
      :bind (:map evil-normal-state-map
    	      ("/" . swiper)) ;; Bind "/", in normal mode, to swiper
      :config
      (add-to-list 'ivy-height-alist '(swiper . 5))) ;; Make swiper's hight to 5

-   Company

Text completion framework(IntelliSense).
From now on, every language must be added as a backend to this one.
Configure all programming language packages with ":after company"


    (use-package company
      :bind
      (:map evil-normal-state-map
    	("M-." . company-complete))
      :config
      (add-hook 'after-init-hook 'global-company-mode)
      (company-mode))

-   Flycheck


    (use-package flycheck
      :defer 1
      :init
      (global-flycheck-mode))

-   Vertigo

Could replace Ivy & Counsel, using Emacs' own API.
Currently **disabled**!


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

-   Helpful

Better keyboard, commands and such documentation(tl;dr better **help** buffer)


    (use-package helpful
      :defer 1
      :bind (("C-h f" . helpful-callable)
    	 ("C-h v" . helpful-variable)
    	 ("C-h k" . helpful-key)
    	 ("C-h C" . helpful-command)))

-   Projectile

Projects management


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

-   Snippets

Let the minions do the hard work


    (use-package yasnippet
      :defer 1
      :config
      (yas-global-mode 1))


<a id="org7be96d4"></a>

# Navigation

-   Shell/Terminal


    ;; Default shell of choise: eshell
    ;; (use-package eshell)

    ;; (bind-key* "M-1" 'eshell evil-normal-state-map)

\*\* Workspaces


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


<a id="orgfd8ffbc"></a>

# Languages

-   Language Server Protocol

VS Code brings LSP to the masses. Allows to plug-and-config servers
for various programming languages to get autocompletion results


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

-   Dap Mode


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

-   CSharp & Omnisharp(TODO: Finish and add omnisharp)


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
