
# Table of Contents

1.  [Welcome to my Emacs config](#orgb6ae7ba)
2.  [Startup](#orgaf064e4)
3.  [UI improvements](#org7015164)
4.  [UI Configuration](#org97fad49)
5.  [Keybinding Configuration](#orgab3da51)
6.  [Editor & Files improvements](#org944f40a)
7.  [Packages](#org81a8cb9)
8.  [Navigation](#org74a85c9)
9.  [Languages](#orgb19fa15)



<a id="orgb6ae7ba"></a>

# Welcome to my Emacs config

Some bla bla stuff goes here


<a id="orgaf064e4"></a>

# Startup

\*\* Initial setup


    ;; You will most likely need to adjust this font size for your system!
    (defvar efs/default-font-size 120)
    (defvar efs/default-variable-font-size 120)

    ;; Make frame transparency overridable
    (defvar efs/frame-transparency '(90 . 90))

\*\* Performace


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

\*\* Package initialization


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

\*\* Keep folders clean


    ;; Set user's directory for config files
    ;; (setq user-emacs-directory "/opt/emacs-config")

    (use-package no-littering)
    ;; Keep auto-save files away, so they don't trash the current directory
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

\*\* Packages auto-update
Ensure all packages stay up-to-date


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


<a id="org7015164"></a>

# UI improvements


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

\*\* Font Configuration
I am using the [Fira Code](https://github.com/tonsky/FiraCode) and [Cantarell](https://fonts.google.com/specimen/Cantarell) fonts for this configuration which will more than likely need to be installed on your machine.  Both can usually be found in the various Linux distro package managers or downloaded from the links above.


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


<a id="org97fad49"></a>

# UI Configuration

\*\* Color theme
Using the doom-themes package, since those themes are lit af


    (use-package doom-themes
      :init (load-theme 'doom-palenight t))

**\* Better modeline
\*NOTE**: You have to run \`M-x all-the-icons-install-fonts\` so that mode
line icons are loaded and installed correctly


    (use-package all-the-icons)

    (use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 15)))


<a id="orgab3da51"></a>

# Keybinding Configuration

\*\* ESC quit prompts(like in VIM)


    (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

\*\* Evil mode
This configuration uses [evil-mode](https://evil.readthedocs.io/en/latest/index.html) for a Vi-like modal editing experience.  [general.el](https://github.com/noctuid/general.el) is used for easy keybinding configuration that integrates well with which-key.  [evil-collection](https://github.com/emacs-evil/evil-collection) is used to automatically configure various Emacs modes with Vi-like keybindings for evil-mode.
The "C-<something>" stands for Ctrl <something>
The "M-<something>" stands for Meta(also called Alt) <something>
Example: "C-g" is clicking at the same time "Ctrl" and "g"


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

Configure evil-undo-system(TODO: Fix, 'cuz it's not working properly)


    ;; Define Evil undo system
    (use-package undo-tree
      :init
      ;;(undo-tree-mode)
      (global-undo-tree-mode))

    ;;(setq evil-undo-system 'undo-redo)

\*\* Which key
[which-key](https://github.com/justbur/emacs-which-key) is a useful UI panel that appears when you start pressing any key binding in Emacs to offer you all possible completions for the prefix.
Example: Pressing "C-c" will show a panel at the bottom of the frame displaying all of the bindings under that prefix and which command they run.


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


<a id="org944f40a"></a>

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

\*\* Smartparens
Smartly dealing with parentheses


    (use-package smartparens
      :init
      (smartparens-global-mode)
      ;; Enable strict mode(don't enable it for a config file like this one)
      ;; (smartparens-strict-mode)
      )

\*\* Org mode

**\*** Remove all "Result" blocks from an org file


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

**\*** Org mode highlighting and code evaluation
The ability to evaluate code and have highlighting in the code blocks
Commands:

-   evaluate-last-expression -> Place on the last parenthesis and click "C-x C-e" to

execute that particular expression

-   org-ctrl-c-ctrl-c-hook -> Cursor placed inside a code block and clicking "C-c C-c"

evaluates the code block as if the config is reloaded


    ;;
    (org-babel-do-load-languages
     'org-babel-load-lanaguages
     '(
       (emacs-lisp . t)
       (python . t)
       ))

    (setq org-confirm-babel-evaluate nil)


<a id="org81a8cb9"></a>

# Packages

\*\* Magit
The famous magit!


    (use-package magit)

\*\* Ivy & Counsel
Generic completion mechanism, paired with ivy-enchanced versions of common Emacs commands


    (use-package ivy
      :config
      (ivy-mode 1))


    (use-package counsel
      :after ivy
      :init
      (global-set-key (kbd "M-x") 'counsel-M-x) ;; Enchanced M-x
      (global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; Enchanced Find File

    )

\*\* Swiper
Fuzzy searching, Ivy-enchanced version of Isearch


    (use-package swiper
      :after counsel
      :init
      (evil-global-set-key 'normal "/" 'swiper) ;; Bind "/", in normal mode, to swiper
      :config
      (add-to-list 'ivy-height-alist '(swiper . 5)) ;; Make swiper's hight to 5
      )

-   Company

Text completion framework(IntelliSense).
From now on, every language must be added as a backend to this one.
Configure all programming language packages with ":after company"


    (use-package company
      :init
      (evil-global-set-key 'insert (kbd "M-.") 'company-complete)
      :config
      (add-hook 'after-init-hook 'global-comapny-mode)
      (company-mode))

-   Flycheck


<a id="org74a85c9"></a>

# Navigation

-   Buffers


<a id="orgb19fa15"></a>

# Languages

-   Language Server Protocol

-   Dap Mode

-   CSharp & Omnisharp(TODO: Finish and add omnisharp)


    (defun efs/csharp-mode-setup ()
      (setq c-syntactic-indentation t)
      (c-set-style "ellemtel")
      (setq c-basic-offset 4)
      (setq truncate-lines t))

    (add-hook 'csharp-mode-hook 'efs/csharp-mode-setup t)
    (add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
