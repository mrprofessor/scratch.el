(setq inhibit-startup-message t)


(scroll-bar-mode -1)                 ; Disable visible scrollbar
(tool-bar-mode -1)                   ; Disable visible scrollbar
(tooltip-mode -1)                    ; Disable tooltips
(set-fringe-mode 10)                 ; Give some breathing room?
(menu-bar-mode -1)                   ; Go full spartan


;; Set up the visible bell
(setq visible-bell t)


;; Set font-face
(set-face-attribute 'default nil :font "Cascadia code" :height 120)


;; Set emacs theme
(load-theme 'adwaita)

;; Make ESC quit prompts
(global-set-key  (kbd "<escape>") 'keyboard-escape-quit)


;; Display line numbers
(global-display-line-numbers-mode t)
;; We don't want line numbers in a shell right?
;; Disable line numbers for certain modes
(dolist (mode '(org-hook-mode
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode(lambda() (display-line-numbers-mode 0))))

;; Truncate long lines in certain modes
(add-hook 'org-mode-hook (lambda() (setq truncate-lines nil)))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize package sources
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(require 'use-package)
(setq use-package-always-ensure t)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; package: evil
;; First thing first. Be EVIL!
(use-package evil)
(evil-mode 1)

;; package: command-log-mode
;; Command log mode shows the key presses and their respective emacs
;; and their respective emacs functions in a buffer.
;; Usage:
;; Enable variable: M-x command-log-mode or M-x global-command-log-mode
;; Show buffer: MX clm/toggle-command-log-buffer
(use-package command-log-mode)


;; package: ivy/counsel/swiper
;; Set up Ivy for better completions
;; Installing counsel will install ivy and swiper.
(use-package counsel
  :diminish
  :bind (("C-s" . swiper)
	 ;; Counsel stuff
	 ("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-x C-r" . 'counsel-minibuffer-history)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))


;; package: ivy-rich
;; More friendly interface (display transformer) for ivy.
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; package: doom-modeline
;; Doom modeline: The beauty.
;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))


;; package:  rainbow-delimiters
;; For any buffer that represents a programming mode, we
;; will enable rainbow-delimiters-mode.
;; This mode matches the parenthesis with same color for
;; better visibility.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; package: emacs-which-key
;; This package displays available keybindings in popup.
;; config:
;; 1. Set the pop up delay to 1 seconds.
;; 2. It should popup in the minibuffer.
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1)
  (setq which-key-popup-type 'minibuffer))


;; ;; package: helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
