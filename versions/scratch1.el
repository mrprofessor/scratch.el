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
;; Install packages
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First thing first. Evil mode
(use-package evil)
(evil-mode 1)

;; Command log mode shows the key presses and their respective emacs
;; and their respective emacs functions in a buffer.
;; Usage:
;; Enable variable: M-x command-log-mode or M-x global-command-log-mode
;; Show buffer: MX clm/toggle-command-log-buffer
(use-package command-log-mode)

;; Set up Ivy for better completions
;; Installing counsel will install ivy and swiper.
(use-package counsel
  :diminish
  :bind (("C-s" . swiper)
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
