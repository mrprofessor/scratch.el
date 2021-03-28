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


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize packate sources
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
