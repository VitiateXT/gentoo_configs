;; init.el Owner: MRH
;; Date: 21.01.2023 15:50

;; no startup message
(setq inhibit-startup-message t)

;; set font
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 90)

;; disable basic emacs tools
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq-default tab-width 4)
;; org babel enable clisp support
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (lisp . t)))

;; package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; refresh
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ivy as completion framework
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

;; all-the-icons package for modeline
(use-package all-the-icons
  :ensure t)

;; set theme
(use-package doom-themes
  :ensure t)
(setq doom-themes-enable-bold t  
      doom-themes-enable-italic t) 
(load-theme 'doom-tokyo-night t)

;; modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
	(setq which-key-idle-delay 1))

;; evil
(use-package evil
	:ensure t
	:init
	(setq evil-want-integration t) 
	(setq evil-want-keybinding nil)
	(setq evil-vsplit-window-right t)
	(setq evil-split-window-right t)
	(setq evil-want-C-u-scroll t)
	(setq evil-new-window-right t)
	:config
	(evil-mode 1))

(use-package org-bullets
      :ensure t
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; pdftools
(use-package pdf-tools
  :ensure t)

(use-package slime
  :ensure t)

;; skeleton
(define-skeleton org-skeleton
  "Header info for a emacs-org file."
  "Title: "
  "#+TITLE:" str " \n"
  "#+AUTHOR: Moritz R. Hoffmann\n"
  "#+email: moritz.hoffmann@tutanota.com\n"
  "#+INFOJS_OPT: \n"
  "#+PROPERTY: session *R*" 
  "#+PROPERTY: cache yes" 
  "#+PROPERTY: results graphics" 
  "#+PROPERTY: exports both"
  "#+PROPERTY: tangle yes"
  "#+STARTUP: indent \n"
  "-------------------------------------------------------------------------------
"
  )

;; general keybinds
(use-package general)

(general-define-key 
  :states '(normal visual motion)
  :prefix "SPC" 
  "h r r" '(lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key
  "Reload emacs config"
  "b s"   'save-buffer :which-key "save buffer"
  "b c"   'clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window"
  "b k"   'kill-current-buffer :which-key "Kill current buffer"
  "b n"   'next-buffer :which-key "Next buffer"
  "b p"   'previous-buffer :which-key "Previous buffer"
  "b B"   'ibuffer-list-buffers :which-key "Ibuffer list buffers"
  "b K"   'kill-buffer :which-key "Kill buffer"
  "f f"   'find-file :which-key "find file"
  "w n"   'evil-window-new :whick-key "open new window"
  "w c"   'evil-window-delete :whick-key "close window"
  "w r"   'evil-window-move-far-right :which-key "move window right"
  "w s"   'evil-window-vsplit :which-key "split window right"
  "e b"   'eval-buffer :which-key "evaluate elisp buffer"
  "l s"   'eval-last-sexp :which-key "evaluates last elisp s-expression"
  "o s"   'shell :which-key "opens shell"
  "o e"   'org-export-dispatch :which-key "opens org export"
  "h h"   'org-insert-headind :which-key "org inserts headinf at same level"
  "c b"   'org-insert-todo-heading :which-key "insert checkbox or TODO heading"
  "t b"   'org-toggle-checkbox :which-key "toggle org checkbox"
  "s k"   'org-skeleton :which-key "use org skeleton implementation"
  )


(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(slime org-bullets general evil which-key doom-modeline doom-themes all-the-icons ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
