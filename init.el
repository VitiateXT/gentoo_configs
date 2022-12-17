;; init.el Owner: MRH
;; Date: 09.10.2022 15:47

;; no startup message
(setq inhibit-startup-message t)

;; set font
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 80)

;; disable basic emacs tools
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

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
(load-theme 'doom-peacock t)

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

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package flycheck)

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; optionally if you want to use debugger
(use-package dap-mode :after lsp-mode)

;; general keybinds
(use-package general
	:config
	(general-evil-setup t)
	(general-create-definer mrh/leader-keys
		:keymaps '(normal insert visual emacs)
		:prefix "SPC"))

(use-package treemacs
  :ensure t
  :defer t
  :init
    (treemacs-resize-icons 44))
 
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(nvmap :keymaps 'override :prefix "SPC"
  "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs config")
  "b s"   '(save-buffer :which-key "save buffer")
  "b c"   '(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
  "b k"   '(kill-current-buffer :which-key "Kill current buffer")
  "b n"   '(next-buffer :which-key "Next buffer")
  "b p"   '(previous-buffer :which-key "Previous buffer")
  "b B"   '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
  "b K"   '(kill-buffer :which-key "Kill buffer")
  "f f"   '(find-file :which-key "find file")
  "w n"   '(evil-window-new :whick-key "open new window")
  "w c"   '(evil-window-delete :whick-key "close window")
  "d j"   '(dap-java-debug :which-key "debug java app")
  "w r"   '(evil-window-move-far-right :which-key "move window right")
  "w s"   '(evil-window-vsplit :which-key "split window right")
  "s d"   '(treemacs-select-directory :which-key "treemacs directory")
  )


(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treemacs-icons-dired flycheck lsp-java dap-java dap-mode lsp-ivy lsp-ui lsp-mode which-key use-package ivy general evil doom-themes doom-modeline all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
