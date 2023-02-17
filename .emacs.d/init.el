;; init.el Owner: MRH
;; Date: 21.01.2023 15:50

;; no startup message
(setq inhibit-startup-message t)

(setq image-use-external-converter t)
;; set font
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 90)

;; disable basic emacs tools
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq-default tab-width 4)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
  (R . t)))

;; package sources
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; refresh
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'cl-lib)
(use-package cl-libify
  :ensure t)

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

;; skeleton
(define-skeleton org-skeleton
  "Header info for a emacs-org file."
  "Title: "
  "#+TITLE:" str " \n"
  "#+AUTHOR: Moritz R. Hoffmann \n"
  "#+INFOJS_OPT: \n"
  "#+PROPERTY: session *R* \n"
  "#+PROPERTY: cache yes \n"
  "#+PROPERTY: results graphics \n" 
  "#+PROPERTY: exports both \n"
  "#+PROPERTY: tangle yes \n"
  "#+STARTUP: indent \n"
  "#+STARTUP: content \n"
  "#+LATEX_HEADER: \\usepackage{parskip} \n"
  "#+LATEX_HEADER: \\usepackage[ngerman]{babel} \n"
  "#+LANGUAGE: ngerman \n"
  "-------------------------------------------------------------------------------
")

(add-hook 'text-mode-hook #'auto-fill-mode)
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'auto-fill-mode)
(global-visual-line-mode 1)
(show-paren-mode 1); Matches parentheses and such in every mode

;;; AUCTeX
;; Customary Customization, p. 1 and 16 in the manual, and http://www.emacswiki.org/emacs/AUCTeX#toc2
(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t); Enable parse on save.
(setq-default TeX-master nil)

(setq TeX-PDF-mode t); PDF mode (rather than DVI-mode)

(use-package slime
  :ensure t)

(require 'org-tempo)
(setq inferior-lisp-program "sbcl") 
(add-to-list 'org-structure-template-alist '("lp" . "src lisp"))
(add-to-list 'org-structure-template-alist '("R" . "src R"))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq org-confirm-babel-evaluate nil)

(setq org-src-tab-acts-natively t)

(use-package org-bullets
      :ensure t
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; pdftools
(use-package pdf-tools
  :ensure t)

;; remember to download ess as gentoo package 
(require 'ess)

(use-package sudo-edit
  :ensure t)

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init))

(use-package magit
  :ensure t)

(use-package go-translate
  :ensure t)

(setq gts-translate-list '(("de" "en") ("jp" "de")))

;; (setq gts-default-translator (gts-translator :engines (gts-bing-engine)))
(setq gts-default-translator
      (gts-translator
       :picker (gts-prompt-picker)
       :engines (list (gts-bing-engine) (gts-google-engine))
       :render (gts-buffer-render)))

(setq default-directory "~/orgdoc/")

;; general keybinds
(use-package general
  :config
  (general-evil-setup t))

(nvmap :prefix "SPC"
       "b b"   '(ibuffer :which-key "Ibuffer")
       "b c"   '(clone-indirect-buffer-other-window :which-key "Clone indirect
       buffer other window")
       "b k"   '(kill-current-buffer :which-key "Kill current buffer")
       "b n"   '(next-buffer :which-key "Next buffer")
       "b p"   '(previous-buffer :which-key "Previous buffer")
       "b l"   '(ibuffer-list-buffers :which-key "Ibuffer list buffers")
       "b K"   '(kill-buffer :which-key "Kill buffer"))

(nvmap :prefix "SPC"
       "w c"   '(delete-other-windows :which-key "delete other windows than
       currently selected")
       "w n"   '(evil-window-new :which-key "create new window")
       "w r"   '(evil-window-move-far-right :which-key "moves window to the
       right")
	   "g t"   '(gts-do-translate :which-key "starts translator")
	   "l h"   '((lambda() (interactive) (set-input-method "japanese-hiragana"))
				 :which-key "changes input language to japanese hiragana")
	   "l k"   '((lambda() (interactive) (set-input-method "japanese-katakana"))
				 :which-key "changes input language to japanese katakana")
	   "l j"   '((lambda() (interactive) (set-input-method "japanese"))
				 :which-key "changes input language to japanese kanji")
	   "l l"   '((lambda() (interactive) (set-input-method "latin-prefix"))
				 :which-key "changes input language to latin-prefix
       (default)")
	   "k k"   '(kanji-mode-stroke-order :which-key "shows kanji stroke order")
	   "t t"   '(term :which-key "opens a terminal")
	   "o s"   '(shell :which-key "opens a shell"))

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "e b"   '(eval-buffer :which-key "Eval elisp in buffer")
       "e d"   '(eval-defun :which-key "Eval defun")
       "e e"   '(eval-expression :which-key "Eval elisp expression")
       "e l"   '(eval-last-sexp :which-key "Eval last sexression")
       "e r"   '(eval-region :which-key "Eval region"))

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "."     '(find-file :which-key "Find file")
       "f f"   '(find-file :which-key "Find file")
       "f s"   '(save-buffer :which-key "Save file")
       "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
       "f c"   '(copy-file :which-key "Copy file")
       "f d"   '(delete-file :which-key "Delete file")
       "f r"   '(rename-file :which-key "Rename file")
       "f w"   '(write-file :which-key "Save file as...")
       "f S"   '(sudo-edit-find-file :which-key "Sudo edit file"))

(nvmap :keymaps 'override :prefix "SPC"
       "c c"   '(compile :which-key "Compile")
  	   "n n"   '((lambda() (interactive) (find-file "~/orgdoc/notes.org"))
				 :which-key "access personal notes")
  	   "e c c" '((lambda() (interactive) (find-file "~/.emacs.d/init.el"))
				 :which-key "access emacs config")
	   "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el"))
				 :which-key "Reload emacs config"))

(nvmap :keymaps 'override :prefix "SPC"
       "o *"   '(org-ctrl-c-star :which-key "Org-ctrl-c-star")
       "o -"   '(org-ctrl-c-minus :which-key "Org-ctrl-c-minus")
       "o e"   '(org-export-dispatch :which-key "Org export dispatch")
       "o f"   '(org-footnote-new :which-key "Org footnote new")
       "o h"   '(org-toggle-heading :which-key "Org toggle heading")
       "o i"   '(org-toggle-item :which-key "Org toggle item")
       "o n"   '(org-store-link :which-key "Org store link")
       "o p"   '(org-set-property :which-key "Org set property")
       "o t"   '(org-todo :which-key "Org todo")
       "o x"   '(org-toggle-checkbox :which-key "Org toggle checkbox")
       "o B"   '(org-babel-tangle :which-key "Org babel tangle")
       "o I"   '(org-toggle-inline-images :which-key "Org toggle inline imager")
       "o T"   '(org-todo-list :which-key "Org todo list")
       "o a"   '(org-agenda :which-key "Org agenda")
       "o s"   '(org-skeleton :which-key "insert org skeleton template")
	   "s e"   '(org-edit-src-exit :which-key "exit src code block edit")
	   "s w"   '(org-edit-src-code :which-key "edit src code block")
	   "s s"   '(org-edit-src-save :which-key "save src code edit"))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook
		shell-mode-hook))

(use-package kanji-mode
  :ensure t)
  
(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Guess who's back, bitches!")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.emacs.d/monado_lambda_emacs.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 7)
                          (agenda . 5 )
                          (bookmarks . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
			      (bookmarks . "book"))))

  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(kanji-mode ement magit-popup magit evil-collection anki go-translate cl-libify general which-key use-package pdf-tools org-bullets ivy evil doom-themes doom-modeline all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
