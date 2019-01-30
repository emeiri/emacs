; General
;(set-background-color "black")
;(cua-mode 1)
(desktop-save-mode 1)                                      ;; save/restore opened files
(electric-pair-mode 1)
(global-auto-revert-mode 1)                                ;; when a file is updated outside emacs, make it update if it's already opened in emacs
(global-hl-line-mode 1)                                    ;; turn on highlighting current line
(global-linum-mode 0)                                      ;; show line numbers
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :height 150)
(setq auto-window-vscroll nil)
(setq compilation-ask-about-save nil)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq visible-bell t)
(setq-default indent-tabs-mode -1)
(show-paren-mode 1)                                        ;; turn on bracket match highlight
(tool-bar-mode -1)
(tooltip-mode -1)
(which-function-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq next-line-add-newlines t)
(winner-mode 1)
(normal-erase-is-backspace-mode 0)
(highlight-indentation-mode 0)
(setq tramp-default-method "ssh")

(defcustom endless/compile-window-size 105
  "Width given to the non-compilation window."
  :type 'integer
  :group 'endless)

(defun endless/compile-please (comint)
  "Compile without confirmation.
With a prefix argument, use comint-mode."
  (interactive "P")
  ;; Do the command without a prompt.
  (save-window-excursion
    (compile (eval compile-command) (and comint t)))
  ;; Create a compile window of the desired width.
  (pop-to-buffer (get-buffer "*compilation*"))
  (enlarge-window
   (- (frame-width)
      endless/compile-window-size
      (window-width))
   'horizontal))


;; keep a list of recently opened files
(require 'recentf)
(recentf-mode 1)

; defaliases
(defalias 'yes-or-no-p 'y-or-n-p)

; MELPA
(require 'package)
(setq package-enable-at-startup nil)
; add MELPA to repository list
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; Packages
;(use-package doom-themes
;  :ensure t
;  :config
					;  (load-theme 'doom-one t))
(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

;(use-package tabbar
;  :ensure t
;  :config (tabbar-mode 1))

(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0))))) 
    ))
(global-set-key (kbd "M-o") 'ace-window)

(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
;;    (global-set-key (kbd "M-x") 'counsel-M-x)
;;    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
 ;;   (global-set-key (kbd "<f1> l") 'counsel-load-library)
 ;;   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(helm-ag-ignore-buffer-patterns (quote ("\\.caffe_pb2\\.py\\'")))
 '(helm-grep-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*/caffe_pb2.py" "tags")))
 '(package-selected-packages
   (quote
    (eyebrowse babel git-gutter pcre2el dired+ treemacs-projectile smartparens ggtags expand-region hungry-delete jedi zenburn-theme flx-ido dash-functional yasnippet-snippets yasnippet-classic-snippets which-key use-package try tabbar-ruler solarized-theme org-bullets neotree magit-gh-pulls iedit helm-projectile helm-c-yasnippet helm-ag frame-tabs flycheck elpy doom-themes counsel autopair auto-complete-c-headers ag ace-window ac-c-headers))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme 'darkokai t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 150 :family "Hack"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

(load "~/.emacs.d/ac.el")

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode t))

;(setq tabbar-ruler-global-tabbar t)    ; get tabbar
;(setq tabbar-ruler-global-ruler nil)   ; get global ruler
;(setq tabbar-ruler-popup-menu t)       ; get popup menu.
;(setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
;(setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
;(require 'tabbar-ruler)

(use-package iedit    ; C-; to edit all occurences of current string
  :ensure t)

;(global-flycheck-mode)
;(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

;(require 'c-c++-mode)
(require 'cc-mode)
(setq c-basic-offset 4)
(setq compilation-scroll-output t)
(setq compilation-skip-threshold 2);                            Skip compilation warnings
(setq split-height-threshold 0)
(setq compilation-window-height 10)
(define-key c-mode-map [C-f9] #'compile) ;; This gives a regular `compile-command' prompt.
(define-key c++-mode-map [C-f9] #'compile) ;; This gives a regular `compile-command' prompt.
(define-key c-mode-map [f9]   #'endless/compile-please) ;; This just compiles immediately.
(define-key c++-mode-map [f9]   #'endless/compile-please) ;; This just compiles immediately.

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
      )
      (run-with-timer 0.5 nil
                      (lambda (buf)
                       ; (bury-buffer buf)
					; (switch-to-prev-buffer (get-buffer-window buf) 'kill)
			(ace-window 1)
			(delete-other-windows)
			(message "Build OK")
			)
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)

(defun my::add-semantic-to-ac()
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'my::add-semantic-to-ac)

(global-ede-mode 1)
;(ede-cpp-root-project "ogldev" :file "/home/emeiri/ogldev/tutorial53/tutorial53.cpp" :include-path '("/home/emeiri/ogldev/"))

;; HELM
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
;(global-set-key (kbd "M-y") 'helm-show-kill-ring)    ; replaced by counsel
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(defalias 'list-buffers 'helm-buffers-list)

(setq helm-split-window-inside-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t
	  helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
	  helm-buffers-fuzzy-matching t
	  helm-recentf-fuzzy-match    t)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
	  

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)
(setq helm-autoresize-max-height 40)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

;; PROJECTILE
(projectile-mode)
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-project-search-path '("~/"))
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-globally-ignored-files '("GTAGS" "GRTAGS"))
(setq projectile-enable-caching t)


(use-package elpy
  :ensure t
  :config  
  (elpy-enable))

; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")
(highlight-indentation-mode -1)

;; All The Icons
(use-package all-the-icons :ensure t)

;; NeoTree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(put 'downcase-region 'disabled nil)

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-word-1))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))

(use-package undo-tree
:ensure t
  :init
  (global-undo-tree-mode))

(use-package beacon
:ensure ;TODO: 
:config
(beacon-mode 1))

(use-package hungry-delete
  :ensure t
  :config
  (hungry-delete-mode))

(use-package expand-region
:ensure t
:config 
(global-set-key (kbd "C-=") 'er/expand-region))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-quoting t)) ; this fixes the quote problem I mentioned

(use-package ggtags
  :ensure t
  :config 
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1))))
  )

(use-package smartparens
  :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "F12") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-git-command-pipe           ""
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-max-git-entries            5000
          treemacs-no-png-images              nil
          treemacs-no-delete-other-windows    t
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-cursor                nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("<f12>"     . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package pcre2el
  :ensure t
  :config  
  (pcre-mode))

(use-package git-gutter
   :ensure t
   :init (global-git-gutter-mode +1))

(use-package eyebrowse
  :ensure t
  :config (eyebrowse-mode t))

(use-package magit
  :ensure t
  )

(defun switch-to-workflow()
  (interactive)
  (switch-to-buffer "workflow.org")
  )

;; Global key bindings
(global-set-key (kbd "<f1>") 'list-buffers)
(global-set-key (kbd "<f2>") 'helm-projectile-find-file-in-known-projects)
(global-set-key (kbd "<f3>") 'helm-semantic-or-imenu)
(global-set-key (kbd "<f4>") 'bookmark-jump)
(global-set-key (kbd "<f6>") 'switch-to-workflow)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-S-f") 'helm-projectile-ag)
(global-set-key [delete] 'delete-char)
(global-set-key (kbd "C-0") 'delete-window)

;;(require 'ob-shell)
;;(org-babel-do-load-languages 'org-babel-load-languages '((sh . t )))

