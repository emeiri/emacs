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
  (which-key-mode t))

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

(use-package swiper-helm
  :ensure t
  :config
  (progn
  ;  (ivy-mode 1)
  ;  (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper-helm)
    ))

(use-package iedit    ; C-; to edit all occurences of current string
  :ensure t)

;(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  (flymake-mode-off))

(use-package helm
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-word-1))

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
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))

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
;        ("<f12>"     . treemacs)
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
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package paredit
  :ensure t)

(use-package iy-go-to-char
  :ensure t)

(use-package free-keys
  :ensure t)

(use-package imenu-anywhere
  :ensure t)

(use-package smartscan
  :ensure t)
(smartscan-mode t)

;(use-package dashboard
;  :ensure t
;  :config
;    (dashboard-setup-startup-hook)
;    (setq dashboard-startup-banner "~/.emacs.d/img/dashLogo.png")
;    (setq dashboard-items '((recents  . 5) (projects . 5)))
;    (setq dashboard-banner-logo-title ""))

(use-package realgud
  :ensure t)

;(use-package spaceline
;  :ensure t
;  :config
;  (require 'spaceline-config)
;  (setq spaceline-buffer-encoding-abbrev-p nil)
;  (setq spaceline-line-column-p nil)
;  (setq spaceline-line-p nil)
;  (setq powerline-default-separator (quote arrow))
;  (spaceline-spacemacs-theme))

(use-package move-text
  :ensure t)

(use-package fill-function-arguments
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-selector 'helm))

(use-package ztree
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package realgud-ipdb
  :ensure t)

(use-package ob-async
  :ensure t)

(use-package rtags
  :ensure t)
