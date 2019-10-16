;;;; General
; General
;(set-background-color "black")
;(cua-mode 1)
(desktop-save-mode 1)                                      ;; save/restore opened files
(electric-pair-mode 1)
(global-auto-revert-mode 1)                                ;; when a file is updated outside emacs, make it update if it's already opened in emacs
(global-hl-line-mode 1)                                    ;; turn on highlighting current line
(global-linum-mode -1)                                      ;; show line numbers
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :height 150)
(setq auto-window-vscroll nil)
(setq compilation-ask-about-save nil)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode -1)
(show-paren-mode 1)                                        ;; turn on bracket match highlight
(tool-bar-mode -1)
(tooltip-mode -1)
(setq tooltip-use-echo-area t)
(which-function-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq next-line-add-newlines t)
(winner-mode 1)
(normal-erase-is-backspace-mode 0)
(setq column-number-mode t)
(savehist-mode 1)
(size-indication-mode t)
(set-language-environment "UTF-8")
(electric-indent-mode 1)
(setq scroll-conservatively 100)
(set-frame-font "Hack 15" nil t)
(setq create-lockfiles nil)
(setq compilation-auto-jump-to-first-error t)
(transient-mark-mode nil)
(setq compilation-always-kill t)

(require 'misc)

(add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))

(setq tramp-default-method "ssh")

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

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

(global-set-key (kbd "M-o") 'ace-window)

;;(load "~/.emacs.d/counsel.el")

(use-package swiper-helm
  :ensure t
  :config
  (progn
  ;  (ivy-mode 1)
  ;  (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper-helm)
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
    (dumb-jump fill-function-arguments beacon undo-tree projectile helm yasnippet company helm-swoop swiper magit treemacs web-mode all-the-icons ztree move-text spaceline realgud dashboard smartscan imenu-anywhere free-keys iy-go-to-char company-anaconda company-quickhelp paredit flycheck-irony company-jedi irony-eldoc company-irony eyebrowse babel git-gutter pcre2el dired+ treemacs-projectile smartparens ggtags expand-region hungry-delete jedi zenburn-theme flx-ido dash-functional yasnippet-snippets yasnippet-classic-snippets which-key use-package try tabbar-ruler solarized-theme org-bullets neotree magit-gh-pulls iedit helm-projectile helm-c-yasnippet helm-ag frame-tabs flycheck elpy doom-themes counsel autopair auto-complete-c-headers ag ace-window ac-c-headers)))
 '(send-mail-function (quote mailclient-send-it)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme 'darkokai t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))


;; Choose autocomplete or company
;;(load "~/.emacs.d/ac.el")
(load "~/.emacs.d/company.el")
;(load "~/.emacs.d/company_aritra.el")

(load "~/.emacs.d/yasnippet.el")

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
(global-semantic-idle-summary-mode 1)
(setq semantic-idle-scheduler-no-working-message t)
(advice-add 'semantic-idle-scheduler-function :around #'ignore)
(semantic-mode 1)

(defun my::add-semantic-to-ac()
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'my::add-semantic-to-ac)

(global-ede-mode 1)
;(ede-cpp-root-project "ogldev" :file "/home/emeiri/ogldev/tutorial53/tutorial53.cpp" :include-path '("/home/emeiri/ogldev/"))

;; HELM
(use-package helm
  :ensure t)
(load "~/.emacs.d/setup_helm_gtags.el")
(use-package helm-ag
  :ensure t)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
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
(use-package projectile
  :ensure t
  :config
  (projectile-mode))
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))
(setq projectile-completion-system 'helm)
(setq projectile-project-search-path '("~/"))
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-globally-ignored-files '("GTAGS" "GRTAGS"))
(setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(use-package elpy
  :ensure t
  :config
  (elpy-enable))

; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
; Fixing another key binding bug in iedit mode

(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")

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
  (add-hook 'python-mode-hook 'jedi:setup))
;  (add-hook 'python-mode-hook 'jedi:ac-setup))

(defun company-jedi-setup ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'company-jedi-setup)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

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
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-quoting t)) ; this fixes the quote problem I mentioned

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
  :ensure t)

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

(defun switch-to-workflow()
  (interactive)
  (switch-to-buffer "workflow.org")
  )

(defun toggle-narrow-exand ()
  "Toggle between narrow and expand region."
  (interactive)
  ;; use a property “state”. Value is t or nil
  (if (get 'toggle-narrow-exand 'state)
      (progn
        (narrow-to-defun)
        (put 'toggle-narrow-exand 'state nil))
    (progn
      (widen)
      (put 'toggle-narrow-exand 'state t))))

(defun copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))

(defun cfn ()
  "Copy the current file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
            (message "Copied file name '%s' to the clipboard." filename))))

(defun cbn ()
  "Copy the current buffer name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-name))))
    (when filename
      (kill-new filename)
            (message "Copied buffer name '%s' to the clipboard." filename))))

(defun cleanup-buffer-safe ()
    "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
    (interactive)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                                      name (file-name-nondirectory new-name)))))))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
    (indent-for-tab-command))

(defun kill-whole-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun switch-to-term()
  "Switch to terminal or create if does not exist"
  (interactive)
  (if (get-buffer "*ansi-term*")
    (switch-to-buffer "*ansi-term*")
    (ansi-term "/bin/bash")))

(defun open-next-line()
  "Open a new line below"
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  )

(defun save-all ()
  "Save all file-visiting buffers without prompting."
  (interactive)
  (save-some-buffers t) ;; Do not prompt for confirmation.
  (message "All buffers saved")
  )

(defun delete-window-balance ()
  "Delete window and rebalance the remaining ones."
  (interactive)
  (delete-window)
  (balance-windows))

(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun goto-line-show ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively #'goto-line))
    (linum-mode -1)))


;; Global key bindings
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key [delete] 'delete-char)
(global-set-key (kbd "<f1>") 'list-buffers)
(global-set-key (kbd "<f2>") 'helm-projectile-find-file-dwim)
(global-set-key (kbd "<f3>") 'helm-semantic-or-imenu)
(global-set-key (kbd "<f4>") 'bookmark-jump)
(global-set-key (kbd "<f5>") 'switch-to-term)
;(global-set-key (kbd "<f6>") 'switch-to-workflow)
(global-set-key (kbd "<f6>") 'avy-goto-word-1)
(global-set-key (kbd "<f12>") 'open-next-line)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-g g") 'goto-line-show)
(global-set-key (kbd "M-g j") 'dumb-jump-go)
(global-set-key (kbd "M-g ,") 'dumb-jump-back)
(global-set-key (kbd "M-g q") 'dumb-jump-quick-look)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(define-key c++-mode-map (kbd "M-q") 'fill-function-arguments-dwim)
(global-set-key (kbd "M-=") 'er/expand-region)
(global-set-key (kbd "C-M-d") 'kill-whole-word)
(global-set-key (kbd "C-0") 'delete-window-balance)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below-focus)
(global-set-key (kbd "C-3") 'split-window-right-focus)
(global-set-key (kbd "C-,") 'pop-global-mark)
(global-set-key (kbd "C-.") 'helm-imenu-anywhere)
(global-set-key (kbd "C-S-f") 'helm-projectile-ag)
;(global-set-key (kbd "<C-return>" 'open-line-below)
;(global-set-key (kbd "<C-S-RET>") 'open-line-above)
(global-set-key (kbd "C-<f1>") 'toggle-narrow-exand)
(global-set-key (kbd "C-<f3>") 'helm-gtags-select)
(global-set-key (kbd "C-<up>") (lambda ()
                                 (interactive)
                                 (ignore-errors (next-line -5))))
(global-set-key (kbd "C-<down>") (lambda ()
                                 (interactive)
                                 (ignore-errors (next-line 5))))
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-l") 'copy-whole-line)
(global-set-key (kbd "C-c l") 'recenter-top-bottom)
(define-key global-map (kbd "C-c o") 'iedit-mode)
(global-set-key (kbd "C-c @ @") 'hs-hide-all)
(global-set-key (kbd "C-c @ h") 'hs-hide-block)
(global-set-key (kbd "C-c @ s") 'hs-show-block)
(global-set-key (kbd "C-c @ SPC") 'hs-show-all)
(global-set-key (kbd "C-x s") 'save-all)
(global-set-key (kbd "C-x <down>") 'pop-global-mark)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key [remap forward-word] 'forward-to-word)

;;(require 'ob-shell)
;;(org-babel-do-load-languages 'org-babel-load-languages '((sh . t )))

(highlight-indentation-mode nil)

(use-package ztree
  :ensure t)

(use-package all-the-icons
  :ensure t)
