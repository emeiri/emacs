;;; Commentary:
                                        ; General
;;; Code:
(load "~/.emacs.d/init_flags.el")
(load "~/.emacs.d/init_melpa.el")
(load "~/.emacs.d/init_packages.el")
(load "~/.emacs.d/init_funcs.el")

(require 'misc)
;; keep a list of recently opened files
(require 'recentf)
(recentf-mode 1)

(add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defcustom endless/compile-window-size 105
  "Width given to the non-compilation window."
  :type 'integer
  :group 'endless)

;;(load "~/.emacs.d/counsel.el")

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
    (rtags ob-async dired-subtree dired-narrow all-the-icons-dired ccls helm-gtags dumb-jump fill-function-arguments beacon undo-tree projectile helm yasnippet company helm-swoop swiper magit treemacs web-mode all-the-icons ztree move-text spaceline realgud dashboard smartscan imenu-anywhere free-keys iy-go-to-char company-anaconda company-quickhelp paredit flycheck-irony company-jedi irony-eldoc company-irony eyebrowse babel git-gutter pcre2el dired+ treemacs-projectile smartparens ggtags expand-region hungry-delete jedi zenburn-theme flx-ido dash-functional yasnippet-snippets yasnippet-classic-snippets which-key use-package try tabbar-ruler solarized-theme org-bullets neotree magit-gh-pulls iedit helm-projectile helm-c-yasnippet helm-ag frame-tabs flycheck elpy doom-themes counsel autopair auto-complete-c-headers ag ace-window ac-c-headers)))
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-I/home/emeiri/projects/ncf/include" "-I/home/emeiri/projects/ncf/kernels/include"))))
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


;(require 'c-c++-mode)
(require 'cc-mode)
(setq c-basic-offset 4)
(setq compilation-scroll-output t)
(setq compilation-skip-threshold 2);                            Skip compilation warnings
(setq split-height-threshold 0)
(setq compilation-window-height 10)


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
(load "~/.emacs.d/setup_helm_gtags.el")

;; PROJECTILE
(setq projectile-completion-system 'helm)
(setq projectile-project-search-path '("~/"))
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-globally-ignored-files '("GTAGS" "GRTAGS"))
(setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(load "~/.emacs.d/init_elpy.el")

(put 'downcase-region 'disabled nil)

(defun company-jedi-setup ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'company-jedi-setup)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-quoting t) ; this fixes the quote problem I mentioned

(load "~/.emacs.d/dired.el")

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)


(add-hook 'eshell-mode-hook 'eshell-load-bash-aliases)

(define-key c++-mode-map (kbd "M-q") 'fill-function-arguments-dwim)
(define-key c++-mode-map (kbd "C-<f5>") 'gdb-c)

;;(require 'ob-shell)
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t )))
(setq org-confirm-babel-evaluate nil)

(load "~/.emacs.d/init_python.el")
