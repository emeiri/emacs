(use-package company
  :ensure t
  :config
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 3)
    (global-company-mode t))

(add-to-list 'load-path "~/.emacs.d/company-c-headers/")
(load "company-c-headers")
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/7")

(setq company-backends (delete 'company-semantic company-backends))
;(define-key c-mode-map  [(C-tab)] 'company-gtags)
;(define-key c++-mode-map  [(C-tab)] 'company-gtags)

;(define-key c-mode-map  [(tab)] 'indent-region)
;(define-key c++-mode-map  [(tab)] 'indent-region)


(use-package company-irony
:ensure t
:config
(add-to-list 'company-backends 'company-irony))

(use-package irony
:ensure t
:config
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;(use-package irony-eldoc
;  :ensure t
;  :config
;  (add-hook 'irony-mode-hook #'irony-eldoc))
