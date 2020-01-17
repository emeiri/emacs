(use-package company
:ensure t
:config
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(global-company-mode t))

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

(use-package company-jedi
    :ensure t
    :config
    (add-hook 'python-mode-hook 'jedi:setup))

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
