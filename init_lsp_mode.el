(use-package lsp-mode
  :commands lsp)

(use-package ccls
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/elpa/ccls")
(load "ccls.el")
(require 'ccls)
(setq ccls-executable "/path/to/ccls/Release/ccls")


(use-package emacs-ccls
  :hook ((c-mode c++-mode python-mode) .
         (lambda () (require 'ccls) (lsp))))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
