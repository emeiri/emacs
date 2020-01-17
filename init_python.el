(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup))
;  (add-hook 'python-mode-hook 'jedi:ac-setup))

(use-package company-jedi
    :ensure t)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)


; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
; Fixing another key binding bug in iedit mode

;(setq python-shell-interpreter "ipython"
;      python-shell-interpreter-args "-i")


(define-key python-mode-map (kbd "<f5>") 'elpy-pdb-debug-buffer)
(define-key python-mode-map (kbd "S-<f5>") 'elpy-shell-kill)
(define-key python-mode-map (kbd "<f9>") 'elpy-pdb-toggle-breakpoint-at-point)
(define-key python-mode-map (kbd "C-<f5>") 'run_python)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--colors=Linux --profile=default"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(defun company-jedi-setup ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'company-jedi-setup)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
