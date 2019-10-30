(use-package elpy
  :ensure t
  :config
  (elpy-enable))

; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
; Fixing another key binding bug in iedit mode

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")
