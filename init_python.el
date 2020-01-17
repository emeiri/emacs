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
