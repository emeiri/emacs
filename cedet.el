
(load-file (concat user-emacs-directory "/cedet/cedet-devel-load.el"))
(load-file (concat user-emacs-directory "/cedet/contrib/cedet-contrib-load.el"))

(require 'cc-mode)
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

;(global-ede-mode 1)
;(ede-cpp-root-project "ogldev" :file "/home/emeiri/ogldev/tutorial53/tutorial53.cpp" :include-path '("/home/emeiri/ogldev/"))
