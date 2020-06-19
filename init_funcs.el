(defun endless/compile-please (comint)
  "Compile without confirmation.
With a prefix argument, use comint-mode."
  (interactive "P")
  ;; Do the command without a prompt.
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
  (save-window-excursion
    (compile (eval compile-command) (and comint t))))
  ;; Create a compile window of the desired width.
  (pop-to-buffer (get-buffer "*compilation*"))
  (other-window 1)
  (enlarge-window
   (- (frame-width)
      endless/compile-window-size
      (window-width))
   'horizontal))

(defun gdb-c()
  "Debug"
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
      (realgud:gdb)))


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
                        (ace-window 0)
                        (delete-window)
                        (message "Build OK")
                        )
                      buffer)))


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

(defun switch-to-term()
  "Switch to terminal or create if does not exist"
  (interactive)
  ( if (get-buffer "*ansi-term*")
      ;;  (if (get-buffer "*shell*")
      (switch-to-buffer "*ansi-term*")
;;      (switch-to-buffer "*eshell")
;;    (eshell)))
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

(defun tl/dired-copy-path-at-point ()
    (interactive)
    (dired-copy-filename-as-kill 0))

(defun copy-current-filename()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun eshell-load-bash-aliases ()
           "Reads bash aliases from Bash and inserts
    them into the list of eshell aliases."
           (interactive)
           (progn
                   (message "Parsing aliases")
                   (shell-command "alias" "bash-aliases" "bash-errors")
                   (switch-to-buffer "bash-aliases")
                   (replace-string "alias " "")
                   (goto-char 1)
                   (replace-string "='" " ")
                   (goto-char 1)
                   (replace-string "'\n" "\n")
                   (goto-char 1)
                   (let ((alias-name) (command-string) (alias-list))
                        (while (not (eobp))
                           (while (not (char-equal (char-after) 32))
                                  (forward-char 1))
                               (setq alias-name
                                       (buffer-substring-no-properties (line-beginning-position) (point)))
                               (forward-char 1)
                               (setq command-string
                                       (buffer-substring-no-properties (point) (line-end-position)))
                               (setq alias-list (cons (list alias-name command-string) alias-list))
                               (forward-line 1))
                        (setq eshell-command-aliases-list alias-list))
               (if (get-buffer "bash-aliases")(kill-buffer "bash-aliases"))
               (if (get-buffer "bash-errors")(kill-buffer "bash-errors"))))

(defun my-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

;; Global key bindings
(load "~/.emacs.d/init_global_keys.el")

(defun run_kfc()
  (interactive)
  (let ((insert-default-directory t))
    (let ((runfile (read-file-name "KFC runfile:")))
      (message "KFC runfile: %s" runfile)
      (message "%s " ((projectile-project-root) "/")
     )
    )
    )
  )

(defun run_shell()
  (interactive)
(let ((process-environment
       `(,(concat "LD_LIBRARY_PATH=.:/opt/intel/mkl_nightly_2017_20160727_lnx/compiler/lib/intel64:/opt/intel/mkl_nightly_2017_20160727_lnx/mkl/lib/intel64:/opt/intel/icc-latest/lib_lin")
         ,@process-environment)))
  (shell)))


(defun run_python()
  (interactive)
  (elpy-shell-send-region-or-buffer)
  )
