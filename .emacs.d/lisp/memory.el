;; User defined functions

(defun my-smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
   Move point to the first non-whitespace character on this line.
   If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))



;; From 'lucid' package as since 23.2 it is treated as obsolete but no replacement was provided
(defun switch-to-other-buffer (arg)
  "Switch to the previous buffer.
With a numeric arg N, switch to the Nth most recent buffer.
With an arg of 0, buries the current buffer at the
bottom of the buffer stack."
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth arg
          (apply 'nconc
                 (mapcar
                  (lambda (buf)
                    (if (= ?\  (string-to-char (buffer-name buf)))
                        nil
                      (list buf)))
                  (buffer-list)))))))



;; Xml formatting method
(defun my-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))



;; Toggling fullscreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
       (set-frame-parameter nil 'fullscreen
                            (if (equal 'fullboth current-value)
                                (if (boundp 'old-fullscreen) old-fullscreen nil)
                                (progn (setq old-fullscreen current-value)
                                       'fullboth)))))



(defun my-swap-buffers ()
  "Swap horizontal buffers"
  (interactive)
  (if (null (windmove-find-other-window 'right))
      (buf-move-left)
      (buf-move-right)))



(defun my-prog-mode ()
  (interactive)
  (whitespace-mode t)
  (flyspell-mode nil)
  (flyspell-prog-mode))


(defun my-common-prog-mode-setup ()
  (add-hook 'lisp-mode-hook       'my-prog-mode)
  (add-hook 'emacs-lisp-mode-hook 'my-prog-mode)
  (add-hook 'c-mode-common-hook   'my-prog-mode)
  (add-hook 'makefile-mode-hook   'my-prog-mode)
  (add-hook 'qt-pro-mode-hook     'my-prog-mode)
  (add-hook 'java-mode-hook       'my-prog-mode)
  (add-hook 'sh-mode-hook         'my-prog-mode)
  (add-hook 'perl-mode-hook       'my-prog-mode)
  (add-hook 'c++-mode-hook        'my-prog-mode)
  (add-hook 'python-mode-hook     'my-prog-mode)
  (add-hook 'nxml-mode-hook       'my-prog-mode)
  )

;; Method for intuitive emacs server sutdown
(defun my-shutdown-emacs-server () (interactive)
  (when (not (eq window-system 'x))
    (message "Initializing x windows system.")
    (x-initialize-window-system)
    (when (not x-display-name) (setq x-display-name (getenv "DISPLAY")))
    (select-frame (make-frame-on-display display '((window-system . x))))
  )
  (let ((last-nonmenu-event nil)(window-system "x"))(save-buffers-kill-emacs)))



(defun my-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))



(defun my-kill-all-dired-buffers()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let((count 0))
      (dolist(buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count ))))

;; Some custom configuration to ediff
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
                  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
                  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook        'my-ediff-bsh        )
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook                'my-ediff-qh         )

(defun my-reverse-region (&optional arg)
  "Reverse current region, like this: \"a(bc) d\" -> \"d )cb(a\"."
  (interactive "P")
  (let ((reversed
         (apply
          'string
          (reverse
           (string-to-list
            (buffer-substring-no-properties (region-beginning) (region-end)))))))
    (delete-region (region-beginning) (region-end))
    (insert reversed)))

(defun my-unescape-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "~/bin/unescape.py" (current-buffer) t)))

(defun my-escape-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "~/bin/escape.py" (current-buffer) t)))

(defun my-indent-tabs-mode()
  (interactive)
  (setq indent-tabs-mode t))

(defun my-indent-spaces-mode()
  (interactive)
  (setq indent-tabs-mode nil))

; Fix to gud to force it to reuse the same code buffer
(defadvice gud-display-line (around do-it-better activate)
  (let* ((last-nonmenu-event t)      ; Prevent use of dialog box for questions.
     (buffer
      (with-current-buffer gud-comint-buffer
        (gud-find-file true-file)))
     (window (and buffer
                  (or (if (eq gud-minor-mode 'gdbmi)
                          (unless (gdb-display-source-buffer buffer)
                            (gdb-display-buffer buffer nil 'visible)))
                      (get-buffer-window buffer)
                      (display-buffer buffer))))
     (pos))
    (when buffer
      (with-current-buffer buffer
    (unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
      (if (yes-or-no-p
           (format "File %s changed on disk.  Reread from disk? "
               (buffer-name)))
          (revert-buffer t t)
        (setq gud-keep-buffer t)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (setq pos (point))
      (or gud-overlay-arrow-position
          (setq gud-overlay-arrow-position (make-marker)))
      (set-marker gud-overlay-arrow-position (point) (current-buffer))
      ;; If they turned on hl-line, move the hl-line highlight to
      ;; the arrow's line.
      (when (featurep 'hl-line)
        (cond
         (global-hl-line-mode
          (global-hl-line-highlight))
         ((and hl-line-mode hl-line-sticky-flag)
          (hl-line-highlight)))))
    (cond ((or (< pos (point-min)) (> pos (point-max)))
           (widen)
           (goto-char pos))))
      (when window
    (set-window-point window gud-overlay-arrow-position)
    (if (eq gud-minor-mode 'gdbmi)
        (setq gdb-source-window window))))))

(provide 'memory)
