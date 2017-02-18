;; .emacs
;; (set-face-font 'default "Anonymous Pro-16")


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/" )

(require 'memory)
(my-common-prog-mode-setup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "/home/memory/.emacs.d/.backup/"))))
 '(before-save-hook (quote (whitespace-cleanup)))
 '(c-basic-offset 4)
 '(c-default-style "bsd")
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(compilation-skip-threshold 0)
 '(completion-styles (quote (basic partial-completion emacs22 initials)))
 '(default-frame-alist
    (quote
     ((font . "Anonymous Pro-16")
      (background-color . "gray12")
      (foreground-color . "gray90")
      (tool-bar-lines . 0)
      (menu-bar-lines . 0)
      (cursor-color . "#ffffff"))))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(desktop-path (quote ("~/.emacs.d/")))
 '(desktop-save-mode t)
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-mode t)
 '(electric-pair-pairs (quote ((34 . 34) (60 . 62))))
 '(gdb-many-windows t)
 '(gdb-max-frames 14000)
 '(global-auto-revert-non-file-buffers t)
 '(global-subword-mode nil)
 '(history-delete-duplicates t)
 '(history-length t)
 '(indent-tabs-mode nil)
 '(iswitchb-mode t)
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(package-selected-packages
   (quote
    (magit smartparens company-c-headers company racer toml-mode cargo rust-mode go-mode flycheck-pyflakes)))
 '(python-indent 4)
 '(python-indent-offset 4)
 '(reb-re-syntax (quote string))
 '(require-final-newline t)
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position nil)
 '(scroll-step 1)
 '(semanticdb-default-save-directory "/home/memory/.emacs.d/.semanticdb")
 '(shell-file-name "/bin/zsh")
 '(show-paren-mode t nil (paren))
 '(srecode-map-save-file "/home/memory/.emacs.d/.srecode/srecode-map")
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar))
 '(truncate-lines t)
 '(undo-limit 1500000)
 '(vlf-batch-size 10000000)
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)))
 '(woman-fill-column 170)
 '(woman-use-own-frame nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-empty ((t (:foreground "firebrick"))))
 '(whitespace-indentation ((t (:foreground "firebrick"))))
 '(whitespace-space ((((class color) (background dark)) (:foreground "grey45"))))
 '(whitespace-space-after-tab ((t (:foreground "firebrick"))))
 '(whitespace-trailing ((t (:background "red1" :foreground "yellow" :weight extra-bold)))))

( global-set-key ( kbd "<f2>"    ) 'switch-to-other-buffer     )
( global-set-key ( kbd "<f5>"    ) 'gud-cont                   )
( global-set-key ( kbd "S-<f5>"  ) 'comint-interrupt-subjob    )
( global-set-key ( kbd "<f6>"    ) 'gud-until                  )
( global-set-key ( kbd "<f7>"    ) 'gud-step                   )
( global-set-key ( kbd "S-<f7>"  ) 'gud-finish                 )
( global-set-key ( kbd "<f8>"    ) 'gud-next                   )
( global-set-key ( kbd "<f9>"    ) 'recompile                  )
( global-set-key ( kbd "M-<f9>"  ) 'compile                    )
( global-set-key ( kbd "<f11>"   ) 'shell                      )
( global-set-key ( kbd "<home>"  ) 'my-smart-beginning-of-line )
( global-set-key ( kbd "C-+"     ) 'goto-last-change           )
( global-set-key ( kbd "M-<tab>" ) 'my-swap-buffers            )
( global-set-key ( kbd "C-<tab>" ) 'ff-find-other-file         )

( global-unset-key ( kbd "C-z" ) )

; Auto Mode
( add-to-list 'auto-mode-alist '( "\\.yaml$"    . yaml-mode                       ) )
( add-to-list 'auto-mode-alist '( "\\.h.*$"     . c++-mode                        ) )
( add-to-list 'auto-mode-alist '( "\\.md$"      . markdown-mode                   ) )
( add-to-list 'auto-mode-alist '( "\\.cql$"     . sql-highlight-postgres-keywords ) )
( add-to-list 'auto-mode-alist '( "\\.m$"       . octave-mode                     ) )
( add-to-list 'auto-mode-alist '( "\\.tac$"     . python-mode                     ) )
( add-to-list 'auto-mode-alist '( "\\.tap$"     . python-mode                     ) )
( add-to-list 'auto-mode-alist '( "\\.service$" . conf-mode                       ) )

(fset 'yes-or-no-p 'y-or-n-p)        ; be shorter
(windmove-default-keybindings 'meta) ; cycle through windows via M + / arrow keys /

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip") ) )

(add-hook 'python-mode-hook
    (function (lambda ()
        (setq indent-tabs-mode nil
            tab-width 4))))

(setq-default cursor-type 'bar)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
