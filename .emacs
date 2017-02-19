;; .emacs
;; (set-face-font 'default "Anonymous Pro-16")

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(backup-directory-alist (quote ((".*" . "/home/memory/.emacs.d/.backup/"))))
 '(before-save-hook (quote (whitespace-cleanup)))
 '(c-basic-offset 4)
 '(c-default-style "bsd")
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(compilation-scroll-output (quote first-error))
 '(compilation-skip-threshold 0)
 '(completion-styles (quote (basic partial-completion emacs22 initials)))
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" default)))
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
 '(electric-pair-pairs (quote ((34 . 34))))
 '(fci-rule-color "#3C3D37")
 '(gdb-many-windows t)
 '(gdb-max-frames 14000)
 '(global-auto-revert-non-file-buffers t)
 '(global-subword-mode nil)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(history-delete-duplicates t)
 '(history-length t)
 '(indent-tabs-mode nil)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (monokai-theme shell-history magit company-c-headers company racer toml-mode cargo rust-mode go-mode flycheck-pyflakes)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(python-indent 4)
 '(python-indent-offset 4)
 '(reb-re-syntax (quote string))
 '(require-final-newline t)
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position nil)
 '(scroll-step 1)
 '(shell-file-name "/bin/zsh")
 '(show-paren-mode t nil (paren))
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar))
 '(truncate-lines t)
 '(undo-limit 1500000)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(vlf-batch-size 10000000)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(whitespace-style
   (quote
    (face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)))
 '(woman-fill-column 170)
 '(woman-use-own-frame nil))

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

(add-to-list 'load-path "~/.emacs.d/lisp/" )
(require 'memory)
(my-common-prog-mode-setup)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
