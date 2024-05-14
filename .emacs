;; .emacs

;; (set-face-font 'default "Inconsolata-12")

(require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

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
 '(auth-source-save-behavior nil)
 '(backup-directory-alist '((".*" . "~/.emacs.d/.backup/")))
 '(before-save-hook '(whitespace-cleanup))
 '(c-basic-offset 4)
 '(c-default-style "bsd")
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(compilation-scroll-output 'first-error)
 '(compilation-skip-threshold 0)
 '(completion-styles '(basic partial-completion emacs22 initials))
 '(cursor-type 'bar)
 '(custom-enabled-themes '(monokai))
 '(custom-safe-themes
   '("95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692" default))
 '(default-frame-alist '((font . "Inconsolata-12")))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(desktop-path '("~/.emacs.d/"))
 '(desktop-save-mode t)
 '(diff-switches "-u")
 '(directory-abbrev-alist '(("/source" . "/home/asabinin/src")))
 '(dired-listing-switches "-alh")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-mode t)
 '(electric-pair-pairs '((34 . 34)))
 '(gdb-many-windows t)
 '(gdb-max-frames 14000)
 '(global-auto-revert-non-file-buffers t)
 '(global-subword-mode nil)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(history-delete-duplicates t)
 '(history-length t)
 '(indent-tabs-mode nil)
 '(lsp-file-watch-threshold 3000)
 '(lsp-keymap-prefix "C-l")
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(fireplace cmake-mode flycheck company yaml-mode smart-tabs-mode monokai-theme magit lsp-ui lsp-pyright keychain-environment goto-last-change dockerfile-mode))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(python-environment-virtualenv
   '("virtualenv" "--system-site-packages" "--quiet" "--python" "python3"))
 '(python-indent 4)
 '(python-indent-offset 4)
 '(reb-re-syntax 'string)
 '(require-final-newline t)
 '(save-place-mode t)
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position nil)
 '(scroll-step 1)
 '(shell-file-name "/bin/zsh")
 '(show-paren-mode t nil (paren))
 '(tab-width 4)
 '(term-mode-hook '(compilation-shell-minor-mode compilation-minor-mode))
 '(tool-bar-mode nil nil (tool-bar))
 '(tramp-histfile-override "/dev/null")
 '(truncate-lines t)
 '(undo-limit 1500000)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
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
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(vlf-batch-size 10000000)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(whitespace-style
   '(face trailing tabs spaces newline empty indentation space-before-tab space-mark tab-mark newline-mark))
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
(require 'package-config)
(require 'memory)
(my-common-prog-mode-setup)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-space ((t (:foreground "darkgray"))))
 '(whitespace-tab ((t (:foreground "darkgray")))))

;; Add color formatting to *compilation* buffer
(add-hook 'compilation-filter-hook
  (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

(setq-default gdb-display-io-nopopup t)

(keychain-refresh-environment)
