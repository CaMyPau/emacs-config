;; .emacs
;; (set-face-font 'default "Anonymous Pro-32")
;; (set-default-font "Anonymous Pro-16")
;; (set-face-font 'default "Anonymous Pro-16")

(setq default-frame-alist '((font . "Anonymous Pro-16")))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

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
   '("d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" "9abe2b502db3ed511fea7ab84b62096ba15a3a71cdb106fd989afa179ff8ab8d" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "d3a406c5905923546d8a3ad0164a266deaf451856eca5f21b36594ffcb08413a" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" default))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(desktop-path '("~/.emacs.d/"))
 '(desktop-save-mode t)
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-mode t)
 '(electric-pair-pairs '((34 . 34)))
 '(fci-rule-color "#3C3D37")
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
 '(magit-diff-use-overlays nil)
 '(magit-git-executable "/opt/bb/bin/git")
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(smart-tabs-mode dockerfile-mode yaml-mode keychain-environment goto-last-change flycheck-rust markdown-mode monokai-theme shell-history magit company-c-headers company racer toml-mode cargo rust-mode go-mode flycheck-pyflakes))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(python-environment-virtualenv
   '("virtualenv" "--system-site-packages" "--quiet" "--python" "python3"))
 '(python-indent 4)
 '(python-indent-offset 4)
 '(python-shell-interpreter "python3")
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
 '(tool-bar-mode nil nil (tool-bar))
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

(smart-tabs-insinuate 'c++)
