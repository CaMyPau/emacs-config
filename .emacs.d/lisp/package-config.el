;; (use-package lsp-mode
;;   :ensure t
;;   :defer t
;;   :hook (lsp-mode . (lambda ()
;;                       (let ((lsp-keymap-prefix "C-l"))
;;                         (lsp-enable-which-key-integration))))
;;   :init
;;   (setq lsp-keep-workspace-alive nil
;;         lsp-signature-doc-lines 5
;;         lsp-idle-delay 0.5
;;         lsp-prefer-capf t
;;         lsp-client-packages nil)
;;   :config
;;   (define-key lsp-mode-map (kbd "C-l") lsp-command-map))

;; (use-package docker-tramp
;;   :ensure t)

;; (use-package smart-tabs-mode
;;   :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package flymake
  :ensure t)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package lsp-ui
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package keychain-environment
  :ensure t)

(use-package goto-last-change
  :ensure t)

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package monokai-theme
  :ensure t)

(provide 'package-config)
