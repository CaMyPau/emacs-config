(require 'memory)

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'my-prog-mode)

(add-hook 'rust-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'rust-format-buffer)))

(setq racer-cmd "/home/memory/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "/home/memory/src/rust-src/src") ;; Rust source code PATH

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'flycheck-mode)

(provide 'rust)
