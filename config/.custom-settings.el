(add-to-list 'auto-mode-alist '("\\.js\\'" . react-mode))

(setq
 treemacs-position 'right
 lsp-ui-doc-enable nil
 lsp-ui-doc-position 'at-point
 tree-sitter-hl-use-font-lock-keywords nil
 evil-emacs-state-cursor '("SeaGreen4" box)
 git-gutter-fr:side 'left-fringe
 evil-normal-state-cursor '("SeaGreen4" box)
 jit-lock-chunk-size 5000
 evil-visual-state-cursor '("cyan" box)
 evil-replace-state-cursor '("red" bar)
 evil-operator-state-cursor '("red" hollow)
 )

(spacemacs/toggle-vi-tilde-fringe-off)
(spacemacs/set-leader-keys-for-major-mode 'elm-mode "fr" 'lsp-ui-peek-find-references)
(spacemacs/set-leader-keys-for-major-mode 'elm-mode "ug" 'lsp-ui-doc-glance)
(spacemacs/set-leader-keys-for-major-mode 'elm-mode "uf" 'lsp-ui-doc-focus-frame)
(spacemacs/set-leader-keys-for-major-mode 'elm-mode "us" 'lsp-ui-doc-show)
(spacemacs/set-leader-keys-for-major-mode 'elm-mode "uh" 'lsp-ui-doc-hide)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "fr" 'lsp-ui-peek-find-references)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "ug" 'lsp-ui-doc-glance)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "uf" 'lsp-ui-doc-focus-frame)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "us" 'lsp-ui-doc-show)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "uh" 'lsp-ui-doc-hide)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode "al" 'lsp-avy-lens)


'(version-control :variables
                  version-control-diff-side 'left
                  version-control-global-margin t)

(require 'dap-node)
(require 'lsp-mode)
(require 'dap-chrome)
(require 'dap-firefox)


;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(defun my-correct-symbol-bounds (pretty-alist)
  "Prepend a TAB character to each symbol in this alist,
  this way compose-region called by prettify-symbols-mode
  will use the correct width of the symbols
  instead of the width measured by char-width."
  (mapcar (lambda (el)
            (setcdr el (string ?\t (cdr el)))
            el)
          pretty-alist))

(defun pretty-lambdas-haskell ()
  (font-lock-add-keywords
   nil `((,(concat "\\(" (regexp-quote "\\") "\\)")
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)
(add-hook 'elm-mode-hook 'pretty-lambdas-haskell)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-tsx-mode))

(defun my-setup-indent (n)
  (setq javascript-indent-level n)
  (setq typescript-indent-level n)
  (setq js-indent-level n)
  (setq web-mode-markup-indent-offset n)
  (setq web-mode-css-indent-offset n)
  (setq web-mode-code-indent-offset n)
  (setq css-indent-offset n)
  (setq groovy-indent-offset n))
(my-setup-indent 2)

