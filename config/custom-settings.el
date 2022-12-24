(setq evil-emacs-state-cursor '("SeaGreen4" box)
      evil-normal-state-cursor '("SeaGreen4" box)
      evil-operator-state-cursor '("red" hollow)
      evil-replace-state-cursor '("red" bar)
      evil-visual-state-cursor '("cyan" box)
      git-gutter-fr:side 'left-fringe
      helm-swoop-speed-or-color t
      jit-lock-chunk-size 5000
      lsp-ui-doc-enable nil
      lsp-ui-doc-position 'at-point
      org-confirm-babel-evaluate nil
      org-ellipsis " ⤵"
      org-roam-directory "~/org-roam"
      org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")
      tab-width 2
      tree-sitter-hl-use-font-lock-keywords nil
      treemacs-position 'right
      treemacs-width 50
      vterm-max-scrollback 100000
      vterm-timer-delay 0.01)

(setq-default visual-fill-column-center-text t
              visual-fill-column-width 110
              valign-fancy-bar 't
              fill-column 110)

(defun pretty-print ()
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun pretty-print-header ()
  (pretty-print)
  (setq-local face-remapping-alist '((header-line (:height 4.0) variable-pitch)))
  (setq header-line-format " "))

(defun my-yaml-hook ()
  (spacemacs/toggle-absolute-line-numbers-on)
  (highlight-indentation-mode 1))

(defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

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

(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))


(defun my-setup-indent (n)
  (setq javascript-indent-level n)
  (setq typescript-indent-level n)
  (setq js-indent-level n)
  (setq web-mode-markup-indent-offset n)
  (setq web-mode-css-indent-offset n)
  (setq web-mode-code-indent-offset n)
  (setq css-indent-offset n)
  (setq groovy-indent-offset n))

(defun my-org-mode-hook ()
  (setq org-hide-leading-stars nil)
  (setq org-indent-mode-turns-on-hiding-stars t)
  (pretty-print-header)
  (org-superstar-mode)
  (setq org-hide-emphasis-markers t)
  (setq global-hl-line-mode nil)
  (org-indent-mode)
  (solaire-mode -1))

(defun kube-hook ()
  (pretty-print-header)
  (solaire-mode -1))

(require 'dap-node)
(require 'dap-chrome)
(require 'dap-firefox)

(add-hook 'markdown-mode-hook 'pretty-print-header)
(add-hook 'help-mode-hook 'visual-line-mode)
(add-hook 'help-mode-hook 'visual-fill-column-mode)
(add-hook 'yaml-mode-hook 'my-yaml-hook)
(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)
(add-hook 'elm-mode-hook 'pretty-lambdas-haskell)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'eww-mode-hook 'pretty-print)
(add-hook 'org-mode-hook 'my-org-mode-hook)
(add-hook 'kubernetes-overview-mode-hook 'kube-hook)

(add-to-list 'auto-mode-alist '("\\.js\\'" . react-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-tsx-mode))
(add-to-list 'custom-theme-load-path "~/code/emacs-configurations/themes")

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

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

(spacemacs/set-leader-keys "gn" 'git-gutter:next-hunk)
(spacemacs/set-leader-keys "gp" 'git-gutter:previous-hunk)

(spacemacs/declare-prefix "m" "Markdown")
(spacemacs/set-leader-keys "ml" 'lsp-ui-doc--open-markdown-link)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (java   . t)
   (latex  . t)
   (ditaa  . t)
   (shell  . t)
   (js     . t)))

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-c b") 'org-babel-tangle-block)))

(my-setup-indent 2)

(spacemacs/toggle-vi-tilde-fringe-off)

(solaire-global-mode +1)

(make-variable-buffer-local 'global-hl-line-mode)

(advice-add 'ediff-quit :around #'disable-y-or-n-p)
