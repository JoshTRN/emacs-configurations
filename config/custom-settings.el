(setq evil-emacs-state-cursor '("SeaGreen4" box)
      evil-normal-state-cursor '("SeaGreen4" box)
      evil-operator-state-cursor '("red" hollow)
      evil-replace-state-cursor '("red" bar)
      evil-visual-state-cursor '("cyan" box)
      git-gutter-fr:side 'left-fringe
      helm-swoop-speed-or-color t
      jit-lock-chunk-size 5000
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

(setq-default fill-column 110
              highlight-indent-guides-character 9615
              visual-fill-column-center-text t
              visual-fill-column-width 110
              valign-fancy-bar 't)

(defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)))

(defun my-correct-symbol-bounds (pretty-alist)
  "Prepend a TAB character to each symbol in this alist,
  this way compose-region called by prettify-symbols-mode
  will use the correct width of the symbols
  instead of the width measured by char-width."
  (mapcar #'(lambda (el)
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
  (cl-letf (((symbol-function 'y-or-n-p) #'(lambda (prompt) t)))
    (apply orig-fun args)))

(defun evil-jump-backward-centered (&optional count)
  (interactive)
  (evil-jump-backward count)
  (evil-scroll-line-to-center count))

(defun evil-search-next-centered (&optional count)
  (interactive)
  (evil-ex-search-next)
  (evil-scroll-line-to-center count))

(defun evil-search-previous-centered (&optional count)
  (interactive)
  (evil-ex-search-previous)
  (evil-scroll-line-to-center count))

(defun evil-jump-forward-centered (&optional count)
  (interactive)
  (evil-jump-forward count)
  (evil-scroll-line-to-center count))

(defun evil-scroll-down-centered (&optional count)
  (interactive)
  (evil-scroll-down count)
  (evil-scroll-line-to-center count))

(defun evil-scroll-up-centered (&optional count)
  (interactive)
  (evil-scroll-up count)
  (evil-scroll-line-to-center count))


(defun my-setup-indent (n)
  (setq javascript-indent-level n)
  (setq typescript-indent-level n)
  (setq js-indent-level n)
  (setq c-basic-offset n)
  (setq web-mode-markup-indent-offset n)
  (setq web-mode-css-indent-offset n)
  (setq web-mode-code-indent-offset n)
  (setq css-indent-offset n)
  (setq groovy-indent-offset n))

(defun pretty-print ()
  (interactive)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun pretty-print-header ()
  (pretty-print)
  (setq-local face-remapping-alist '((header-line (:height 4.0) variable-pitch)))
  (setq header-line-format " "))

(defun my-yaml-hook ()
  (spacemacs/toggle-absolute-line-numbers-on)
  (highlight-indentation-mode 1))

(defun my-org-mode-hook ()
  (setq org-hide-leading-stars nil)
  (setq org-indent-mode-turns-on-hiding-stars t)
  (pretty-print-header)
  (org-superstar-mode)
  (setq org-hide-emphasis-markers t)
  (setq global-hl-line-mode nil)
  (org-indent-mode)
  (solaire-mode -1))

(defun my-markdown-mode-hook ()
  (pretty-print-header)
  (setq global-hl-line-mode nil)
  (markdown-toggle-markup-hiding 1)
  (solaire-mode -1))

(defun kube-hook ()
  (pretty-print-header)
  (solaire-mode -1))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))


(require 'dap-node)
(require 'dap-chrome)
(require 'dap-firefox)

(remove-hook 'org-present-mode-hook 'spacemacs//org-present-start)

(add-hook 'markdown-mode-hook #'my-markdown-mode-hook)
(add-hook 'help-mode-hook #'pretty-print)
(add-hook 'lsp-help-mode-hook #'pretty-print)
(add-hook 'yaml-mode-hook #'my-yaml-hook)
(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)
(add-hook 'elm-mode-hook #'pretty-lambdas-haskell)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'eww-mode-hook #'pretty-print)
(add-hook 'org-mode-hook #'my-org-mode-hook)
(add-hook 'kubernetes-overview-mode-hook 'kube-hook)
(add-hook 'js-mode-hook #'(lambda () (buffer-face-set :foreground "white")))

(add-to-list 'auto-mode-alist '("\\.js\\'" . react-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-tsx-mode))
(add-to-list 'custom-theme-load-path "~/code/emacs-configurations/themes")

(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-ex-search-next>") 'evil-search-next-centered )
(define-key evil-motion-state-map (kbd "<remap> <evil-ex-search-previous>") 'evil-search-previous-centered)

(define-key evil-motion-state-map (kbd "<remap> <evil-jump-backward>") 'evil-jump-backward-centered)
(define-key evil-motion-state-map (kbd "<remap> <evil-jump-forward>") 'evil-jump-forward-centered)

(define-key evil-motion-state-map (kbd "<remap> <evil-scroll-down>") 'evil-scroll-down-centered)
(define-key evil-motion-state-map (kbd "<remap> <evil-scroll-up>") 'evil-scroll-up-centered)

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

(define-derived-mode ts-mode typescript-mode "ts"
  "Major mode for editing ts code blocks.")

(defun org-babel-execute:typescript (body params)
  (let ((org-babel-js-cmd "npx ts-node < "))
    (org-babel-execute:js body params)))

(defalias 'org-babel-execute:ts 'org-babel-execute:typescript)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python  . t)
   (java    . t)
   (http    . t)
   (latex   . t)
   (ditaa   . t)
   (shell   . t)
   (haskell . t)
   (sql     . t)
   (js      . t)))

(my-setup-indent 2)

(helm-ff-icon-mode)

(spacemacs/toggle-vi-tilde-fringe-off)

(solaire-global-mode +1)

(pixel-scroll-precision-mode)

(make-variable-buffer-local 'global-hl-line-mode)

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-c b") 'org-babel-tangle-block)))

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               #'(lambda ()
                 (org-display-inline-images)
                 (evil-define-key 'normal org-present-mode-keymap
                   (kbd "<left>")  'org-present-prev
                   (kbd "<right>") 'org-present-next
                   "q"             'org-present-quit)
                 (text-scale-set 2)
                 (setq visual-fill-column-width 50)
                 (funcall #'(lambda ()
                           (message "setting org shift tab")
                           (org-shifttab)
                           ))
                 ))
     (add-hook 'org-present-mode-quit-hook
               #'(lambda ()
                 (org-remove-inline-images)
                 (text-scale-set 1)
                 (setq visual-fill-column-width 100)
                 ))))
