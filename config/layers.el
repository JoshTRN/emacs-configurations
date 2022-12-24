(setq custom-layers '(
                      csv
                      (auto-completion :variables
                                       auto-completion-enable-snippets-in-popup t)
                      better-defaults
                      dap
                      debug
                      docker
                      eaf
                      elm
                      (elm :variables
                           elm-format-on-save t
                           elm-sort-imports-on-save t
                           )
                      unicode-fonts
                      (unicode-fonts :variables unicode-fonts-enable-ligatures t)
                      emacs-lisp
                      eww
                      pandoc
                      (vue :variables vue-backend 'lsp)
                      (git :variables git-enable-magit-delta-plugin t)
                      (node :variables node-add-modules-path t)
                      (go :variables go-backend 'lsp
                          go-tab-width 2)
                      tree-sitter
                      html
                      (mu4e :variables
                            mu4e-installation-path "/usr/share/emacs/site-lisp")
                      haskell
                      helm
                      java
                      javascript
                      (javascript :variables js2-mode-show-strict-warnings nil)
                      kubernetes
                      lsp
                      markdown
                      multiple-cursors
                      nixos
                      vimscript
                      (org :variables
                           org-enable-verb-support t
                           org-enable-transclusion-support t
                           org-enable-appear-support t
                           org-enable-valign t
                           org-enable-epub-support t
                           org-enable-roam-protocol t
                           org-enable-roam-ui t
                           org-enable-roam-support t)
                      prettier
                      search-engine
                      shell-scripts
                      (shell :variables
                             shell-default-height 30
                             shell-default-position 'bottom
                             shell-default-shell 'multi-vterm)
                      slack
                      spell-checking
                      syntax-checking
                      (typescript :variables
                                  javascript-backend 'lsp
                                  typescript-fmt-tool 'lsp
                                  typescript-fmt-on-save t
                                  typescript-backend 'lsp
                                  typescript-tsx-mode t
                                  typescript-linter 'tslint)
                      terraform
                      themes-megapack
                      treemacs
                      (version-control :variables
                                       version-control-diff-side 'left
                                       version-control-global-margin t)
                      latex
                      yaml
                      ))
