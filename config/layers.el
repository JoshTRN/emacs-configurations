(setq custom-layers '(
                      lua
                      rust
                      csv
                      (auto-completion :variables auto-completion-enable-snippets-in-popup t)
                      better-defaults
                      csv
                      dap
                      debug
                      docker
                      eaf
                      (elm :variables
                           elm-format-on-save t
                           elm-sort-imports-on-save t)
                      emacs-lisp
                      eww
                      (git :variables git-enable-magit-delta-plugin t)
                      (go :variables
                          go-backend 'lsp
                          go-tab-width 2)
                      haskell
                      (helm :variables helm-position 'top)
                      html
                      java
                      (javascript :variables js2-mode-show-strict-warnings nil)
                      kubernetes
                      (lsp :variables lsp-lens-enable t)
                      markdown
                      multiple-cursors
                      nixos
                      (node :variables node-add-modules-path t)
                      (org :variables
                           org-enable-appear-support t
                           org-enable-epub-support t
                           org-enable-roam-protocol t
                           org-enable-roam-support t
                           org-enable-roam-ui t
                           org-enable-transclusion-support t
                           org-enable-valign t
                           org-enable-org-journal-support t
                           org-enable-verb-support t)
                      pandoc
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
                      terraform
                      themes-megapack
                      treemacs
                      tree-sitter
                      (typescript :variables
                                  javascript-backend 'lsp
                                  typescript-backend 'lsp
                                  typescript-fmt-on-save t
                                  typescript-fmt-tool 'lsp
                                  typescript-linter 'tslint
                                  typescript-tsx-mode t)
                      (unicode-fonts :variables unicode-fonts-enable-ligatures t)
                      (version-control :variables
                                       version-control-diff-side 'left
                                       version-control-global-margin t)
                      vimscript
                      (vue :variables vue-backend 'lsp)
                      yaml))
