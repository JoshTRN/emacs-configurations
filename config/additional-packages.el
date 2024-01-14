
(setq custom-additional-packages
      '(
        helm-posframe
        org-modern
        (ob-kubectl :location (recipe
                               :fetcher github
                               :repo "ifitzpat/ob-kubectl"))
        ob-rust
        ob-http
        exec-path-from-shell
        ox-timeline
        indium
        js-comint
        rjsx-mode
        solaire-mode
        yasnippet-snippets
        direnv
        magit-delta
        prettier-js
        quelpa
        ))
