;; If you are distributing this theme, please replace this comment
;; with the appropriate license attributing the original VS Code
;; theme author.

(deftheme obscurii "A nice dark theme.")


(let (
      (color0 "#031417")
      (color1 "#b2cacd")
      (color2 "#17282b")
      (color3 "#85efff")
      (color4 "#304144")
      (color5 "#1c2d30")
      (color6 "#2ab3c5")
      (color7 "#2b3c3f")
      (color8 "#3ec7d9")
      (color9 "#ebfdff")
      (color10 "#4e5f62")
      (color11 "#49d6e9")
      (color12 "#d5971a")
      (color13 "#16a3b6")
      (color14 "#49e9a6")
      (color15 "#49ace9")
      (color16 "#e4b781")
      (color17 "#4e6b6e")
      (color18 "#122326")
      (color19 "#c1d9dc")
      (color20 "#0e1f22")
      (color21 "#bdd5d8"))


  (custom-theme-set-faces
   'obscurii


   ;; BASIC FACES
   `(default ((t (:background ,color0 :foreground ,color1 ))))
   `(hl-line ((t (:background ,color2 ))))
   `(cursor ((t (:foreground ,color3 ))))
   `(region ((t (:background ,color4 ))))
   `(secondary-selection ((t (:background ,color5 ))))
   `(fringe ((t (:background ,color0 ))))
   `(mode-line-inactive ((t (:background ,color2 :foreground ,color6 ))))
   `(mode-line ((t (:background ,color7 :foreground ,color8 ))))
   `(minibuffer-prompt ((t (:background ,color0 :foreground ,color9 ))))
   `(vertical-border ((t (:foreground ,color10 ))))


   ;; FONT LOCK FACES
   `(font-lock-builtin-face ((t (:foreground ,color11 ))))
   `(font-lock-comment-face ((t (:fontStyle :italic t ))))
   `(font-lock-constant-face ((t (:foreground ,color12 ))))
   `(font-lock-function-name-face ((t (:foreground ,color13 ))))
   `(font-lock-keyword-face ((t (:fontStyle :bold t ))))
   `(font-lock-string-face ((t (:foreground ,color14 ))))
   `(font-lock-type-face ((t (:foreground ,color15 ))))
   `(font-lock-variable-name-face ((t (:foreground ,color16 ))))


   ;; linum-mode
   `(linum ((t (:foreground ,color17 ))))
   `(linum-relative-current-face ((t (:foreground ,color17 ))))


   ;; display-line-number-mode
   `(line-number ((t (:foreground ,color17 ))))
   `(line-number-current-line ((t (:foreground ,color17 ))))


   ;; THIRD PARTY PACKAGE FACES


   ;; doom-modeline-mode
   `(doom-modeline-bar ((t (:background ,color7 :foreground ,color8 ))))
   `(doom-modeline-inactive-bar ((t (:background ,color2 :foreground ,color6 ))))


   ;; web-mode
   `(web-mode-string-face ((t (:foreground ,color14 ))))
   `(web-mode-html-attr-name-face ((t (:foreground ,color15 ))))


   ;; company-mode
   `(company-tooltip ((t (:background ,color18 :foreground ,color19 ))))


   ;; org-mode
   `(org-block ((t (:background ,color20 :foreground ,color21 ))))))


(custom-theme-set-variables
 'obscurii
 '(linum-format " %3i "))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


;;;###autoload
(defun obscurii-theme()
  "Apply the obscurii-theme."
  (interactive)
  (load-theme 'obscurii t))


(provide-theme 'obscurii)


;; Local Variables:
;; no-byte-compile: t
;; End:


;; Generated using https://github.com/nice/themeforge
;; Feel free to remove the above URL and this line.
