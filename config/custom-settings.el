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

;; magit
(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-tsx-mode))

;; background settings
(set-frame-parameter (selected-frame) 'alpha-background 75)
(add-to-list 'default-frame-alist '(alpha-background . 75))
(add-to-list 'custom-theme-load-path "~/code/emacs-configurations/themes")

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1c1e1f" "#e74c3c" "#b6e63e" "#e2c770" "#268bd2" "#fb2874" "#66d9ef" "#d6d6d4"])
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("3239c17e4d51ef03a0acbfb3fd99649776cb1af593c5081c00a930896a9f5fbd" "0544e5e41f91ba01dea8c0b3f9637729b335e8485ad7631460e903d750f83c04" "a0f80e77781401f4d1c00579fd576ae60aa3ed1db0f6d71a1f7398bbd5c61404" "cdb21b0c55defef8529e4900c162782af869320c18602a70becdb438d6cd43c1" "e67a76b35a37e2ea3e744a7fe26db1073c41f9ba91844503e31cb9cfa3d4eb57" "f6ac740aa14c96a3d6362b052fb3ff7af6539376dad6a779f0c8451552b07056" "c06cd7b25a87903f5ddcc79038541f8557ff4682b5dd7705ea4d0903d24ea7b8" "8304d8508acb462050fbb5dd2160a1bdcdf46b184e8b37f6339bc2a64feb7234" "78c8eab1b64d577954455d5a9dd2b46039635fba971506943a62aa948712e589" "86c52737dc2f8d921ab515a853f21fca61a1cfaee270337c778caa3492303903" "2e4a1b2583d8fd1f02744de86620457f77f807f437f6a9e803642ffb758179ab" "4dab30464cf58c69df4bf1e7dd47331e3fb75c8665ee7eb5de53b30eab07563d" "a6a89db9b77f01b868b298361230e4e01d560c6fbc3834e6c0cac572401a3603" "6bf26d08762f83578333dcda55fe30d1aac0538943f38fbe89f003951d88802b" "1039a4186c932b160e8a29399c0f3bfb056121209fa3919ec89176f74cefac74" "369e4c01a372140327e0a7cd4bc970c60cab8bceb34ea1cfcc0f0fa2b262bbb2" "12005c3234379362a3a0373c9a4147d1485feba6dc2cb6bef1b1e38d24182c3d" "7507e21ceb4f50d920a156f1f81ec3b5f58004fbdd220e953422069dde211ba6" "8d044cdac0ec20b1462003ea0fd345d736e983857456f9f1e5d929ce8ac27527" "74eeb7113d82ce6b99a5fa8d96ad4985ed8be094da3aa8f2dcce48009693c4d6" "9c2af5055d7ce9cf3b7b5fd09207d59d5d3042318dcc6df133e3e01b7ae724cc" "60e2068c6658738d64fb94f611f6b21df9236e06f1d5e0bad791b6a1a2ea53f0" "e9c8d339278e54dc56e8f41823f67e335164d76264d5c16417d29300a3eb956e" "e074b02a4fe54973db6297be69a5e4cb20b117ed13e1d3ef5686a3c49a650c3a" "16dbc7df42eeff70430b43c0741fba3b17ec0244659b034bca7abf8336feecd5" "bc2d780ec8af1310326ab649fb7b382a4248e4d26cef17844f50776c0669c602" "c173b557076fff428c4de77524d63fca8857c24c5b3c82ea3fe6cfb6e56d7f12" "db4f8042a9efbd35715335fc67a76290d54df7c4cff94e5dd9965cc1b0ee5133" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "e4b6f65543c35fc66557349f2568166918a1a4e22fdf425431d6283e48061a36" "4b6ef53e4bcff10618464531efc2abdf05dc4cbace63ae85a17f0268deb59769" "db22fc60de5e54a52a166bad6b29d5b668219cf00713947f07102a543e4a0aaa" "d1fa414dc8607191b558de531133d504781d4a5b5945b91d1b7cb34f528e903a" "effa3c62ca509a1bc6be600c84a977b8a44638116a53ec15c4b6b59aac6ae005" "39f3484306b28ff39279f9524b198428e02d439456e2989f1e9329858042354a" "3186cd14ae57cba267a1b37c9b911e0f65cdda34016dd2d8017615b1e0b0dd07" "e019c876eba9d503c7b7b96c5cd36ad56e66eba5c2253af0bb97e515121b53c0" "e51930b22a28d9ddb988881254d89ab910d880a740df176fe604501834841303" "1323c35a279f2db30e2f39dcc70cf229eb9c6dd4714dfc4295b8204b1d709057" "d47e8b77869a84263ba5345dc1bf1fd9d5dc6a23dff0d9cbb2b950c057834c40" "35fdc964185228c73a71a5d91411af69dcdb65d2cebcf3998a517d2febd81842" "119a80997cb5dc31d35ccc7e808e95ba5b02444e9cf0148ee77a869a4b506474" "292348043edc618e51d38815a3c4512faab79d274539f5cb998b9e83a9f968ab" "fbadefa8bddb2b3eacc13bed24a5dc6715df87dfcebc11ff7555a0579143b497" "0de39f25befa78ef31112e43a02ee1aaf42da8e76b3ffe2248c9f009ae6fbb54" "fed9d02841a7d71fad6243185e723942edc4198494fc685245b0b41571b32147" "5f463b810da0b8e98d3139359e3ddd8ae3dc0987bf15eb4493724759d4edca6b" "9b327f2854ccf542d98143839bc157daf8c4a323512b26e93bf80411401b7043" "0713fbc239090993f78ab07a4a69f66ad54e055e6513475e40420a9937743cfd" "2a44b382e3a902c773245365d0aa47b1cf4bb90746d94afe3cb9ddcc4dd4c0e1" "3a7a3b0691cd983b0efc8d097a8d0e73fdc83ce1fca11403d5bfff41528c69af" "f8edb229b201d4fbc90a0a3d7734550041f7d73fff3c89c942e5601e5c5e0c4b" "543f4f4f74094501bc289fe81357e06434e636480e125f6e88a2291f5f80cb42" "10126a5453a45c2632301a866f3a7ac5bec320cfb432340546bee8c740d009a6" "ce4cf7346bb263d737a9863fe4b0fec0edbff3f001cb8f419ce74733566d1d5a" "37ba344948ae1a80b59755bedfed68a40ff31c86829f02c202fb9681fdad8bdd" "b4e09faeec5a6ca430458087c111629179db8082dcb36c852c377316c0c1460c" "771b9fff56651280555b10537970f810ac088fc61456fc8be510519e0c85cffb" "f732c9e2f326cac7ed30e79da6c16a13457d478f2c099e60a064ae367136d785" "6792307e28d85350a43bea72871726be72f8fa36ac8a4fae04c5640ea8212f23" "597efbb56709c6465138b219bfc7d350272e777045be8db1ba9b9475bd7655df" "ddb7f6a65763b21630dc71ef26522b0c59a45531bd64ef9a1dd0ce097e5a0523" "4733357b94a75984d365e8e1e7d25d1f2b397cc18b03bb31a69aca1f6256a12f" "c811d382e4dcf71176113fd6b4229da2c212ae22be1cbcd29c1aa6e30562425e" "aa4e6e6514286a099f9fbdd46a34e4bb8de7926f57c9c65fc5d8e0bed43dc09f" "03527d027efc3d411586c0bb17f70b2fc0b0da9cdacc8df32a3a52fed755393d" "92207fe8317b585416c51dbfe291cc96fbd5334d8a557ae4c82be54ec7b9eb45" "6ac40477b6315ebe5a503212ffa4c8e30e9c5b38ed9fd4a2d72e75e6e6323dc0" "b7f65b67f1395871b1ac1c8cb762af27edc96bbe042f36e93dbe708ab19ec557" "bab6c42da3891fb81ab7b8c7069d598c25135debb36ff18fd5a26d08dab56e8f" "ec88fd0c87ea75da1579d5e283019c36e355ee4236a2d0770c7b42f885ad22f3" "6854bef7fa3973ef5a8bb6d1d016276496beb79edec2ed1f3f971a5da55caa7f" "90c564f32a2e44f80aa9eaf084c86e8127e656d1aae5e68ac8f9b8f556a5daa9" "511d5293ee7c00672338df10011bd5a45cb42a713dfb4d8701756cc1dfc994c2" "ee5499dc412998c89727ee2f491dd168a01e0ec0a98772eb5bb6b16b54641cd3" "b05a182f12e9f1fd508976e0075f3ca71ca21e6e64f3dca66c1da9ba4f4917db" "ebea9c135d1097babe7870dde2020d5192ed3475e2535bc2801c085c029fde20" "01385032f225a9d014c09bd2ebe725aac451f836bad97d488fde2f17bb1fd9dd" "eae8ad013825489be59b3dd5e5b825c8ed27b544220feea866db73609307557e" "37941207ab90b41a1e05c18a323a52564f9d8a4f8b11e37681f7aea9a5b4d7b5" "f2d9f85296bf0ba3a47a80c93cf6fea13f549dda55224276f8c316b6fdcdfffe" "e91f0f226a294cdf351357dc7473f853eb5b4fc767b2649bd031741ce1b25667" "a007c0d6d1df6e849cba48fb3cfa4b6fdd1565e0199ecda8c4fe4f783a291259" "19f94b19dfd7be11cc1a0b3799edfd75ab9fb7b8ad4f0bd2efbd797d1ddd0e5d" "3cb6521beeab686752293e52406abebf382d6850e965915dc9649e312e5b0318" "5d40cc5a4f9f12212cefceb65cdf238855264645042136feaffb19c33518d02c" "89e58e58d07068b1b762a1a9b7c3ae6c45ada15aaa605b14f6dfc3bd58628837" "20b21e9407a53aa6a5100f1a25619f1b145e92422c8715e75620c73340517aa7" "3148c5860321bd350b86c2c3d13f3daeff6b6574e42fe2d85b30d991e6c155ff" "eb7123d11d0d09dc3877952a6b939e34708ce9dfd79068afcb3efa6e6b0fe1b1" "1528f86ac446db3e41a98df5e20cf7be10495d8e6ffc0f2588e07206e8cbc199" "b5cf24aa6c7d1511b7afb107038b3312b481e41543948047f7d488b14dc96138" "7eff2d774d747dbe4952e0d8045443f52a4f45d8181a79109886fa7b8b44dcb0" "7d717357ea2c1117a050f58f07cdce3fb4ee0e978b6968a3e9ac576fa1a6d32f" "ea0e6722e7144cb8fefcf80be8e87acabde88f7b6fe34b0f15ed93c95ec9b735" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "b375fc54d0c535bddc2b8012870008055bf29d70eea151869e6ad7aaaadb0d24" "d721b328585cd1c9482b00945c8bc01b0951012d0d981ba731625819023c8bde" "c06166af411222b8d7772ba8e52d05c31d4c6f6cc4788b737ae21cdb8ffe66a7" "9bf256e93b77d12ccd1e3213d65310aa1a90bc4be120bf2b39a653573e4671f8" "0faef0ad660f779f76a8f20e0737b4f9093fc271a6111a5935f141ef810b23dc" "0c5512e2958343c0ad4145f16028a69da4a619ce515c96cab8e0b924c029a7c3" "04e240f3ff3db9616c2535c2895cc3e5f88b92baa62c0239e1c2c9c2d84b9b2d" "928ed6d4997ec3cdce10b65c59d0f966a61792a69b84c47155cb5578ce2972be" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "eb122e1df607ee9364c2dfb118ae4715a49f1a9e070b9d2eb033f1cefd50a908" "77c16fe29860c6fc12cde892d6e3051c740a7b3781c988aeb4df9743f4a24e0d" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "b47eca77c785108ab443aea40fbabb2af3e13a3ac8a8537975dee099b866a0f0" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "5e2cdea6453f8963037723ab91c779b203fb201bf5c377094440f0c465d688ec" "afeb7b07dbc1a4cfadb24f3ef6c8cf5e63051bf76411779f03a0fe3aadc07768" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "82ef0ab46e2e421c4bcbc891b9d80d98d090d9a43ae76eb6f199da6a0ce6a348" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "db7f422324a763cfdea47abf0f931461d1493f2ecf8b42be87bbbbbabf287bfe" "a37d20710ab581792b7c9f8a075fcbb775d4ffa6c8bce9137c84951b1b453016" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "a6fc75241bcc7ce6f68dcfd0de2d4c4bd804d0f8cd3a9f08c3a07654160e9abe" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "246cd0eb818bfd347b20fb6365c228fddf24ab7164752afe5e6878cb29b0204e" "3263bd17a7299449e6ffe118f0a14b92373763c4ccb140f4a30c182a85516d7f" "1278c5f263cdb064b5c86ab7aa0a76552082cf0189acf6df17269219ba496053" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "0cd00c17f9c1f408343ac77237efca1e4e335b84406e05221126a6ee7da28971" "4af38f1ae483eb9335402775b02e93a69f31558f73c869af0d2403f1c72d1d33" "3a78cae35163bb71df460ebcfdebf811fd7bc74eaa15428c7e0bccfd4f858d30" "1bddd01e6851f5c4336f7d16c56934513d41cc3d0233863760d1798e74809b4b" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "9f1d0627e756e58e0263fe3f00b16d8f7b2aca0882faacdc20ddd56a95acb7c2" "b494aae329f000b68aa16737ca1de482e239d44da9486e8d45800fd6fd636780" "4699e3a86b1863bbc695236036158d175a81f0f3ea504e2b7c71f8f7025e19e3" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "dc8ad8b5833ae06e373cc3d64be28e67e6c3d084ea5f0e9e77225b3badbec661" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" "d0e5f30ec00345b9a56e5ca4c54c5f942d3fec30e1b32c2f4aca969cd1ac94c2" "a1f89b80b3b5abc1307c069792583cf5c665f50f97631295cf1f8b768afc31ba" "a6d218431bbcbf7a761c7ab202ab41694fb2c192043b76a956bebd2283a489ff" "b2cb63279f3fa10855a9f2f00ef69c896af758a391daf54af118a56b7f54a5c4" "f3be24e783d547598c6343159f57e5720a8afb0bd28ae04e0ac0c417f99f54e5" "d7f48b7745a9af015e944755d0eca33a4acac75f3a242c97400e1f047ee389fb" "b874fdf67db2213ea4708a8543410a975b6d18a89c963559e091b936f50a14d3" "aa2b6bda6a0b1463094de070d2c7ccfcf715b2bcbfaff7e7ad7459b2ccbccd54" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "e11143c2cf0228b322835a569222afeb0f9baa4054da622313e90dce5a9bc1af" "7bddb4b02590c78769e7ccfdd240664cbfd40c15df8236ed7c121a507bd7dbb0" "170c3b5bd56673763566ec81bb1ebbdeb2e146d104708d504cef4812da1aa163" "7450d359a27702b79af095ea07c7525f6df2f710aca81e6b7dfa0a04b3c0e4c6" "9ada38681078bb699b6e689f152b39de0b75882afdbd06176e86705f0112a43e" "a4db08fe7a726ad3e066cf7f8da15376c64ccb5a760a822b051ae1b3f96637d4" "6ac4a80211c6c75f546f283453f1526a47b82806a8f7ac6753b9b562182c1870" "b3b415844825939b7752670f89e53f7c3aa8b2636f3853c97246097847f13f96" "2527fe6f9717e1e72f2f674af3f003f989d329b8a7f03326393a5026b89d02d5" "a7630b96fd9b4a363ded131fd2b225e120df88b39afa3f93f4c0d8afc261baa0" "79b703e8cb6b66c22109862006874b4860c6f23e15dcd19a96fc1a6e350c9bc1" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "1cd4df5762b3041a09609b5fb85933bb3ae71f298c37ba9e14804737e867faf3" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "3a9f65e0004068ecf4cf31f4e68ba49af56993c20258f3a49e06638c825fbfb6" "c560237b7505f67a271def31c706151afd7aa6eba9f69af77ec05bde5408dbcd" "3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "e7ba99d0f4c93b9c5ca0a3f795c155fa29361927cadb99cfce301caf96055dfd" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "087693200c2ed8ca0cf0180bedb97f687d8b3e1898408608b5fdf045eaf6bcae" "e0a6171c0e1bfb4f64e96aff8b9359301ea66f8fa8b7558fc8dfda9c81ffc213" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "5dbc01213de04de5b5a376d1c4c22dd7dc50cd51c87691689da9948003a2188a" "493265c5fbdaa222e753fde3db3ee8e75d474ad4803faded7267c8fb7b82b915" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "43858fe79c7a0bc7dd20f7449f18e24af8e33e142f491e0ea331f52f7c18bcaf" "bd8d9b849da798b827764da5fa1d42a522aa2990a0f035d4281dbff285278167" "ff8d612f4d3949f92df959140a5c2cd3a60987464158d8999a03e1438a9709a9" "a495b335ac5cc5782c6d5c3187e8542818e389ca3fc987b49f0864048d60031f" "0d71f14d28d87f532444ade3eab9c478021cf65820512ad00fc49ae5c1bd59fd" "fafb66fec1e0066adbe6dd0d820b405eed48c5dd3025cdcef4bc48c6fde51d04" "e53fa397313da8acab692ad59191f35a4c2845e9929d0f19f002713ea62e8c4a" "c79d8631f637ea5b1c72a7fc5a0ec5333132d6e5c2a87150f94e626b3df3330d" "93545ceae7a30579448ab7a716e5f310602c509ab4558400833126a3be8661ba" "186d9c5f596fbb57275241a321df68234abf15e09fd185fa93e38cadc258693e" "8074c43cc7dcc50390c38743fe59c4c630dcb86f0b75fc66a8a813571bc00f03" "21eb64081f6a42946cedcfe7dd22f1d3d8def331f499d600efc749c5edb05083" "8f51914ae521b5098f73553928aac732ed6bf83a40990e29b89c4ce752f4227a" "f97aed303b78c21a72244d343672cb0dcd53878a3f11742bd58ac140f831a970" "6068fdcba238093e6ac75fe1503dcda7bc69337d794dd637e30ba0a6a4aad5e3" "bd1f357ab8227587bd0760226818e6c1432e558c39d66a68df5085c9d2ec59b8" "6d7791b323fecc94b94695a8e8c6b09006e6beae7314ccf7bf26b5291ecfa84e" "8838e2c13e4759e444090c16a5ad89e82659c0824917025ada6a71175cb2e5e2" "72a3d43bb51386c0f33410886a7a958ed673537f2c619d602f07c2861cca4e7d" "92084826a5bb2d5f698cb34d921889b3157bbbfa2af4366fc5f73de4950e0c5f" "25dcd3d505594ddc404813bf5c165840288c6faf3118241ba575a50c8946beaf" "a82b4341f4c7a8571525dbafc09f708c65b79b8314ed05c246d622289af54484" "830c3a767b150086d3e07783d8af8d1b0d5dcfb1c03b1929854eaadf36c06446" "d04a5f6d991494aa7810e2daded6ba828640942eebc8717cdfb4d0c1b9d159d8" default))
 '(evil-want-Y-yank-to-eol nil)
 '(exwm-floating-border-color "#323435")
 '(fci-rule-color "#3C3D37" t)
 '(helm-completion-style 'helm)
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
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fd971f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#b6e63e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#525254"))
 '(magit-diff-use-overlays nil)
 '(objed-cursor-color "#e74c3c")
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(package-selected-packages
   '(texfrag auctex company-emoji emoji-cheat-sheet-plus slack emojify circe oauth2 websocket company-nixos-options helm-nixos-options nix-mode nixos-options haskell-emacs flymake-hlint shfmt insert-shebang flycheck-bashate fish-mode company-shell godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc flycheck-golangci-lint company-go go-mode yaml-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode counsel-css company-web web-completion-data rjsx-mode tide typescript-mode mmm-mode markdown-toc lsp-ui lsp-origami origami helm-lsp gh-md xterm-color vterm terminal-here shell-pop multi-term flyspell-correct-helm flyspell-correct eshell-z eshell-prompt-extras esh-help auto-dictionary yasnippet-snippets unfill mwim helm-company helm-c-yasnippet git-gutter-fringe+ fringe-helper git-gutter+ dash fuzzy flycheck-pos-tip pos-tip browse-at-remote auto-yasnippet ac-ispell auto-complete flycheck-elm elm-test-runner elm-mode reformatter lsp-haskell hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell dante lcr haskell-mode company-cabal cmm-mode attrap web-beautify tern prettier-js npm-mode nodejs-repl livid-mode skewer-mode js2-refactor yasnippet multiple-cursors js2-mode js-doc import-js grizzl impatient-mode simple-httpd helm-gtags ggtags dap-mode lsp-treemacs bui lsp-mode dash-functional counsel-gtags counsel swiper ivy company add-node-modules-path treemacs-magit smeargle orgit org-rich-yank org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-cliplink org-brain magit-svn magit-section magit-gitflow magit-popup htmlize helm-org-rifle helm-gitignore helm-git-grep gnuplot gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link forge markdown-mode magit ghub closql emacsql-sqlite emacsql treepy git-commit with-editor transient evil-org ws-butler writeroom-mode winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree treemacs-projectile treemacs-persp treemacs-icons-dired treemacs-evil toc-org symon symbol-overlay string-inflection spaceline-all-the-icons restart-emacs request rainbow-delimiters popwin pcre2el password-generator paradox overseer org-superstar open-junk-file nameless move-text macrostep lorem-ipsum link-hint indent-guide hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org helm-mode-manager helm-make helm-ls-git helm-flx helm-descbinds helm-ag google-translate golden-ratio font-lock+ flycheck-package flycheck-elsa flx-ido fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu emr elisp-slime-nav editorconfig dumb-jump dotenv-mode dired-quick-sort diminish devdocs define-word column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-compile aggressive-indent ace-link ace-jump-helm-line))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (cons "#d6d6d4" "#1c1e1f"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(powerline-default-separator nil)
 '(rustic-ansi-faces
   ["#1c1e1f" "#e74c3c" "#b6e63e" "#e2c770" "#268bd2" "#fb2874" "#66d9ef" "#d6d6d4"])
 '(safe-local-variable-values
   '((setf lsp-haskell-language-server-path "/nix/store/crzk2p5c79c1ym9nlgmafiiiprvw76qc-ghc-8.10.7-with-packages/bin/haskell-language-server-wrapper")
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp)))
 '(standard-indent 2)
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
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
