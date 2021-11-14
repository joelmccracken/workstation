(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282c34" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(custom-safe-themes
   '("a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "0b3aee906629ac7c3bd994914bf252cf92f7a8b0baa6d94cb4dfacbd4068391d" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" default))
 '(exwm-floating-border-color "#c2c6cb")
 '(fci-rule-color "#37474F")
 '(highlight-tail-colors ((("#d6dfdf") . 0) (("#d3dfe9") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#237AD3"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#579C4C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(objed-cursor-color "#D16969")
 '(pdf-view-midnight-colors (cons "#d4d4d4" "#1e1e1e"))
 '(rustic-ansi-faces
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (org-element-map . defun)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (->> . 1)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (lsp-haskell-formatting-provider . "stylish-haskell")
     (lsp-haskell-server-path . "~/bin/haskell-language-server-macOS-8.8.4")
     (jnm/in-ef-dir . t)
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")
     (eval progn
           (let*
               ((load-path
                 (cons
                  (locate-dominating-file default-directory "ef.el")
                  load-path)))
             (require 'ef)))
     (ef/files "actions.org" "projects-maintenance.org" "projects.org" "upcoming.org" "waiting.org")))
 '(vc-annotate-background "#1e1e1e")
 '(vc-annotate-color-map
   (list
    (cons 20 "#579C4C")
    (cons 40 "#81a65c")
    (cons 60 "#acb06c")
    (cons 80 "#D7BA7D")
    (cons 100 "#d8ab79")
    (cons 120 "#d99c76")
    (cons 140 "#DB8E73")
    (cons 160 "#d38b8c")
    (cons 180 "#cc88a6")
    (cons 200 "#C586C0")
    (cons 220 "#c97ca3")
    (cons 240 "#cd7286")
    (cons 260 "#D16969")
    (cons 280 "#ba6c6c")
    (cons 300 "#a37070")
    (cons 320 "#8d7374")
    (cons 340 "#37474F")
    (cons 360 "#37474F")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
