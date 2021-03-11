;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Joel McCracken"
      user-mail-address "mccraken.joel@gmail.com")

(setq haskell-stylish-on-save t)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/Dropbox/EF/")
(setq org-roam-directory "~/Dropbox/EF/")
(setq org-roam-db-location "~/Dropbox/EF/org-roam.db")
(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
    '(("d" "default" entry
       #'org-roam-capture--get-point
       "* %?"
       :file-name "daily/%<%Y-%m-%d>"
       :head "#+title: %<%Y-%m-%d>\n\n")))

(setq org-archive-location "~/Dropbox/EF/archive::* From %s")

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(use-package! org
  :config
  (add-hook 'org-mode-hook 'auto-fill-mode)
)


(setq custom-file "~/workstation/custom.el")


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq global-flycheck-mode nil)

(use-package direnv
  :config (direnv-mode))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21242b" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(custom-safe-themes
   (quote
    ("2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" default)))
 '(fci-rule-color "#37474F")
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#237AD3"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#579C4C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(objed-cursor-color "#D16969")
 '(pdf-view-midnight-colors (cons "#d4d4d4" "#1e1e1e"))
 '(rustic-ansi-faces
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")
     (eval progn
           (let*
               ((load-path
                 (cons
                  (locate-dominating-file default-directory "ef.el")
                  load-path)))
             (require
              (quote ef))))
     (ef/files "actions.org" "projects-maintenance.org" "projects.org" "upcoming.org" "waiting.org"))))
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
