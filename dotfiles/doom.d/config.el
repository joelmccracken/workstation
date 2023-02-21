;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq no-native-compile t)

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

(setq workstation-config-path
      (concat "~/workstation/hosts/current/config.el"))

(when (file-exists-p workstation-config-path)
  (load workstation-config-path))


(after! org
  (setq org-agenda-custom-commands
      '(("p" "Projects" tags "+CATEGORY=\"PROJ\"+LEVEL=1")
        ("a" "Actions" tags "+TODO=\"TODO\"|+TODO=\"LOOP\"")))

      (setq +org-capture-notes-file "inbox.org")

      (setq org-mobile-files
            '( "~/Dropbox/EF/actions.org"
               "~/Dropbox/EF/projects.org"))

      (setq org-mobile-inbox-for-pull "~/Dropbox/EF/inbox-mobile.org")

      (add-hook 'org-mode-hook 'turn-on-auto-fill))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq global-flycheck-mode nil)

(load-theme 'tsdh-light)


;; (add-to-list 'load-path "~/Projects/shen-elisp/")
;; (require 'shen-elisp)
;; (require 'shen-repl)
;; (setq org-src-preserve-indentation nil)

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
(setq safe-local-variable-values
      '((lsp-haskell-server-path . "~/bin/haskell-language-server-macOS-8.8.4")
        (lsp-haskell-formatting-provider . "stylish-haskell")
        (lsp-enable-file-watchers . t)
        (lsp-file-watch-threshold . 2000)))
