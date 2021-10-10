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
       "* %?"
       :if-new
       (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n\n" ("")))))

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(after! org
  (add-hook 'org-mode-hook 'auto-fill-mode))

(setq custom-file "~/.doom.d/custom.el")
(let
  ((this-machine (expand-file-name "~/var/this-machine.el")))
    (when (file-exists-p this-machine)
      (load this-machine)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq global-flycheck-mode nil)

(load-theme 'tsdh-light)

;; (use-package direnv
;;   :config (direnv-mode))

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
