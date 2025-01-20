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

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/freckle/notes/")
(setq org-directory "/Volumes/Personal/EF")

(setq workstation-config-path
      (concat (or (getenv "WORKSTATION_DIR")
                  "~/workstation")
              "/hosts/current/config.el"))

(when (file-exists-p workstation-config-path)
  (load workstation-config-path))

(defun jnm/ef-ssh-belthronding ()
  "open EF via ssh on belthronding"
  (interactive)
  ;; (find-file "/ssh:joel@belthronding.wildkraken.monster:~/EF/")
  (find-file "/ssh:joel@137.184.110.5:~/EF/") ;;
  )

(map! "C-c e" #'jnm/ef-ssh-belthronding)
(map! "C-c d" #'org-timestamp-inactive)

;; (map! :leader "SPC"
;;       :n "SPC SPC e" #'jnm/ef-ssh-belthronding)

(after! +popup
  ;; added here to change the behavior of *info* buffers (set :quit to nil)
  (set-popup-rules!
    '(("^\\*Completions" :ignore t)
      ("^\\*Local variables\\*$"
       :vslot -1 :slot 1 :size +popup-shrink-to-fit)
      ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\|Messages\\)"
       :vslot -2 :size 0.3  :autosave t :quit t :ttl nil)
      ("^\\*\\(?:doom \\|Pp E\\)"  ; transient buffers (no interaction required)
       :vslot -3 :size +popup-shrink-to-fit :autosave t :select ignore :quit t :ttl 0)
      ("^\\*doom:"  ; editing buffers (interaction required)
       :vslot -4 :size 0.35 :autosave t :select t :modeline t :quit nil :ttl t)
      ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"  ; editing buffers (interaction required)
       :vslot -5 :size 0.35 :select t :modeline nil :quit nil :ttl nil)
      ("^\\*\\(?:Wo\\)?Man "
       :vslot -6 :size 0.45 :select t :quit t :ttl 0)
      ("^\\*Calc"
       :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0)
      ("^\\*Customize"
       :slot 2 :side right :size 0.5 :select t :quit nil)
      ("^ \\*undo-tree\\*"
       :slot 2 :side left :size 20 :select t :quit t)
      ;; `help-mode', `helpful-mode'
      ("^\\*\\([Hh]elp\\|Apropos\\)"
       :slot 2 :vslot -8 :size 0.42 :select t)
      ("^\\*eww\\*"  ; `eww' (and used by dash docsets)
       :vslot -11 :size 0.35 :select t)
      ("^\\*xwidget"
       :vslot -11 :size 0.35 :select nil)
      ("^\\*info\\*$"  ; `Info-mode'
       :slot 2 :vslot 2 :size 0.45 :select t :quit nil))
    '(("^\\*Warnings" :vslot 99 :size 0.25)
      ("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)
      ("^\\*CPU-Profiler-Report "    :side bottom :vslot 100 :slot 1 :height 0.4 :width 0.5 :quit nil)
      ("^\\*Memory-Profiler-Report " :side bottom :vslot 100 :slot 2 :height 0.4 :width 0.5 :quit nil)
      ("^\\*Process List\\*" :side bottom :vslot 101 :size 0.25 :select t :quit t)
      ("^\\*\\(?:Proced\\|timer-list\\|Abbrevs\\|Output\\|Occur\\|unsent mail.*?\\|message\\)\\*" :ignore t))))

(after! org
  (setq org-agenda-custom-commands
      '(("p" "Projects" tags "+CATEGORY=\"PROJ\"+LEVEL=1")
        ("a" "Actions" tags "+TODO=\"TODO\"|+TODO=\"LOOP\"")))

  (setq +org-capture-notes-file "inbox.org")

  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  (setq org-src-preserve-indentation t)
  (setq org-adapt-indentation t)
  (setq org-startup-indented t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq global-flycheck-mode nil)

(load-theme 'tsdh-light)

;; (setq twelf-root "~/vendor/twelf/")
;; (load (concat twelf-root "emacs/twelf-init.el"))


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
        (lsp-file-watch-threshold . 2000)
        (org-babel-noweb-wrap-start . "«")
        (org-babel-noweb-wrap-end . "»")
        (org-src-preserve-indentation)
        (eval progn
              (make-local-variable 'projectile-make-test-cmd)
              (setq projectile-ruby-test-cmd "rake"))
        (eval message "starting yaml-mode")
        (eval message "hello there")
        (eval progn
              (let*
                  ((load-path
                    (cons
                     (locate-dominating-file default-directory "ef.el")
                     load-path)))
                (require 'ef)))
        (jnm/in-ef-dir . t)
        (ef/files "actions.org" "projects-maintenance.org" "projects.org" "upcoming.org" "waiting.org")))
