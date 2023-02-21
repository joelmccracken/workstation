(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" default))
 '(safe-local-variable-values
   '((org-src-preserve-indentation)
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
     (ef/files "actions.org" "projects-maintenance.org" "projects.org" "upcoming.org" "waiting.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
