;;; config.el --- Description -*- lexical-binding: t; -*-

(after! org
  (setq org-directory "~/Dropbox/EF/")
  (setq org-roam-directory "~/Dropbox/EF/")
  (setq org-roam-db-location "~/Dropbox/EF/org-roam.glamdring.db")

  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-directory "~/Dropbox/EF")
  (setq org-id-locations-file "~/Dropbox/EF/.orgids.el")

  (setq org-agenda-files '("~/Dropbox/EF"
                           "~/Dropbox/EF/reference"
                           "~/Dropbox/EF/projects"
                           ))

  (setq +org-capture-notes-file "inbox.org")
  (setq org-mobile-files (org-agenda-files))
  (setq org-mobile-inbox-for-pull "~/Dropbox/EF/inbox-mobile.org"))
;;; config.el ends here
