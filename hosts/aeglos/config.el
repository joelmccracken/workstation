;; aeglos/

;; [[file:../../workstation.org::*aeglos/][aeglos/:1]]
(after! org
  (setq org-directory "~/Dropbox/EF/")
  (setq org-roam-directory "~/Dropbox/EF/")
  (setq org-roam-db-location "~/Dropbox/EF/org-roam.aeglos.db")

  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-directory "~/Dropbox/EF")
  (setq org-id-locations-file "~/Dropbox/EF/.orgids.el")
  (setq org-agenda-files '("~/EF/actions.org" "~/EF/projects.org"))
  (setq +org-capture-notes-file "inbox.org")
  (setq org-mobile-files (org-agenda-files))
  (setq org-mobile-inbox-for-pull "~/Dropbox/EF/inbox-mobile.org"))
;; aeglos/:1 ends here
