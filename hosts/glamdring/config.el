;; [[file:../../workstation.org::*glamdring/][glamdring/:2]]
(after! org
  (setq org-directory "~/EF/")
  (setq org-roam-directory "~/EF/")
  (setq org-roam-db-location "~/EF/org-roam.glamdring.db")

  ;; for now wont be able to org-mobile-push from glamdring
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-directory "~/EF")
  (setq org-id-locations-file "~/EF/.orgids.el")
  (setq org-agenda-files '("~/EF/actions.org" "~/EF/projects.org"))
  (setq +org-capture-notes-file "inbox.org")
  (setq org-mobile-files (org-agenda-files))
  (setq org-mobile-inbox-for-pull "~/EF/inbox-mobile.org"))
;; glamdring/:2 ends here
