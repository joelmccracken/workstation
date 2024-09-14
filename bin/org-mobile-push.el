(doom-require 'doom-start)

(require 'org)

(org-mobile-pull)
(save-some-buffers t)
(org-mobile-push)
(org-roam-db-sync)
(save-some-buffers t)
