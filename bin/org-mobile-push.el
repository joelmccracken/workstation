(doom-require 'doom-start)

(require 'org)

(org-mobile-pull)
(save-some-buffers t)
(org-mobile-push)
(save-some-buffers t)
