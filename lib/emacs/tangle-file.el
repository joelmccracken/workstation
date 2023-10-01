

;; And a little emacs lisp that goes with the tangle process:

;; [[file:../../workstation.org::*Makefile][Makefile:3]]
;;; tangle-file.el --- description -*- lexical-binding: t; -*-

(setq safe-local-variable-values
      '((org-babel-noweb-wrap-start . "«")
        (org-babel-noweb-wrap-end . "»")))

(doom-require 'doom-start)

(defun do-tangle ()
  "Do the tangle"
  (find-file "workstation.org")
  (org-babel-tangle))

(do-tangle)

(provide 'tangle-file)

;;; tangle-file.el ends here
;; Makefile:3 ends here
