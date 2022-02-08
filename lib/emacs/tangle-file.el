;; [[file:../../workstation.org::*Makefile][Makefile:2]]
;;; tangle-file.el --- description -*- lexical-binding: t; -*-

;; WARNING: This file is managed by tangling workstation.org. Do not edit directly!
(add-to-list 'command-switch-alist '("--tangle" . do-tangle))

(defun do-tangle (arg)
  "Do the tangle. ARG is ignored."
  (find-file "workstation.org")
  (org-babel-tangle))

(provide 'tangle-file)

;;; tangle-file.el ends here
;; Makefile:2 ends here
