;;; tangle-file.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Joel McCracken
;;
;; Author: Joel McCracken <http://github/joel>
;; Maintainer: Joel McCracken <mccraken.joel@gmail.com>
;; Created: August 15, 2020
;; Modified: August 15, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/joel/tangle-file
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(add-to-list 'command-switch-alist '("--tangle" . do-tangle))

(defun do-tangle (arg)
  "Do the tangle. ARG is ignored."
  (find-file "workstation.org")
  (org-babel-tangle))

(provide 'tangle-file)
;;; tangle-file.el ends here
