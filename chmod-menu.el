;;; chmod-menu.el --- Menu for chmod -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/chmod-menu
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Menu for chmod

;;; Code:


(require 'transient)
(defvar chmod-menu-file nil)

(defun chmod-menu-transient-init-val ()
  "Print permissions for FILE."
  (let* ((perm (format "%o" (file-modes chmod-menu-file)))
         (nums (mapcar #'string-to-number (split-string perm "" t)))
         (permissions-short '(""
                              "x"
                              "w"
                              "xw"
                              "r"
                              "rx"
                              "rw"
                              "rwx")))
    (seq-map-indexed
     (lambda (it i)
       (concat "--" it "=" (nth (nth i nums)
                                permissions-short)))
     '("owner" "group" "others"))))

(defun chmod-menu-transient-to-octal (value)
  "Convert VALUE to octal format."
  (pcase value
    ("x"  "1")
    ("w"  "2")
    ("xw"  "3")
    ("r"  "4")
    ("rx"  "5")
    ("rw"  "6")
    ("rwx"  "7")
    (_ "0")))

(transient-define-argument chmod-menu--arg-group ()
  :class 'transient-switches
  :argument-format "--group=%s"
  :init-value (lambda (obj)
                (setf (slot-value obj 'value)
                      (nth 1 (chmod-menu-transient-init-val))))
  :argument-regexp "\\(?:r\\(?:wx\\|[wx]\\)\\|xw\\|[rwx]\\)"
  :choices '("x" "w" "xw" "r" "rx" "rw" "rwx"))

(transient-define-argument chmod-menu--arg-owner ()
  :class 'transient-switches
  :argument-format "--owner=%s"
  :init-value (lambda (obj)
                (setf (slot-value obj 'value)
                      (nth 0 (chmod-menu-transient-init-val))))
  :argument-regexp "\\(?:r\\(?:wx\\|[wx]\\)\\|xw\\|[rwx]\\)"
  :choices '("x" "w" "xw" "r" "rx" "rw" "rwx"))

(transient-define-argument chmod-menu--others ()
  :class 'transient-switches
  :argument-format "--others=%s"
  :init-value (lambda (obj)
                (setf (slot-value obj 'value)
                      (nth 2 (chmod-menu-transient-init-val))))
  :argument-regexp "\\(?:r\\(?:wx\\|[wx]\\)\\|xw\\|[rwx]\\)"
  :choices '("x" "w" "xw" "r" "rx" "rw" "rwx"))


;;;###autoload (autoload 'chmod-menu "chmod-menu.el" nil t)
(transient-define-prefix chmod-menu ()
  "Change permission for current file or directory with `chmod'."
  [:description
   (lambda ()
     (concat
      "Permissions of "
      (abbreviate-file-name
       chmod-menu-file)
      ": "
      (format "%o" (file-modes chmod-menu-file))))
   ("o" "Owner" chmod-menu--arg-owner)
   ("g" "Group" chmod-menu--arg-group)
   ("s" "Others" chmod-menu--others)]
  [("RET" "Change mode" (lambda ()
                          (interactive)
                          (let* ((args
                                  (transient-args transient-current-command))
                                 (owner (transient-arg-value "--owner=" args))
                                 (group (transient-arg-value "--group=" args))
                                 (others (transient-arg-value "--others=" args))
                                 (val (string-join
                                       (list (chmod-menu-transient-to-octal
                                              owner)
                                             (chmod-menu-transient-to-octal
                                              group)
                                             (chmod-menu-transient-to-octal
                                              others))
                                       "")))
                            (when (yes-or-no-p (format
                                                "Change file permission to %s on %s?"
                                                val chmod-menu-file))
                              (set-file-modes
                               chmod-menu-file
                               (string-to-number val 8)
                               'nofollow)))))]
  (interactive)
  (setq chmod-menu-file
        (read-file-name "File: "
                        (when buffer-file-name
                          (file-name-nondirectory buffer-file-name))))
  (transient-setup 'chmod-menu))

(provide 'chmod-menu)
;;; chmod-menu.el ends here