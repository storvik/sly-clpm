;;; sly-clpm.el --- SLY CLPM integration -*- lexical-binding: t -*-

;; Copyright © 2021

;; Author: Petter Storvik
;; URL: https://github.com/storvik/sly-clpm
;; Version: 0.1.0
;; Created: 2021-11-05
;; Package-Requires: ((emacs "25.1"))
;; Keywords: extensions sly-clpm

;; This file is NOT part of GNU Emacs.

;;; License:

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
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; External contrib for SLY that provides support for working with the
;; Common Lisp Package Manager (CLPM).

;;; Code:

(require 'sly)

(defvar sly-clpm-shortcut-alist
  '(("clpm install context" . sly-clpm-install-context)
    ("clpm active context" . sly-clpm-active-contex)
    ("clpm activate bundle context" . sly-clpm-activate-context)
    ("clpm activate global context" . sly-clpm-activate-globale-context)))

(define-sly-contrib sly-clpm
  "CLPM system support"
  (:authors "Petter S. Storvik <petterstorvik@gmail.com>")
  (:license "MIT")
  (:slynk-dependencies slynk-clpm)
  (:on-load
   (setq sly-mrepl-shortcut-alist
         (append sly-mrepl-shortcut-alist sly-clpm-shortcut-alist))))

(defun sly-clpm--find-clpmfile ()
  "Locate dominating clpmfile."
  (concat (locate-dominating-file (buffer-file-name) "clpmfile") "clpmfile"))

(defun sly-clpm-bundle-init (clpmfile asd)
  "Initiate clpm in project
CLPMFILE must be path to a non existing clpmfile and ASD is a list containing
.asd files. Typically project root '(<projectname>.asd <projectname>-test.asd)."
  ;; TODO: Detect project .asd files automagically
  (interactive (list (expand-file-name (read-file-name "Path to clpmfile: " nil nil t))

                     (let (tags tag)
                       (while (file-regular-p
                                     (setq tag (expand-file-name (read-file-name "Path to asd file: " nil nil t))))
                         (push tag tags))
                       (nreverse tags))))
  (sly-eval `(slynk-clpm:bundle-init clpmfile asd)))

(defun sly-clpm-install-context (&optional clpmfile)
  "Install clpm context.
Looks for project root clpmfile if non is specified."
  (interactive (list (expand-file-name (read-file-name "Path to clpmfile: " (sly-clpm--find-clpmfile)))))
  (let ((context (or clpmfile (sly-clpm--find-clpmfile))))
    (when (sly-eval `(slynk-clpm:install-context ,context))
      (sly-message "context %s installed" context))))

;; TODO Implement this
(defun sly-clpm-install-from-asd (asd)
  "Install system by using pointer to .asd file."
  (interactive))

(defun sly-clpm-install-from-source (project)
  "Install project from one of the configured sources."
  (when (sly-eval `(slynk-clpm:install-project ,project))
    (sly-message "project %s installed" project)))

(defun sly-clpm-active-contex ()
  "Return active context."
  (interactive)
  (message (sly-eval '(slynk-clpm:active-context))))

(defun sly-clpm-activate-context (&optional clpmfile)
  "Activate clpm context from clpmfile.
Uses project root clpmfile if non is specified."
  ;; (interactive "FPath to clpmfile")
  (interactive (list (expand-file-name (read-file-name "Path to clpmfile: " (sly-clpm--find-clpmfile)))))
  (let ((context (or clpmfile (sly-clpm--find-clpmfile))))
    (when context
      (sly-eval `(slynk-clpm:activate-context ,context))
      (sly-message "context %s activated" (sly-clpm-active-contex)))))

(defun sly-clpm-activate-global-context (context)
  "Activate global context."
  (interactive "PGlobal context: ")
  (when context
    (sly-eval `(slynk-clpm:activate-global-context ,context))
    (sly-message "context %s activated" (sly-clpm-active-contex))))

;; TODO Implement update, should update all / update system / update project

;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-clpm 'append))

(provide 'sly-clpm)
;;; sly-clpm.el ends here
