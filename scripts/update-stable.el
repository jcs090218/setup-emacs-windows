;;; update-stable.el --- Update stable version release  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Update stable version release.
;;

;;; Code:

(load-file (if (file-exists-p "./scripts/_prepare.el")
               "./scripts/_prepare.el"
             "./_prepare.el"))

(defun extract-latest-version (response)
  "Convert the file to just the version.

For example, `emacs-29.1_2.zip' to `29.1.2'."
  (let ((file (get-latest-snapshot response)))
    (setq file (s-replace "emacs-" "" file)
          file (s-replace ".zip" "" file)
          file (s-replace "_" "." file))
    file))

(defun update-gha-workflow (latest)
  "Update workflow file `.github/workflows/test.yml'."
  (with-current-buffer (find-file "./.github/workflows/test.yml")
    (goto-char (point-min))
    (search-forward "emacs_version: [")
    (goto-char (- (line-end-position) 9))
    (insert version ", ")
    (save-buffer)))

(defun update-stable-to-src (latest)
  "Paste the new snapshot to `src/main.ts'"
  (with-current-buffer (find-file "./src/main.ts")
    (goto-char (point-min))
    (search-forward "default: {")
    (forward-line -4)
    (goto-char (line-end-position))
    (search-backward "{")
    (insert "\n" (spaces-string (current-indentation)) "case \"" "29.1.2" "\": ")
    (save-buffer)))

(defun navigate-version-folder (dir)
  "After navigate to latest emacs version.

The argument DIR is a string like `emacs-30'; therefore, we will need
to form the new url ourselves."
  (let ((new-dir (concat "https://ftp.gnu.org/gnu/emacs/windows/" dir "/")))
    (request new-dir
      :sync t
      :complete
      (cl-function
       (lambda (&key response &allow-other-keys)
         (let ((latest (extract-latest-version response)))
           (update-gha-workflow latest)
           (update-stable-to-src latest)
           ))))))

(request "https://ftp.gnu.org/gnu/emacs/windows/"
  :sync t
  :complete
  (cl-function
   (lambda (&key response &allow-other-keys)
     (let ((dir (get-major-version response)))
       (navigate-version-folder dir)))))

;;; update-stable.el ends here
