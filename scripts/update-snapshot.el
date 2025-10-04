;;; update-snapshot.el --- Update the snapshot URL when available  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Shen, Jen-Chieh

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
;; Update the snapshot URL when available.
;;

;;; Code:

(load-file (if (file-exists-p "./scripts/_prepare.el")
               "./scripts/_prepare.el"
             "./_prepare.el"))

(defun update-snapshot-to-src (parent response)
  "Paste the new snapshot to `src/main.ts'"
  (let* ((file (get-latest-snapshot response))
         (snapshot-url (concat parent file)))
    (with-current-buffer (find-file "./src/main.ts")
      (goto-char (point-min))
      (search-forward "zipPath = \"")
      (delete-region (point) (- (line-end-position) 2))
      (insert snapshot-url)
      (save-buffer))))

(defun navigate-version-folder (dir)
  "After navigate to latest emacs version.

The argument DIR is a string like `emacs-30'; therefore, we will need
to form the new url ourselves."
  (let ((new-dir (concat ftp-source-snapshot dir "/")))
    (request new-dir
      :sync t
      :complete
      (cl-function
       (lambda (&key response &allow-other-keys)
         (update-snapshot-to-src new-dir response))))))

(request ftp-source-snapshot
  :sync t
  :complete
  (cl-function
   (lambda (&key response &allow-other-keys)
     (let ((dir (get-major-version response)))
       (navigate-version-folder dir)))))

;;; update-snapshot.el ends here
