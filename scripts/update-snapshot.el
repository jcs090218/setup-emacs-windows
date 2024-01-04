;;; update-snapshot.el --- Update the snapshot URL when available  -*- lexical-binding: t; -*-

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
;; Update the snapshot URL when available.
;;

;;; Code:

(require 'request)
(require 's)

(defun get-major-version (response)
  "Return the latest snapshot string."
  (let ((data (request-response-data response)))
    (with-temp-buffer
      (insert data)
      (re-search-backward "emacs-[0-9\.]+/")
      (let ((beg (point))
            (end (progn (search-forward "<") (- (point) 2))))
        (buffer-substring-no-properties beg end)))))

(defun capture-time-at-line ()
  "Return timestamp at current line."
  (save-excursion
    (let ((end (point)))
      (search-backward ">")
      (buffer-substring-no-properties (1+ (point)) end))))

(defun is-target-file-line ()
  "Return t if this line is the valid Emacs .zip file."
  (let ((line (thing-at-point 'line)))
    (and (s-contains-p ".zip\">" line)
         (not (s-contains-p "-no-deps" line))
         (not (s-contains-p "-sha" line))
         (not (s-contains-p "-src" line)))))

(defun get-latest-snapshot-line-no (response)
  "Return the line number with the latest snapshot."
  (let ((data (request-response-data response))
        (time)
        (line))
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (while (re-search-forward "[0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+" nil t)
        (when (is-target-file-line)
          (let ((current (capture-time-at-line))
                (line-no (line-number-at-pos)))
            (when (or (null time)
                      (time-less-p (date-to-time time)
                                   (date-to-time current)))
              (setq time current
                    line line-no))))))
    line))

(defun get-latest-snapshot (response)
  "Return the snapshot file.

It should return a string. e.g., `emacs-30.0.50-bc61a1.zip'."
  (let ((data (request-response-data response))
        (line (get-latest-snapshot-line-no response)))
    (with-temp-buffer
      (insert data)
      (goto-line line)
      (goto-char (line-beginning-position))
      (search-forward ".zip\">" nil t)
      (let ((end (- (point) 2)))
        (search-backward "=\"")
        (buffer-substring-no-properties (+ (point) 2) end)))))

(defun update-snapshot-to-src (parent response)
  "Paste the new snapshot to `src/main.ts'"
  (let* ((file (get-latest-snapshot response))
         (snapshot-url (concat parent file)))
    (with-current-buffer (find-file "./src/main.ts")
      (goto-char (point-min))
      (search-forward "zipPath = \"")
      (delete-region (point) (- (line-end-position) 2))
      (insert snapshot-url "sss")
      (save-buffer))))

(defun navigate-version-folder (dir)
  "After navigate to latest emacs version.

The argument DIR is a string like `emacs-30'; therefore, we will need
to form the new url ourselves."
  (let ((new-dir (concat "https://alpha.gnu.org/gnu/emacs/pretest/windows/" dir "/")))
    (request new-dir
      :sync t
      :complete
      (cl-function
       (lambda (&key response &allow-other-keys)
         (update-snapshot-to-src new-dir response))))))

(request "https://alpha.gnu.org/gnu/emacs/pretest/windows/"
  :sync t
  :complete
  (cl-function
   (lambda (&key response &allow-other-keys)
     (let ((dir (get-major-version response)))
       (navigate-version-folder dir)))))

;;; update-snapshot.el ends here
