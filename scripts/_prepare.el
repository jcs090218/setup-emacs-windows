;;; _prepare.el --- Prepration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'rect)

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
         (not (s-contains-p "-no-deps." line))
         (not (s-contains-p "-deps." line))
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

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; _prepare.el ends here
