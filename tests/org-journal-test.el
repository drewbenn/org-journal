;; org-journal-test.el --- Test file for org-journal -*- lexical-binding: t; -*-
;;
;; Author: Christian Schwarzgruber (c.schwarzgruber.cs@gmail.com)
;;
;; Copyright (c) 2020-2024 Christian Schwarzgruber
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;; this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its contributors
;; may be used to endorse or promote products derived from this software without
;; specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;; Description:
;;

(require 'ert)
(require 'org-journal)

(defvar org-journal-dir-test (make-temp-file "org-journal-" t))

(defun org-journal-dir-test-setup ()
  "Create temporary directory."
  (when (file-exists-p org-journal-dir-test)
    (delete-directory org-journal-dir-test t))
  ;; Kill leftover buffers from previous failed test
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer) (string-match org-journal-dir-test (buffer-file-name buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (kill-buffer buffer))))
  (make-directory org-journal-dir-test)
  (make-symbolic-link org-journal-dir-test (concat org-journal-dir-test "-link") t)
  (org-journal-invalidate-cache))

(defun org-journal-buffer-content (&optional no-properties ignore-leading-whitespaces)
  (let ((buffer-content-fn (if no-properties
                               #'buffer-substring-no-properties
                             #'buffer-substring)))
    (when ignore-leading-whitespaces
      (fundamental-mode)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\([ \t]+\\)" nil t)
          (replace-match "" nil nil))))
    (funcall buffer-content-fn (point-min) (point-max))))

(defmacro org-journal-test-macro (&rest body)
  "Wrapp a `org-journal' -- `ert'-test with default values."
  (declare (indent 1))
  `(let* ((org-journal-dir (concat org-journal-dir-test "-link"))
          (comment-start-skip "^\\s-*#\\(?: \\|$\\)")
          (org-journal--cache-file (expand-file-name  "org-journal.cache" org-journal-dir-test))
          (org-journal-file-type 'daily)
          (org-journal-date-format "Test header")
          (org-agenda-inhibit-startup t)
          (org-journal-time-format "%R")
          (org-journal-created-property-timestamp-format "%Y%m%d")
          org-journal-file-header
          org-journal-encrypt-journal
          org-journal-enable-encryption)
     (org-journal-invalidate-cache)
     (org-journal-dir-test-setup)
     ,@body
     (delete-directory org-journal-dir-test t)
     (delete-file (concat org-journal-dir-test "-link"))))
(def-edebug-spec org-journal-test-macro (body))







(ert-deftest org-journal-carryover-items-test ()
  "Org journal new entry test."
  (org-journal-test-macro
   (let ((org-journal-file-type 'weekly)
         ;; Always use english as time locale.
         (system-time-locale "C")
         (buffer "20181231"))
     (with-temp-buffer
       (org-journal-mode)
       (insert "* Tuesday, 01/01/19\n")
       (org-set-property "CREATED" "20190101")
       (insert "** 13:00 Some journal entry\n")
       (insert "* Wednesday, 01/02/19\n")
       (org-set-property "CREATED" "20190102")
       (insert "** 13:00 Some journal entry 1\n")
       (insert "** TODO Second\n")
       (insert "** 14:00 Some journal entry 2\n")
       (write-file (expand-file-name buffer org-journal-dir-test))
       (kill-buffer buffer))
     (find-file (expand-file-name buffer org-journal-dir-test))
     (should (not (stringp (org-journal-read-or-display-entry (encode-time 0 0 0 1 1 2019)))))
     (kill-buffer buffer)
     (should (not (stringp (org-journal-read-or-display-entry (encode-time 0 0 0 2 1 2019)))))
     (kill-buffer buffer)
     (org-journal-new-entry t)
     (save-buffer)
     (kill-buffer) ;; Kills new journal file buffer
     (kill-buffer buffer)
     (should (not (stringp (org-journal-read-or-display-entry (encode-time 0 0 0 1 1 2019)))))
     (kill-buffer)
     (should (not (stringp (org-journal-read-or-display-entry (encode-time 0 0 0 2 1 2019)))))
     (kill-buffer)

     (should (string= (with-temp-buffer
                        (org-journal-mode)
                        (message "Inserted %s." (insert-file-contents (org-journal--get-entry-path)))
                        (replace-regexp-in-string
                         "^[ ]*" ""
                         (buffer-substring-no-properties (point-min) (point-max))))
                      (concat "* Test header\n:PROPERTIES:\n:CREATED:  "
                              (format-time-string org-journal-created-property-timestamp-format)
                              "\n:END:\n** TODO Second\n"))))))












