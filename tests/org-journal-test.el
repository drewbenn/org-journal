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
  (message "ZZZ: setup for %s" org-journal-dir-test)
  (when (file-exists-p org-journal-dir-test)
    (message "ZZZ: Deleting!")
    (delete-directory org-journal-dir-test t))
  (when (file-exists-p org-journal-dir-test) (message "ZZZ: Warning!!"))
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
     (message "files before starting: %s" (directory-files org-journal-dir-test))
     ,@body
     (message "files left behind: %s" (directory-files org-journal-dir-test))
     (delete-directory org-journal-dir-test t)
     (delete-file (concat org-journal-dir-test "-link"))))
(def-edebug-spec org-journal-test-macro (body))











(ert-deftest org-journal-carryover-delete-empty-journal-test ()
  "Org journal delete empty journal test
TODO: deleting this test allows widen+yearly to pass"
  (org-journal-test-macro
      (let ((buffer "20181231")
            (org-journal-carryover-delete-empty-journal 'always)
            (inhibit-message t))

        ;; Test that journal file gets dumped, after carryover
        (with-temp-buffer
          (org-journal-mode)
          (insert "* Wednesday, 01/02/19\n")
          (insert "** TODO a\n")
          (write-file (expand-file-name buffer org-journal-dir-test))
          (kill-buffer buffer))
        (org-journal-new-entry nil)
        (save-buffer)
        (kill-buffer)
        (should (equal "No journal entry for this date."
                       (org-journal-read-or-display-entry (encode-time 0 0 0 31 12 2018) 'noselect)))

        ;; Test even single entry gets deleted, relevant for weekly, monthly and yearly journal files.
        (org-journal-dir-test-setup)
        (setq org-journal-file-type 'yearly)
        (with-temp-buffer
          (org-journal-mode)
          (insert "* Wednesday, 01/01/19\n")
          (org-set-property "CREATED" "20190101")
          (insert "** 15:00 a\n")
          (insert "* Wednesday, 01/02/19\n")
          (org-set-property "CREATED" "20190102")
          (insert "** TODO a\n")
          (write-file (expand-file-name "20190101" org-journal-dir-test))
          (kill-buffer "20190101"))

        (org-journal-new-entry nil)
        (save-buffer)
        (kill-buffer)
        (should (equal "No journal entry for this date."
                       (org-journal-read-or-display-entry (encode-time 0 0 0 2 1 2019) 'noselect))))))



(ert-deftest org-journal-scheduled-carryover-yearly-test ()
  (org-journal-test-macro
      (let ((org-journal-file-type 'yearly)
            (org-journal-created-property-timestamp-format "[%Y-%m-%d %a]")
            new-scheduled-date)
        ;; Copy test files to temporary directory
        (copy-directory
         (directory-file-name "tests/journals/yearly/carryover1")
         (file-name-as-directory org-journal-dir-test) nil nil t)
        (org-journal-new-entry 4) ;; 4 - no new time entry
        (setq new-scheduled-date (with-temp-buffer
                                   (org-insert-time-stamp (current-time))
                                   (org-journal-buffer-content t t)))
        (goto-char (point-min))
        (search-forward new-scheduled-date)
        (search-forward new-scheduled-date))))




