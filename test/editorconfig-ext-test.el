;;; editorconfig-ext-test.el --- Tests for editorconfig-ext  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ab.

;; Author: Ab. <3223197+ab-ten@users.noreply.github.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;;; Commentary:

;; Unit tests for editorconfig-ext package using ERT (Emacs Regression Testing).
;;
;; Run tests with:
;;   emacs -batch -l ert -l editorconfig-ext.el -l test/editorconfig-ext-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'editorconfig-ext)

;;; Test Fixtures and Helpers

(defmacro editorconfig-ext-test-with-temp-buffer (contents &rest body)
  "Create a temporary buffer with CONTENTS and execute BODY.
The buffer will have a temporary file name for realistic testing."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((temp-file (make-temp-file "editorconfig-ext-test" nil ".txt")))
       (unwind-protect
           (progn
             (setq buffer-file-name temp-file)
             (insert ,contents)
             ,@body)
         (when (file-exists-p temp-file)
           (delete-file temp-file))))))

(defmacro editorconfig-ext-test-with-editorconfig (config-content file-path &rest body)
  "Create a temporary .editorconfig file and execute BODY.
CONFIG-CONTENT is the content of the .editorconfig file.
FILE-PATH is the path where the test file should be created relative to the config."
  (declare (indent 2))
  `(let* ((temp-dir (make-temp-file "editorconfig-ext-test" t))
          (config-file (expand-file-name ".editorconfig" temp-dir))
          (test-file (expand-file-name ,file-path temp-dir)))
     (unwind-protect
         (progn
           ;; Create .editorconfig file
           (with-temp-file config-file
             (insert ,config-content))
           ;; Create directory structure if needed
           (let ((test-dir (file-name-directory test-file)))
             (when test-dir
               (make-directory test-dir t)))
           ;; Execute test body with the test file
           (with-temp-buffer
             (setq buffer-file-name test-file)
             ,@body))
       ;; Cleanup
       (when (file-exists-p temp-dir)
         (delete-directory temp-dir t)))))

;;; Status Detection Tests

(ert-deftest editorconfig-ext-test-get-status-no-file ()
  "Test status detection when buffer has no associated file."
  (with-temp-buffer
    (setq buffer-file-name nil)
    (should (eq (editorconfig-ext-get-status) nil))))

(ert-deftest editorconfig-ext-test-get-status-editorconfig-disabled ()
  "Test status detection when editorconfig-mode is disabled."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (let ((editorconfig-mode nil))
      (should (eq (editorconfig-ext-get-status) nil)))))

(ert-deftest editorconfig-ext-test-get-status-applied ()
  "Test status detection when config is found and properties applied."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (let ((editorconfig-mode t)
          (editorconfig-ext-config-found "/path/to/.editorconfig")
          (editorconfig-ext-applied 3))
      (should (eq (editorconfig-ext-get-status) 'applied)))))

(ert-deftest editorconfig-ext-test-get-status-found-no-props ()
  "Test status detection when config is found but no properties applied."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (let ((editorconfig-mode t)
          (editorconfig-ext-config-found "/path/to/.editorconfig")
          (editorconfig-ext-applied 0))
      (should (eq (editorconfig-ext-get-status) 'found)))))

(ert-deftest editorconfig-ext-test-get-status-not-found ()
  "Test status detection when no config is found."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (let ((editorconfig-mode t)
          (editorconfig-ext-config-found nil)
          (editorconfig-ext-applied nil))
      (should (eq (editorconfig-ext-get-status) 'not-found)))))

;;; Mode Line Display Tests

(ert-deftest editorconfig-ext-test-mode-line-string-applied ()
  "Test mode line string generation for applied status."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (let ((editorconfig-mode t)
          (editorconfig-ext-config-found "/path/to/.editorconfig")
          (editorconfig-ext-applied 3)
          (editorconfig-ext-use-colors nil))
      (should (string= (editorconfig-ext-mode-line-string) " EC✓")))))

(ert-deftest editorconfig-ext-test-mode-line-string-found ()
  "Test mode line string generation for found status."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (let ((editorconfig-mode t)
          (editorconfig-ext-config-found "/path/to/.editorconfig")
          (editorconfig-ext-applied 0)
          (editorconfig-ext-use-colors nil))
      (should (string= (editorconfig-ext-mode-line-string) " EC○")))))

(ert-deftest editorconfig-ext-test-mode-line-string-not-found ()
  "Test mode line string generation for not-found status."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (let ((editorconfig-mode t)
          (editorconfig-ext-config-found nil)
          (editorconfig-ext-applied nil)
          (editorconfig-ext-use-colors nil))
      (should (string= (editorconfig-ext-mode-line-string) " EC✗")))))

(ert-deftest editorconfig-ext-test-mode-line-string-no-status ()
  "Test mode line string generation when no status is available."
  (with-temp-buffer
    (setq buffer-file-name nil)
    (should (eq (editorconfig-ext-mode-line-string) nil))))

(ert-deftest editorconfig-ext-test-mode-line-string-with-colors ()
  "Test mode line string generation with colors enabled."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (let ((editorconfig-mode t)
          (editorconfig-ext-config-found "/path/to/.editorconfig")
          (editorconfig-ext-applied 3)
          (editorconfig-ext-use-colors t))
      (let ((result (editorconfig-ext-mode-line-string)))
        (should (stringp result))
        (should (string= (substring-no-properties result) " EC✓"))
        (should (get-text-property 0 'face result))))))

;;; Tracking Function Tests

(ert-deftest editorconfig-ext-test-track-application-with-props ()
  "Test tracking function with applied properties."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (let ((props (make-hash-table :test 'equal)))
      (puthash "indent_style" "space" props)
      (puthash "indent_size" "2" props)
      ;; Mock editorconfig-core-get-nearest-editorconfig
      (cl-letf (((symbol-function 'editorconfig-core-get-nearest-editorconfig)
                 (lambda (_) "/mock/path/.editorconfig")))
        (editorconfig-ext-track-application props)
        (should (eq editorconfig-ext-applied 2))
        (should (string= editorconfig-ext-config-found "/mock/path/.editorconfig"))))))

(ert-deftest editorconfig-ext-test-track-application-empty-props ()
  "Test tracking function with empty properties."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (let ((props (make-hash-table :test 'equal)))
      ;; Mock editorconfig-core-get-nearest-editorconfig
      (cl-letf (((symbol-function 'editorconfig-core-get-nearest-editorconfig)
                 (lambda (_) "/mock/path/.editorconfig")))
        (editorconfig-ext-track-application props)
        (should (eq editorconfig-ext-applied 0))
        (should (string= editorconfig-ext-config-found "/mock/path/.editorconfig"))))))

(ert-deftest editorconfig-ext-test-track-application-no-file ()
  "Test tracking function when buffer has no file."
  (with-temp-buffer
    (setq buffer-file-name nil)
    (let ((props (make-hash-table :test 'equal)))
      (editorconfig-ext-track-application props)
      (should (eq editorconfig-ext-applied 0))
      (should (eq editorconfig-ext-config-found nil)))))

;;; Minor Mode Tests

(ert-deftest editorconfig-ext-test-minor-mode-enable ()
  "Test enabling editorconfig-ext-mode."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (editorconfig-ext-mode 1)
    (should (bound-and-true-p editorconfig-ext-mode))
    ;; Check if hook is added
    (should (memq 'editorconfig-ext-track-application
                  editorconfig-after-apply-functions))))

(ert-deftest editorconfig-ext-test-minor-mode-disable ()
  "Test disabling editorconfig-ext-mode."
  (editorconfig-ext-test-with-temp-buffer "test content"
    (editorconfig-ext-mode 1)
    (editorconfig-ext-mode -1)
    (should-not (bound-and-true-p editorconfig-ext-mode))
    ;; Check if hook is removed (locally)
    (should-not (memq 'editorconfig-ext-track-application
                      (buffer-local-value 'editorconfig-after-apply-functions
                                        (current-buffer))))))

;;; Configuration Tests

(ert-deftest editorconfig-ext-test-custom-symbols ()
  "Test custom symbol configuration."
  (let ((editorconfig-ext-symbols '((applied . " ✅")
                                    (found . " ⚠️")
                                    (not-found . " ❌"))))
    (editorconfig-ext-test-with-temp-buffer "test content"
      (let ((editorconfig-mode t)
            (editorconfig-ext-config-found "/path/to/.editorconfig")
            (editorconfig-ext-applied 3)
            (editorconfig-ext-use-colors nil))
        (should (string= (editorconfig-ext-mode-line-string) " ✅"))))))

(ert-deftest editorconfig-ext-test-custom-colors ()
  "Test custom color configuration."
  (let ((editorconfig-ext-colors '((applied . (:foreground "blue"))
                                   (found . (:foreground "yellow"))
                                   (not-found . (:foreground "purple")))))
    (editorconfig-ext-test-with-temp-buffer "test content"
      (let ((editorconfig-mode t)
            (editorconfig-ext-config-found "/path/to/.editorconfig")
            (editorconfig-ext-applied 3)
            (editorconfig-ext-use-colors t))
        (let ((result (editorconfig-ext-mode-line-string)))
          (should (equal (get-text-property 0 'face result)
                         '(:foreground "blue"))))))))

;;; Integration Tests

(ert-deftest editorconfig-ext-test-real-editorconfig-applied ()
  "Test with a real .editorconfig file and applied properties."
  :expected-result (if (fboundp 'editorconfig-call-get-properties-function)
                       :passed :failed)
  (editorconfig-ext-test-with-editorconfig
      "[*.txt]
indent_style = space
indent_size = 4
end_of_line = lf
"
      "test.txt"
    (let ((editorconfig-mode t))
      ;; Simulate property application
      (when (fboundp 'editorconfig-call-get-properties-function)
        (let ((props (editorconfig-call-get-properties-function buffer-file-name)))
          (when (hash-table-p props)
            (editorconfig-ext-track-application props)
            (should (> editorconfig-ext-applied 0))
            (should (stringp editorconfig-ext-config-found))))))))

(ert-deftest editorconfig-ext-test-real-editorconfig-no-match ()
  "Test with a real .editorconfig file but no matching rules."
  :expected-result (if (fboundp 'editorconfig-call-get-properties-function)
                       :passed :failed)
  (editorconfig-ext-test-with-editorconfig
      "[*.js]
indent_style = space
indent_size = 2
"
      "test.txt"
    (let ((editorconfig-mode t))
      ;; Simulate property application
      (when (fboundp 'editorconfig-call-get-properties-function)
        (let ((props (editorconfig-call-get-properties-function buffer-file-name)))
          (when (hash-table-p props)
            (editorconfig-ext-track-application props)
            (should (eq editorconfig-ext-applied 0))
            (should (stringp editorconfig-ext-config-found))))))))

;;; Test Runner Helper

;;;###autoload
(defun editorconfig-ext-run-tests ()
  "Run all editorconfig-ext tests interactively."
  (interactive)
  (ert "^editorconfig-ext-test-"))

(provide 'editorconfig-ext-test)

;;; editorconfig-ext-test.el ends here
