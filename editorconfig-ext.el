;;; editorconfig-ext.el --- EditorConfig Extension for Mode Line Display  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Ab. <3223197+ab-ten@users.noreply.github.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience editorconfig mode-line
;; URL: https://github.com/ab-ten/editorconfig-ext

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/gpl-3.0.html>.

;;; Commentary:

;; EditorConfig Extension provides visual feedback in the mode line about
;; whether the current buffer is under EditorConfig control.
;;
;; This package addresses the issue that Emacs 30.1's built-in
;; editorconfig-mode is a global minor mode, making it difficult to
;; determine whether EditorConfig settings are actually applied to
;; the current buffer.
;;
;; Features:
;; - Visual mode line indicator showing EditorConfig status
;; - Color-coded display: Green (applied), Orange (found but not applied), Red (not found)
;; - Automatic detection of .editorconfig files and property application
;; - Compatible with both Emacs 30+ versions
;;
;; Usage:
;;   (require 'editorconfig-ext)
;;   (editorconfig-ext-global-mode 1)

;;; Code:

(require 'editorconfig)
(eval-when-compile (require 'editorconfig-core))

(defgroup editorconfig-ext nil
  "EditorConfig Extension for mode line display."
  :tag "EditorConfig Extension"
  :prefix "editorconfig-ext-"
  :group 'editorconfig)

(defcustom editorconfig-ext-use-colors t
  "Whether to use colors in the mode line display.
When non-nil, the EditorConfig status will be displayed with colors:
- Green: .editorconfig found and properties applied
- Orange: .editorconfig found but no properties applied
- Red: .editorconfig not found"
  :type 'boolean
  :group 'editorconfig-ext)

(defcustom editorconfig-ext-symbols
  '((applied . " EC✓")
    (found . " EC○")
    (not-found . " EC✗"))
  "Symbols to display for different EditorConfig states.
Alist of (STATE . SYMBOL) pairs where STATE can be:
- applied: .editorconfig found and properties applied
- found: .editorconfig found but no properties applied
- not-found: .editorconfig not found"
  :type '(alist :key-type symbol :value-type string)
  :group 'editorconfig-ext)

(defcustom editorconfig-ext-colors
  '((applied . (:foreground "green"))
    (found . (:foreground "orange"))
    (not-found . (:foreground "red")))
  "Colors for different EditorConfig states.
Alist of (STATE . FACE-SPEC) pairs where STATE can be:
- applied: .editorconfig found and properties applied
- found: .editorconfig found but no properties applied
- not-found: .editorconfig not found"
  :type '(alist :key-type symbol :value-type (plist :key-type keyword))
  :group 'editorconfig-ext)

;;; Internal Variables

(defvar-local editorconfig-ext-applied nil
  "Whether EditorConfig properties were applied to this buffer.
This is set to the number of properties applied, or nil if none.")

(defvar-local editorconfig-ext-config-found nil
  "Path to the nearest .editorconfig file for this buffer.
Set to the absolute path if found, nil otherwise.")

;;; Core Functions

(defun editorconfig-ext-track-application (props)
  "Track EditorConfig application status.
This function is added to `editorconfig-after-apply-functions' to monitor
when EditorConfig properties are applied to the current buffer.
PROPS is the hash table of EditorConfig properties."
  (when (hash-table-p props)
    (setq editorconfig-ext-applied (hash-table-count props))
    (setq editorconfig-ext-config-found
          (when buffer-file-name
            (editorconfig-core-get-nearest-editorconfig buffer-file-name)))
    ;; Update mode line
    (force-mode-line-update)))

(defun editorconfig-ext-get-status ()
  "Determine the current EditorConfig status for this buffer.
Returns one of: 'applied, 'found, 'not-found, or nil."
  (cond
   ;; No file associated with buffer
   ((not buffer-file-name) nil)
   ;; EditorConfig mode not enabled
   ((not (bound-and-true-p editorconfig-mode)) nil)
   ;; Config found and properties applied
   ((and editorconfig-ext-config-found
         editorconfig-ext-applied
         (> editorconfig-ext-applied 0))
    'applied)
   ;; Config found but no properties applied
   (editorconfig-ext-config-found 'found)
   ;; No config found
   (t 'not-found)))

(defun editorconfig-ext-mode-line-string ()
  "Return the mode line string for EditorConfig status.
This function is called by the mode line evaluation to display
the current EditorConfig status with appropriate styling."
  (when-let ((status (editorconfig-ext-get-status)))
    (let* ((symbol (cdr (assq status editorconfig-ext-symbols)))
           (color (when editorconfig-ext-use-colors
                    (cdr (assq status editorconfig-ext-colors)))))
      (if color
          (propertize symbol 'face color)
        symbol))))

(defun editorconfig-ext-refresh-status ()
  "Refresh EditorConfig status for the current buffer.
This can be called manually to update the status display."
  (interactive)
  (when buffer-file-name
    (let ((props (editorconfig-call-get-properties-function buffer-file-name)))
      (editorconfig-ext-track-application props))))

(defun editorconfig-ext-show-status ()
  "Show detailed EditorConfig status for the current buffer."
  (interactive)
  (let ((status (editorconfig-ext-get-status)))
    (cond
     ((eq status 'applied)
      (message "EditorConfig: %d properties applied from %s"
               editorconfig-ext-applied
               editorconfig-ext-config-found))
     ((eq status 'found)
      (message "EditorConfig: Found %s but no properties applied"
               editorconfig-ext-config-found))
     ((eq status 'not-found)
      (message "EditorConfig: No .editorconfig file found for %s"
               (file-name-directory buffer-file-name)))
     (t
      (message "EditorConfig: Not applicable (no file or mode disabled)")))))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode editorconfig-ext-mode
  "Minor mode to display EditorConfig status in the mode line.
When enabled, shows a visual indicator in the mode line that indicates
whether the current buffer is under EditorConfig control:

- EC✓ (green): .editorconfig found and properties applied
- EC○ (orange): .editorconfig found but no properties applied
- EC✗ (red): .editorconfig not found

The colors can be disabled by setting `editorconfig-ext-use-colors' to nil.
The symbols can be customized via `editorconfig-ext-symbols'."
  :lighter (:eval (editorconfig-ext-mode-line-string))
  :group 'editorconfig-ext
  (if editorconfig-ext-mode
      (progn
        ;; Add our tracking function to the hook
        (add-hook 'editorconfig-after-apply-functions
                  #'editorconfig-ext-track-application nil t)
        ;; Initial status check
        (editorconfig-ext-refresh-status))
    ;; Remove our tracking function
    (remove-hook 'editorconfig-after-apply-functions
                 #'editorconfig-ext-track-application t)))

;;;###autoload
(define-globalized-minor-mode editorconfig-ext-global-mode
  editorconfig-ext-mode
  (lambda ()
    (when (and buffer-file-name
               (bound-and-true-p editorconfig-mode))
      (editorconfig-ext-mode 1)))
  :group 'editorconfig-ext)

;;; Integration Functions

;;;###autoload
(defun editorconfig-ext-setup ()
  "Set up EditorConfig extension.
This function ensures compatibility with different Emacs versions
and EditorConfig configurations."
  (interactive)
  ;; Ensure editorconfig-mode is available
  (unless (fboundp 'editorconfig-mode)
    (error "EditorConfig mode is not available. Please install or enable editorconfig"))

  ;; Enable global mode if editorconfig-mode is enabled
  (when (bound-and-true-p editorconfig-mode)
    (editorconfig-ext-global-mode 1)
    (message "EditorConfig extension enabled")))

;;;###autoload
(defun editorconfig-ext-info ()
  "Display information about EditorConfig extension."
  (interactive)
  (let ((global-enabled (bound-and-true-p editorconfig-ext-global-mode))
        (local-enabled (bound-and-true-p editorconfig-ext-mode))
        (ec-enabled (bound-and-true-p editorconfig-mode)))
    (with-output-to-temp-buffer "*EditorConfig Extension Info*"
      (princ "EditorConfig Extension Information\n")
      (princ "===================================\n\n")
      (princ (format "EditorConfig Mode: %s\n" (if ec-enabled "enabled" "disabled")))
      (princ (format "Extension Global Mode: %s\n" (if global-enabled "enabled" "disabled")))
      (princ (format "Extension Local Mode: %s\n" (if local-enabled "enabled" "disabled")))
      (princ (format "Colors Enabled: %s\n" (if editorconfig-ext-use-colors "yes" "no")))
      (princ "\nCurrent Buffer Status:\n")
      (when buffer-file-name
        (let ((status (editorconfig-ext-get-status)))
          (princ (format "  File: %s\n" buffer-file-name))
          (princ (format "  Status: %s\n" (or status "N/A")))
          (princ (format "  Config File: %s\n" (or editorconfig-ext-config-found "None")))
          (princ (format "  Properties Applied: %s\n" (or editorconfig-ext-applied "None"))))))))

;; Legacy hook registration for compatibility
(add-hook 'editorconfig-after-apply-functions
          #'editorconfig-ext-track-application)

(provide 'editorconfig-ext)
