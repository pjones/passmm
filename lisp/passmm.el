;;; passmm.el -- A minor mode for pass (Password Store).  -*- lexical-binding: t -*-

;; Copyright (C) 2016 Peter Jones <pjones@devalot.com>
;;
;; Author: Peter Jones <pjones@devalot.com>
;; URL: https://github.com/pjones/passmm
;; Version: 0.1.0
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; FIXME:

;;; License:
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:
(require 'dired)

(defgroup passmm nil
  "A minor mode for pass (Password Store)."
  :version "0.1.0"
  :prefix "passmm-"
  :group 'applications)

(defcustom passmm-store-directory
  (or (getenv "PASSWORD_STORE_DIR") "~/.password-store")
  "The directory pass uses to store passwords in."
  :type 'string
  :group 'passmm)

(defcustom passmm-password-length 15
  "Length of generated passwords."
  :type 'integer
  :group 'passmm)


;;; Nothing interesting after this point.
(defvar passmm-buffer-name "*passwords*"
  "Name to use for the dired buffer.")

(defvar passmm-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-p g") 'passmm-generate-password)
    (define-key map (kbd "C-c C-p +") 'passmm-generate-password)
    map)
  "Default keymap for passmm.")

;;;###autoload
(defun passmm-list-passwords ()
  "List all passwords using a `dired' buffer.

The created `dired' buffer will have `passmm' running inside it.
If a `passm' buffer already exists it is made to be the current
buffer and refreshed."
  (interactive)
  (let ((buf (or (get-buffer passmm-buffer-name)
                 (dired-noselect (expand-file-name passmm-store-directory)
                                 (concat dired-listing-switches " -R")))))
    (if (string= (buffer-name buf) passmm-buffer-name)
        (with-current-buffer buf
          (revert-buffer))
      (with-current-buffer buf
        (rename-buffer passmm-buffer-name)
        (passmm-mode t)))
    (switch-to-buffer buf)))

(defun passmm-generate-password (&optional ask-dir)
  "Generate a password entry after asking for its name.

If ASK-DIR is non-nil then you'll be prompted for the name of
directory to store the entry in.  Otherwise it's taken from the
current directory in the `dired' buffer."
  (interactive "P")
  (let* ((store (expand-file-name passmm-store-directory))
         (adir (if (and (string= major-mode "dired-mode") (not ask-dir))
                   (dired-current-directory)
                 (read-directory-name "New Password Location: " store)))
         (rdir (file-relative-name adir store))
         (name (read-string (concat "Password Name (in " rdir "): ")))
         (jump (lambda nil (dired-goto-file (concat adir name ".gpg")))))
    (passmm-pass jump "generate" (concat (file-name-as-directory rdir) name)
                 (number-to-string passmm-password-length))))

(defun passmm-pass (callback &rest args)
  "Run the pass program and invoke CALLBACK when it completes.
ARGS are given directly to pass unchanged.  (Note: CALLBACK is
invoked with the passmm/dired buffer active.)"
  (let ((p (apply 'start-process "pass" "*pass*" "pass" args)))
    (set-process-sentinel p
      (lambda (_process _event)
        (save-excursion
          (with-current-buffer (or (get-buffer passmm-buffer-name)
                                   (passmm-list-passwords))
            (revert-buffer)
            (and callback (funcall callback))))))))

(define-minor-mode passmm-mode
  "Add pass related features to `dired'."
  :lighter " pass"
  :group 'applications
  :keymap passmm-mode-map)

(provide 'passmm)
;;; passmm.el ends here
