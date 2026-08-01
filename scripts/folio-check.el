;;; folio-check.el --- CI checks for Folio -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers invoked by the shell scripts in this directory.

;;; Code:

(require 'checkdoc)
(require 'ert)

(defconst folio-check--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Absolute path to the Folio repository root.")

(defun folio-check--elisp-files ()
  "Return absolute paths of Folio source, test, and check files."
  (list (expand-file-name "folio.el" folio-check--root)
        (expand-file-name "tests/folio-test.el" folio-check--root)
        (expand-file-name "scripts/folio-check.el" folio-check--root)))

(defun folio-check-parens ()
  "Check balanced parentheses in every Folio Elisp file."
  (dolist (file (folio-check--elisp-files))
    (with-temp-buffer
      (insert-file-contents file)
      (check-parens)))
  "Parentheses are balanced.")

(defun folio-check-indent (&optional fix)
  "Check or fix indentation in every Folio Elisp file.
When FIX is non-nil, rewrite files instead of failing."
  (put 'folio--with-entry-at-point 'lisp-indent-function 1)
  (let (bad-files)
    (dolist (file (folio-check--elisp-files))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((original (buffer-string)))
          (let ((emacs-lisp-mode-hook nil))
            (emacs-lisp-mode))
          (indent-region (point-min) (point-max))
          (unless (string-equal original (buffer-string))
            (if fix
                (write-region (point-min) (point-max) file nil 'silent)
              (push (file-relative-name file folio-check--root) bad-files))))))
    (when bad-files
      (error "Indentation check failed: %S" (nreverse bad-files))))
  "Indentation looks good.")

(defun folio-check-byte-compile ()
  "Byte-compile Folio without leaving generated files in the repository."
  (let ((destination (make-temp-file "folio-" nil ".elc")))
    (unwind-protect
        (let ((byte-compile-dest-file-function (lambda (_file) destination)))
          (unless (byte-compile-file (expand-file-name "folio.el" folio-check--root))
            (error "Byte compilation failed")))
      (when (file-exists-p destination)
        (delete-file destination))))
  "Byte compilation passed.")

(defun folio-check-checkdoc ()
  "Run Checkdoc against the Folio package source."
  (unless (checkdoc-file (expand-file-name "folio.el" folio-check--root))
    (error "Checkdoc failed"))
  "Checkdoc passed.")

(defun folio-check-ert ()
  "Load and run all Folio ERT tests."
  (add-to-list 'load-path folio-check--root)
  (load (expand-file-name "folio.el" folio-check--root) nil t)
  (load (expand-file-name "tests/folio-test.el" folio-check--root) nil t)
  (let ((stats (ert-run-tests-batch "^folio")))
    (unless (zerop (ert-stats-completed-unexpected stats))
      (error "%d of %d Folio tests failed"
             (ert-stats-completed-unexpected stats)
             (ert-stats-total stats)))
    (format "%d Folio tests passed" (ert-stats-total stats))))

(provide 'folio-check)

;;; folio-check.el ends here
