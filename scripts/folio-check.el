;;; folio-check.el --- CI checks for Folio -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers invoked by the shell scripts in this directory.

;;; Code:

(require 'checkdoc)
(require 'ert)

;; Load the libraries whose macros appear in the sources, so their own
;; `declare' specs are registered.  Without `cl-lib', for instance,
;; `cl-letf' bodies get measured against the fallback rule.
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

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

(defun folio-check--source-files ()
  "Return absolute paths of the Folio package source files.
Globbed rather than listed so a second source file cannot silently
escape byte compilation the way a hand-maintained list allows."
  (directory-files folio-check--root t "\\`folio.*\\.el\\'"))

(defun folio-check-parens ()
  "Check balanced parentheses in every Folio Elisp file."
  (dolist (file (folio-check--elisp-files))
    (with-temp-buffer
      (insert-file-contents file)
      (check-parens)))
  "Parentheses are balanced.")

(defun folio-check--register-indent-declaration (form)
  "Register the `lisp-indent-function' spec FORM declares, if it declares one."
  (when (and (proper-list-p form)
             (memq (car form) '(defmacro cl-defmacro))
             (symbolp (nth 1 form)))
    (let ((declaration (seq-find (lambda (subform)
                                   (and (consp subform)
                                        (eq (car subform) 'declare)))
                                 (nthcdr 3 form))))
      (when-let* ((spec (assq 'indent (cdr declaration))))
        (put (nth 1 form) 'lisp-indent-function (cadr spec))))))

(defun folio-check--register-indent-form (form)
  "Register `lisp-indent-function' specs declared anywhere within FORM.
Walks FORM recursively so macros wrapped in conditionals are found too.
The spine is walked iteratively and dotted pairs are tolerated, so
quoted test data cannot abort the scan."
  (when (consp form)
    (folio-check--register-indent-declaration form)
    (while (consp form)
      (folio-check--register-indent-form (car form))
      (setq form (cdr form)))))

(defun folio-check--register-indent-specs (files)
  "Register indentation specs declared by macros defined in FILES.
The indentation check never evaluates the sources, so `declare' forms
inside `defmacro' would otherwise be invisible and macro call sites
would be measured against Emacs' fallback rule instead of the rule the
macro actually declares."
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (folio-check--register-indent-form (read (current-buffer))))
        (end-of-file nil)
        ;; A file we cannot fully read still contributes whatever it
        ;; declared before the unreadable form; the indentation check
        ;; itself will report the real problem.
        (error nil)))))

(defun folio-check-indent (&optional fix)
  "Check or fix indentation in every Folio Elisp file.
When FIX is non-nil, rewrite files instead of failing."
  (folio-check--register-indent-specs (folio-check--elisp-files))
  (let (bad-files)
    (dolist (file (folio-check--elisp-files))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((original (buffer-string)))
          (let ((emacs-lisp-mode-hook nil))
            (emacs-lisp-mode))
          ;; Keep the result independent of host and user defaults.
          (setq-local indent-tabs-mode nil)
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
        ;; `byte-compile-error-on-warn' makes warnings fail the check;
        ;; without it `byte-compile-file' succeeds and real defects (a macro
        ;; used before its definition, a misdeclared optional dependency)
        ;; scroll past a passing run.
        (let ((byte-compile-dest-file-function (lambda (_file) destination))
              (byte-compile-error-on-warn t))
          (dolist (file (folio-check--source-files))
            (unless (byte-compile-file file)
              (error "Byte compilation failed: %s"
                     (file-relative-name file folio-check--root)))))
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
