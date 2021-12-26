;;; anki-editor-test.el --- Testing anki-editor
;;
;; Copyright (C) 2021 Salomé Cheysson <salome@cheysson.fr>
;;
;; Description: Test anki-editor
;; Author: Salomé Cheysson
;;
;; Released under the GNU General Public License version 3.
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'ert)
(require 'org-element)

(defconst anki-editor-test-dir (expand-file-name
                                (file-name-directory
                                 (or load-file-name buffer-file-name))))
(defconst anki-editor-dir (expand-file-name
                           (concat anki-editor-test-dir "../")))

(unless (featurep 'anki-editor)
  (setq load-path (cons anki-editor-dir load-path))
  (require 'anki-editor))

(defmacro with-org-file (f &rest body)
  "Create temporary buffer with contents from file F to execute BODY."
  (declare (indent 1))
  (let ((f-var (make-symbol "f")))
    `(let* ((,f-var (expand-file-name ,f anki-editor-test-dir)))
       (with-temp-buffer
         (insert-file-contents ,f-var t)
         (setq default-directory (file-name-directory ,f-var))
         (org-mode)
         ,@body))))

(defmacro at-headline (path &rest body)
  "Execute BODY at the headline with PATH."
  (declare (indent 1))
  (let ((path-var (make-symbol "path")))
        `(let ((,path-var ,path))
           (save-excursion
             (goto-char (point-min))
             (cl-loop for i from 1 to (length ,path-var)
                      for p in ,path-var
                      do (re-search-forward
                          (rx line-start
                              (literal (make-string i ?*))
                              (+ (any space)) (literal p))))
             ,@body))))


(ert-deftest export-string ()
  (with-org-file "fixtures/sample-cards.org"
    (at-headline '("Simple card" "Front")
      (let* ((el (org-element-at-point))
             (beg (org-element-property :contents-begin el))
             (end (org-element-property :contents-end el)))
        (should
         (equal
          "<p>\nThe <b>front</b> from a <i>simple</i> card.\n</p>\n"
          (anki-editor--export-string beg end t))))
    (at-headline '("Card with attachment" "Front")
      (let* ((el (org-element-at-point))
             (beg (org-element-property :contents-begin el))
             (end (org-element-property :contents-end el)))
        (should
         (string-match
          (rx
           (literal "<img")
           (+ (not ?>))
           "src=\"file://"
           (+ (not "\""))
           "/testing/fixtures/data/c5/27faa2-770a-4be3-9d74-ef8ec909e5ed/unicorn.png")
          (anki-editor--export-string beg end t))))))))


(provide 'anki-editor-tests)
;;; anki-editor-test.el ends here
