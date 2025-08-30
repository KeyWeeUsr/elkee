;;; elkee-tests.el -- tests for elkee

;;; Code:

(require 'ert)
(require 'elkee)

(defconst elkee-kdbx4-dummy
  (string-join
   '("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"
     "SgAAAAA=")
   "")
  "Base64-encoded KDBX v4 dummy DB. Pass='dummy'.")

(defmacro with-dummy-db (db &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (set-buffer-multibyte nil)
     (insert (cond ((eq ,db 'kdbx4) elkee-kdbx4-dummy)
                   (t (error "Wrong db type: %s" `,db))))
     (base64-decode-region (point-min) (point-max))
     (goto-char (point-min))

     (progn ,@body)))

(ert-deftest elkee-kdbx4-signature-parse-buffer ()
  (with-dummy-db 'kdbx4
    (should (equal (elkee-parse-signature-buffer (current-buffer))
                   elkee-signature))))

(ert-deftest elkee-kdbx4-signature-parse-buffer-destructive ()
  (with-dummy-db 'kdbx4
    (let ((old-len (length (buffer-string))))
      (should (equal (elkee-parse-signature-buffer (current-buffer) t)
                     elkee-signature))
      (should (= (- old-len (length elkee-signature))
                 (length (buffer-string)))))))

(ert-deftest elkee-kdbx4-signature-parse-buffer-offset ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (dolist (item '(#x3F #x40 #x41 #x42 #x43 #x44
                    #x45 #x46 #x47 #x48 #x49 #x4A))
      (insert item))
    (let ((old-len (length (buffer-string))))
      (should (equal (elkee-parse-signature-buffer
                      (current-buffer) nil (+ (point-min) 2))
                     [#x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48]))
      (should (= (- old-len (length (buffer-string))))))))

(ert-deftest elkee-kdbx4-signature-parse-buffer-offset-destructive ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (dolist (item '(#x3F #x40 #x41 #x42 #x43 #x44
                    #x45 #x46 #x47 #x48 #x49 #x4A))
      (insert item))
    (let ((old-len (length (buffer-string))))
      (should (equal (elkee-parse-signature-buffer
                      (current-buffer) t (+ (point-min) 2))
                     [#x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48]))
      (should (equal (string-to-vector (buffer-string))
                     [#x3F #x40 #x49 #x4A])))))

(provide 'elkee-tests)
;;; elkee-tests.el ends here
