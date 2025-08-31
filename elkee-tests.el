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

(ert-deftest elkee-util-intparsing-matrix ()
  (let ((matrix `((:name "Check nil"
                   :data nil :result nil
                   :error wrong-type-argument
                   :error-args ((,elkee-16-bit . (arrayp nil))
                                (,elkee-32-bit . (arrayp nil))
                                (,elkee-64-bit . (arrayp nil))))
                  (:name "Check string"
                   :data "" :result nil
                   :error args-out-of-range
                   :error-args ((,elkee-16-bit . ("" 0))
                                (,elkee-32-bit . ("" 0))
                                (,elkee-64-bit . ("" 0))))
                  (:name "Check list"
                   :data (1 2 3) :result nil
                   :error wrong-type-argument
                   :error-args ((,elkee-16-bit . (arrayp (1 2 3)))
                                (,elkee-32-bit . (arrayp (1 2 3)))
                                (,elkee-64-bit . (arrayp (1 2 3)))))
                  (:name "Check nil vector"
                   :data ,(make-vector 0 nil) :result nil
                   :error args-out-of-range
                   :error-args ((,elkee-16-bit . ([] 0))
                                (,elkee-32-bit . ([] 0))
                                (,elkee-64-bit . ([] 0))))
                  (:name "Check empty vector"
                   :data ,(make-vector 1 nil) :result nil
                   :error wrong-type-argument
                   :error-args
                   ((,elkee-16-bit . (number-or-marker-p nil))
                    (,elkee-32-bit . (number-or-marker-p nil))
                    (,elkee-64-bit . (number-or-marker-p nil))))
                  (:name "Check vector 1"
                   :data ,(make-vector 1 0) :result nil
                   :error args-out-of-range
                   :error-args ((,elkee-16-bit . ([0] 1))
                                (,elkee-32-bit . ([0] 1))
                                (,elkee-64-bit . ([0] 1))))
                  (:name "Check vector 2"
                   :data ,(make-vector 2 0) :result 0
                   :error args-out-of-range
                   :error-args ((,elkee-16-bit . nil)
                                (,elkee-32-bit . ([0 0] 2))
                                (,elkee-64-bit . ([0 0] 2))))
                  (:name "Check vector 3"
                   :data ,(make-vector 3 0) :result 0
                   :error args-out-of-range
                   :error-args ((,elkee-16-bit . nil)
                                (,elkee-32-bit . ([0 0 0] 3))
                                (,elkee-64-bit . ([0 0 0] 3))))
                  (:name "Check vector 4"
                   :data ,(make-vector 4 0) :result 0
                   :error args-out-of-range
                   :error-args ((,elkee-16-bit . nil)
                                (,elkee-32-bit . nil)
                                (,elkee-64-bit . ([0 0 0 0] 4))))
                  (:name "Check vector 5"
                   :data ,(make-vector 5 0) :result 0
                   :error args-out-of-range
                   :error-args ((,elkee-16-bit . nil)
                                (,elkee-32-bit . nil)
                                (,elkee-64-bit . ([0 0 0 0 0] 5))))
                  (:name "Check vector 6"
                   :data ,(make-vector 6 0) :result 0
                   :error args-out-of-range
                   :error-args ((,elkee-16-bit . nil)
                                (,elkee-32-bit . nil)
                                (,elkee-64-bit . ([0 0 0 0 0 0] 6))))
                  (:name "Check vector 7"
                   :data ,(make-vector 7 0) :result 0
                   :error args-out-of-range
                   :error-args ((,elkee-16-bit . nil)
                                (,elkee-32-bit . nil)
                                (,elkee-64-bit . ([0 0 0 0 0 0 0] 7))))
                  (:name "Check vector 8"
                   :data ,(make-vector 8 0) :result 0
                   :error args-out-of-range
                   :error-args ((,elkee-16-bit . nil)
                                (,elkee-32-bit . nil)
                                (,elkee-64-bit . nil)))))
        (bitness `(,elkee-16-bit
                   ,elkee-16-bit
                   ,elkee-32-bit)))
    (dolist (kind bitness)
      (dolist (case matrix)
        (let ((exp-err (plist-get case :error))
              (exp-err-args (alist-get kind (plist-get case :error-args)))
              result)
          (if exp-err-args
              (condition-case err
                  (progn
                    (setq result (elkee-read-uint (plist-get case :data) kind))
                    (should nil))
                (error
                 (should (equal (car err) exp-err))
                 (should (equal (cdr err) exp-err-args))))
            (setq result (elkee-read-uint (plist-get case :data) kind))
            (should (equal result (plist-get case :result)))))))))

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

(ert-deftest elkee-kdbx4-version-internal ()
  (with-dummy-db 'kdbx4
    (delete-char (length elkee-signature))
    (should (equal (elkee-parse-version-buffer (current-buffer) t)
                   '(4 0)))))

(ert-deftest elkee-kdbx4-headers-internal ()
  (with-dummy-db 'kdbx4
    (delete-char (+ (length elkee-signature)
                    (/ elkee-32-bit elkee-byte)))
    (should (equal
             (elkee-parse-headers-buffer (current-buffer) 4 0 t)
             `((kdf-parameters
                . ((V . 19)
                   (S . ,(apply 'string
                                '(#x24 #xD2 #x89 #xF7 #xE6 #xB8 #xEE #xE2
                                  #x0A #xF9 #x11 #xF3 #x0B #xA2 #xFC #x3D
                                  #xD7 #xF6 #xEF #x42 #x2E #x25 #xA3 #x70
                                  #x63 #x39 #x1C #x8C #x26 #x77 #x02 #x44)))
                   (P . 2)
                   (M . 67108864)
                   (I . 11)
                   (kdf . argon2)
                   (version . ,(apply 'string '(#x00 #x01)))))
               (compression . gzip)
               (cipher . aes256)
               (end . (#x0D #x0A #x0D #x0A))
               (kdf-parameters-raw
                . (#x00 #x01 #x42 #x05 #x00 #x00 #x00 #x24
                   #x55 #x55 #x49 #x44 #x10 #x00 #x00 #x00
                   #xEF #x63 #x6D #xDF #x8C #x29 #x44 #x4B
                   #x91 #xF7 #xA9 #xA4 #x03 #xE3 #x0A #x0C
                   #x05 #x01 #x00 #x00 #x00 #x49 #x08 #x00
                   #x00 #x00 #x0B #x00 #x00 #x00 #x00 #x00
                   #x00 #x00 #x05 #x01 #x00 #x00 #x00 #x4D
                   #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x04
                   #x00 #x00 #x00 #x00 #x04 #x01 #x00 #x00
                   #x00 #x50 #x04 #x00 #x00 #x00 #x02 #x00
                   #x00 #x00 #x42 #x01 #x00 #x00 #x00 #x53
                   #x20 #x00 #x00 #x00 #x24 #xD2 #x89 #xF7
                   #xE6 #xB8 #xEE #xE2 #x0A #xF9 #x11 #xF3
                   #x0B #xA2 #xFC #x3D #xD7 #xF6 #xEF #x42
                   #x2E #x25 #xA3 #x70 #x63 #x39 #x1C #x8C
                   #x26 #x77 #x02 #x44 #x04 #x01 #x00 #x00
                   #x00 #x56 #x04 #x00 #x00 #x00 #x13 #x00
                   #x00 #x00 #x00))
               (encryption-iv
                . (#x91 #x71 #x6B #x53 #x72 #x0B #x77 #x80
                   #xFB #xC2 #x5F #xDA #x6F #x44 #x46 #x74))
               (master-seed
                . (#x84 #xF8 #x8E #x88 #xAF #xFE #x19 #x38
                   #x8F #xD1 #x3C #x33 #x7E #x22 #x5E #x8F
                   #x96 #x67 #x23 #x7F #x6E #x9F #x3B #xB2
                   #x56 #x3A #xCE #x6A #x4A #x68 #x5D #x1B))
               (compression-flags . (#x01 #x00 #x00 #x00))
               (cipher-id
                . (#x31 #xC1 #xF2 #xE6 #xBF #x71 #x43 #x50
                   #xBE #x58 #x05 #x21 #x6A #xFC #x5A #xFF)))))))

(ert-deftest elkee-compute-composite-password-part ()
  (let ((part (elkee-compute-composite-password-part "dummy")))
    (should (string= (string-join '("b5a2c96250612366ea272ffac6d9744a"
                                    "af4b45aacd96aa7cfcb931ee3b558259"))
                     (apply 'concat (mapcar (lambda (x) (format "%02x" x))
                                            part))))))

(ert-deftest elkee-compute-composite-key-password-no-keyfile ()
  (let ((key (elkee-compute-composite-key "dummy" nil)))
    (should (string= (string-join
                      '("1a2a45fb72ab2d3a465a49675921b7e1"
                        "2a36ca3aa06e4060278ac1046354da9d") "")
                     (apply 'concat (mapcar (lambda (x) (format "%02x" x))
                                            key))))))

(ert-deftest elkee-kdbx4-transformed-key-internal ()
  (with-dummy-db 'kdbx4
    (let* ((password "dummy")
           (headers `((kdf-parameters
                . ((S . ,(apply 'string
                                '(#x24 #xD2 #x89 #xF7 #xE6 #xB8 #xEE #xE2
                                  #x0A #xF9 #x11 #xF3 #x0B #xA2 #xFC #x3D
                                  #xD7 #xF6 #xEF #x42 #x2E #x25 #xA3 #x70
                                  #x63 #x39 #x1C #x8C #x26 #x77 #x02 #x44)))
                   (P . 2)
                   (M . 67108864)
                   (I . 11)
                   (kdf . argon2))))))
      (should (equal (elkee-compute-transformed-key
                      (elkee-compute-composite-key password nil)
                      (alist-get 'kdf-parameters headers))
                     '(#x87 #xCD #x99 #xE9 #xE3 #x84 #xCB #xC3
                       #xC3 #x9F #xC6 #x7F #xDF #x1E #xC5 #x52
                       #xB2 #xAF #x38 #x00 #x22 #x24 #x67 #xE1
                       #x00 #x7B #xF7 #x20 #x98 #x04 #xD1 #xF4))))))

(ert-deftest elkee-kdbx4-master-key-internal ()
  (with-dummy-db 'kdbx4
    (let* ((password "dummy")
           (headers `((kdf-parameters
                . ((S . ,(apply 'string
                                '(#x24 #xD2 #x89 #xF7 #xE6 #xB8 #xEE #xE2
                                  #x0A #xF9 #x11 #xF3 #x0B #xA2 #xFC #x3D
                                  #xD7 #xF6 #xEF #x42 #x2E #x25 #xA3 #x70
                                  #x63 #x39 #x1C #x8C #x26 #x77 #x02 #x44)))
                   (P . 2)
                   (M . 67108864)
                   (I . 11)))
               (master-seed
                . (#x84 #xF8 #x8E #x88 #xAF #xFE #x19 #x38
                   #x8F #xD1 #x3C #x33 #x7E #x22 #x5E #x8F
                   #x96 #x67 #x23 #x7F #x6E #x9F #x3B #xB2
                   #x56 #x3A #xCE #x6A #x4A #x68 #x5D #x1B))))
           (expected
            '(#x83 #x78 #x91 #x41 #x2B #x1D #xB1 #xE1
              #x06 #xDB #x92 #xAE #xEC #x71 #xD9 #x8D
              #x38 #xDD #x0C #xD6 #x23 #xE5 #xB5 #x37
              #x5D #x58 #x6C #xDA #xCE #xAF #x18 #x12)))
      (should (equal (elkee-compute-master-key
                      (alist-get 'master-seed headers)
                      (elkee-compute-transformed-key
                       (elkee-compute-composite-key password nil)
                       (alist-get 'kdf-parameters headers)))
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (dolist (item expected)
                         (insert item))
                       (buffer-string)))))))

(ert-deftest elkee-kdbx4-decrypt-block ()
  (let ((password "dummy")
        (expected
         (string-join
          '("1f8b0800000000000003a5574d6fe34418eeee8250fa2bac684fac8a93b4db66"
            "2bc74b1a3bd96cf3d5d8e976178968e2bc4ddcdae3acc7ded68bf6c6990be284"
            "387242e2c45e1067e0823870e186b8213821fe0033fe883fe2745bb0d4d47edf"
            "679e773cf3cc33e35b6f6d6c6cdca17fb7dfa73f671f6ffcfeeaddb7fffae9c3"
            "4ffff9fceeb3cdf309f783f7fab7dae4eec91fad3f7ffe45fcbbf9f517df7cd0"
            "d37abfdeaa775e7f5bfff1d99def5a5f7df9c9e13bb7bf9f999f6db04b787869"
            "1adc0bb0896ee15ab1fc5ea9c801d6aca98e67b5e2486d6e558b1c71109e22c3"
            "c2502b7a408a0fc54de11060800869ea06889b05a10b0ea2ff0b420b30d8c8b1"
            "6c31449c34043e0e3288841c3441047ac80451724dd313f8542c0b6acc119ec1"
            "547c31dfbeb73ddba9d3ab966e1221922d25209aad2f1cfa62e29455e1a67124"
            "6e9e84ad697e75fd1ca0cf03a7c8359c11019bf590cf0b4678729022ce07b1f6"
            "5da4630730c21a3cd2091d504f421e11b777ef0bfc9a1c6bd6b00ccbe64306e2"
            "807d085e44fb78be97a8bd92ce6933044ddc2aaf60593807ddb46c0df2f041c2"
            "6f0126ededc0b61cd0a2792808e1b3aa3b54624d641010f8542c898a062b035c"
            "869358a6ca0bcb9e8aaaedc6d06534453bec6419692489e8d15f92c10431f666"
            "7cdeab090d974e8fd9d62c4c8249a143e769061ce858c6686240d4b5d5781a3d"
            "1ab525b19e7bd56ac9e63e30dd365f7bab79d64ac68eeda9602e0c44dfac655b"
            "eee28ab279e8352cf99db802c8783a54470a18744061faa6beac622306d55a1c"
            "eb44a7a37a1d8e2c9ab184abac8b2edb0e98442c97043e1b4be314fd2588bb95"
            "07e59dfbbb49ac1f6750051c873a2f895ef7e993aab47d118d4b361b6b897951"
            "204b56d6bf2b508bf6c471a7aea8e36e5f6a37dbb224f02c16648f91e182a8b8"
            "989340e32a0fb8f2ee7e656fbfbcc3554a951daed555053ec0f8bcfc92385ba2"
            "29f5c78adc18caaa32964f067d4596c6ad617f3458adf65129bcb6727ea2ebd5"
            "35eb1e0e4e1a6389167e3a50dbfdde586d77e5f1602837e5a1dc6bc8abd5cb94"
            "3c9f5be053c3c8966d7033b42c27d8d6a2592f08fe5adaa9424f3eb31405eea9"
            "9de3d9594febe047174c2dd1522b08bef13006818f3dc8f7063e7c21ea003e17"
            "ed4970eb8755dd0cec23546a97eec6a7ba86987fb09c789eb2ec5c48d0ba6143"
            "1c4aafb1542aae55d73420644d95443268215f2e74ba5257d91389043236cae8"
            "31488e089a41c372b123d2094a3c85fdb28217cb378b6c3698d9780c8536a1d5"
            "e80166e9aa89800f08f7dbbaeb58aab700059ebbf40804e12405ce1b2545ec1a"
            "0673a85430015400d9da9caed214328e6e46231d3b8a6f77d7f69f001d568c6e"
            "43551ed7a1baf7c43b3d6ae806a9f6c9ded111deada754b9545d2925ba8240b7"
            "63985195e3697c5aa0e103a49de784fbf4c868eb53a0db611452d18c2cef6309"
            "ff4f0d6744dcbbdcbb52c4ff41c52919a7f9b332be5ac76f12f2aa92339dcb51"
            "724aca747f70ec4844a10bfa7e92f0baa5b15b26709826938e47d9920c3974d1"
            "296895910b0f3130ad15d93a2a8acff5a6070b73b2ad572bb2aed56e56c93fc3"
            "e574bceb714e90ba091bd5610ed7dc7116fb3c6fd08135e61671f6aba56ae986"
            "c4e10932879d7e51b02cf6b3eb39532eb1f489c88c1207bb42f8f5a1da089353"
            "b0fb935397049260725a978a5a062e96762fda9774f9e830129a1bbfb410fa91"
            "b63cd848f4c044e7b93f39a3f3edaf697a30f47742360a89efbe7f01e9d38397"
            "910e0000")
          ""))
        (kdbx (make-elkee-database)))
    (with-dummy-db 'kdbx4
      (elkee-parse-signature-buffer (current-buffer) t)
      (elkee-parse-version-buffer (current-buffer) t)
      (setf (elkee-database-headers kdbx)
            (elkee-parse-headers-buffer (current-buffer) 4 0 t))
      (setf (elkee-database-key kdbx)
            (elkee-compute-master-key
             (alist-get 'master-seed (elkee-database-headers kdbx))
             (elkee-compute-transformed-key
              (elkee-compute-composite-key password nil)
              (alist-get 'kdf-parameters (elkee-database-headers kdbx)))))
      (delete-char (+ 32 32))
      (should
       (string= expected
                (apply 'concat (mapcar (lambda (x) (format "%02x" x))
                                       (car (elkee-aes256-decrypt-buffer
                                             kdbx (current-buffer) t)))))))))

(ert-deftest elkee-bad-signature ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((what '(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
                  #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
      (dolist (item what)
        (insert item))
      (condition-case err
          (progn (elkee-read-buffer "" nil) (should nil))
        (error (should (eq (car err) 'elkee-unsupported-file)))))))

(ert-deftest keepass-kdbx4-version ()
  (with-dummy-db 'kdbx4
    (should (equal (elkee-database-version (elkee-read-buffer "dummy" nil))
                   '(4 0)))))

(ert-deftest elkee-kdbx4-headers ()
  (with-dummy-db 'kdbx4
    (should (equal
             (elkee-database-headers (elkee-read-buffer "dummy" nil))
             `((kdf-parameters
                . ((V . 19)
                   (S . ,(apply 'string
                                '(#x24 #xD2 #x89 #xF7 #xE6 #xB8 #xEE #xE2
                                  #x0A #xF9 #x11 #xF3 #x0B #xA2 #xFC #x3D
                                  #xD7 #xF6 #xEF #x42 #x2E #x25 #xA3 #x70
                                  #x63 #x39 #x1C #x8C #x26 #x77 #x02 #x44)))
                   (P . 2)
                   (M . 67108864)
                   (I . 11)
                   (kdf . argon2)
                   (version . ,(apply 'string '(#x00 #x01)))))
               (compression . gzip)
               (cipher . aes256)
               (end . (#x0D #x0A #x0D #x0A))
               (kdf-parameters-raw
                . (#x00 #x01 #x42 #x05 #x00 #x00 #x00 #x24
                   #x55 #x55 #x49 #x44 #x10 #x00 #x00 #x00
                   #xEF #x63 #x6D #xDF #x8C #x29 #x44 #x4B
                   #x91 #xF7 #xA9 #xA4 #x03 #xE3 #x0A #x0C
                   #x05 #x01 #x00 #x00 #x00 #x49 #x08 #x00
                   #x00 #x00 #x0B #x00 #x00 #x00 #x00 #x00
                   #x00 #x00 #x05 #x01 #x00 #x00 #x00 #x4D
                   #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x04
                   #x00 #x00 #x00 #x00 #x04 #x01 #x00 #x00
                   #x00 #x50 #x04 #x00 #x00 #x00 #x02 #x00
                   #x00 #x00 #x42 #x01 #x00 #x00 #x00 #x53
                   #x20 #x00 #x00 #x00 #x24 #xD2 #x89 #xF7
                   #xE6 #xB8 #xEE #xE2 #x0A #xF9 #x11 #xF3
                   #x0B #xA2 #xFC #x3D #xD7 #xF6 #xEF #x42
                   #x2E #x25 #xA3 #x70 #x63 #x39 #x1C #x8C
                   #x26 #x77 #x02 #x44 #x04 #x01 #x00 #x00
                   #x00 #x56 #x04 #x00 #x00 #x00 #x13 #x00
                   #x00 #x00 #x00))
               (encryption-iv
                . (#x91 #x71 #x6B #x53 #x72 #x0B #x77 #x80
                   #xFB #xC2 #x5F #xDA #x6F #x44 #x46 #x74))
               (master-seed
                . (#x84 #xF8 #x8E #x88 #xAF #xFE #x19 #x38
                   #x8F #xD1 #x3C #x33 #x7E #x22 #x5E #x8F
                   #x96 #x67 #x23 #x7F #x6E #x9F #x3B #xB2
                   #x56 #x3A #xCE #x6A #x4A #x68 #x5D #x1B))
               (compression-flags . (#x01 #x00 #x00 #x00))
               (cipher-id
                . (#x31 #xC1 #xF2 #xE6 #xBF #x71 #x43 #x50
                   #xBE #x58 #x05 #x21 #x6A #xFC #x5A #xFF)))))))

(ert-deftest elkee-kdbx4-transformed-key ()
  (with-dummy-db 'kdbx4
    (let* ((password "dummy")
           (headers (elkee-database-headers
                     (elkee-read-buffer "dummy" nil))))
      (should (equal (elkee-compute-transformed-key
                      (elkee-compute-composite-key password nil)
                      (alist-get 'kdf-parameters headers))
                     '(#x87 #xCD #x99 #xE9 #xE3 #x84 #xCB #xC3
                       #xC3 #x9F #xC6 #x7F #xDF #x1E #xC5 #x52
                       #xB2 #xAF #x38 #x00 #x22 #x24 #x67 #xE1
                       #x00 #x7B #xF7 #x20 #x98 #x04 #xD1 #xF4))))))

(provide 'elkee-tests)
;;; elkee-tests.el ends here
