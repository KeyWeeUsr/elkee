;;; elkee.el --- Keepass client -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, keepass, client
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (kaesar "20230626.2314"))
;; Homepage: https://github.com/KeyWeeUsr/elkee

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TBD

;;; Code:
(require 'elchacha)
(require 'kaesar)

(defconst elkee-signature [#x03 #xD9 #xA2 #x9A #x67 #xFB #x4B #xB5]
  "Expected KDBX file signature.")

(defconst elkee-cipher-aes256-signature
  [#x31 #xC1 #xF2 #xE6 #xBF #x71 #x43 #x50
   #xBE #x58 #x05 #x21 #x6A #xFC #x5A #xFF])

(defconst elkee-cipher-twofish-signature
  [#xAD #x68 #xF2 #x9F #x57 #x6F #x4B #xB9
   #xA3 #x6A #xD4 #x7A #xF9 #x65 #x34 #x6C])

(defconst elkee-cipher-chacha20-signature
  [#xD6 #x03 #x8A #x2B #x8B #x6F #x4C #xB5
   #xA5 #x24 #x33 #x9A #x31 #xDB #xB5 #x9A])

(defconst elkee-kdf-argon2-signature
  '(#xEF #x63 #x6D #xDF #x8C #x29 #x44 #x4B
    #x91 #xF7 #xA9 #xA4 #x03 #xE3 #x0A #x0C))

(defconst elkee-kdf-argon2id-signature
  '(#x9E #x29 #x8B #x19 #x56 #xDB #x47 #x73
    #xB2 #x3D #xFC #x3E #xC6 #xF0 #xA1 #xE6))

(defconst elkee-kdf-aeskdf-signature
  '(#xC9 #xD9 #xF3 #x9A #x62 #x8A #x44 #x60
    #xBF #x74 #x0D #x08 #xC1 #x8A #x4F #xEA))

(defconst elkee-byte 8
  "Bits in a byte.")

(defconst elkee-16-bit 16
  "Bitness of 16-bit int.")

(defconst elkee-32-bit 32
  "Bitness of 32-bit int.")

(defconst elkee-64-bit 64
  "Bitness of 64-bit int.")

(define-error 'elkee-error "Generic Elkee error")

(define-error 'elkee-unsupported-file
  "Unsupported file, mismatched signature."
  'elkee-error)

(defun elkee-read-uint (data bitness &optional offset)
  "Read an unsigned integer of BITNESS from DATA at OFFSET."
  (let ((offset (or offset 0))
        (result 0))
    (dotimes (idx (/ bitness elkee-byte))
      (setq result
            ;; todo: lsh -> ash crashes tests
            (+ result (with-no-warnings
                        (lsh (aref data (+ idx offset))
                             (* idx elkee-byte))))))
    result))

(defun elkee-read-sint (data bitness &optional offset)
  "Read a signed integer of BITNESS from DATA at OFFSET."
  (let ((val (elkee-read-uint data bitness offset)))
    (if (>= val (/ (expt 2 bitness) 2)) (- val (expt 2 bitness)) val)))

(defun elkee-parse-signature-buffer (buff &optional delete start-pos)
  "Parse buffer BUFF for KDBX signature.
Optional argument DELETE destroys buffer data while reading.
Optional argument START-POS marks position to start processing from."
  (with-current-buffer buff
    (save-excursion
      (unless start-pos
        (setq start-pos (point-min)))

      (let ((header (make-vector elkee-byte nil)))
        (goto-char start-pos)
        (dotimes (idx (length elkee-signature))
          (aset header idx (char-after))
          (forward-char 1))
        (when delete
          (delete-region start-pos (+ start-pos (length elkee-signature))))
        header))))

(defun elkee-parse-version-buffer (buff &optional delete start-pos)
  "Parse KDBX signature from buffer BUFF.
Optional argument DELETE destroys buffer data while reading.
Optional argument START-POS marks position to start processing from."
  (with-current-buffer buff
    (save-excursion
      (unless start-pos
        (setq start-pos (point-min)))

      (let* ((size (/ elkee-16-bit elkee-byte))
             (minor-bytes (make-vector size nil))
             (major-bytes (make-vector size nil))
             last-pos version)
        (goto-char start-pos)
        (dotimes (idx size)
          (aset minor-bytes idx (char-after))
          (forward-char 1))
        (push (elkee-read-uint minor-bytes elkee-16-bit) version)
        (when delete
          (delete-region start-pos (+ start-pos size))
          (goto-char start-pos))

        (dotimes (idx size)
          (aset major-bytes idx (char-after))
          (forward-char 1))
        (push (elkee-read-uint major-bytes elkee-16-bit) version)
        (when delete
          (delete-region start-pos (+ start-pos size)))
        version))))

(defun elkee-parse-headers-buffer (buff maj min &optional delete start-pos)
  "Parsing KDBX headers from BUFF based on github.com/dlech/KeePass2.x .
Argument MAJ specifies KDBX major version.
Argument MIN specifies KDBX minor version.
Optional argument DELETE destroys buffer data while reading.
Optional argument START-POS marks position to start processing from.
Ref: VS2022/KeePassLib/Serialization/KdbxFile.Read.cs#L319."
  (let (headers)
    (let* ((size (if (>= maj 4)
                     (/ elkee-32-bit elkee-byte)
                   (/ elkee-16-bit elkee-byte)))
           break)
      (with-current-buffer buff
        (save-excursion
          (unless start-pos
            (setq start-pos (point-min)))

          (goto-char start-pos)
          (while (not break)
            (let ((data-length-raw (make-vector size nil)) data-length
                  id-enum id data)
              (setq id-enum (char-after))
              (forward-char 1)

              (setq id (cond ((= id-enum 0) 'end)
                             ((= id-enum 1) 'comment)
                             ((= id-enum 2) 'cipher-id)
                             ((= id-enum 3) 'compression-flags)
                             ((= id-enum 4) 'master-seed)
                             ((= id-enum 5) 'transform-seed)
                             ((= id-enum 6) 'transform-rounds)
                             ((= id-enum 7) 'encryption-iv)
                             ((= id-enum 8) 'protected-stream-key)
                             ((= id-enum 9) 'stream-start-bytes)
                             ((= id-enum 10) 'protected-stream-id)
                             ((= id-enum 11) 'kdf-parameters-raw)
                             ((= id-enum 12) 'public-custom-data)
                             (t (error "Unexpected ID %s" id-enum))))

              (dotimes (idx size)
                (aset data-length-raw idx (char-after))
                (forward-char 1))
              (if (>= maj 4)
                  (setq data-length
                        (elkee-read-uint data-length-raw elkee-32-bit))
                (setq data-length
                      (elkee-read-uint data-length-raw elkee-16-bit)))

              (when (eq id 'end) (setq break t) (setq data nil))

              (when (> data-length 0)
                (dotimes (_ data-length)
                  (setq data (nconc data `(,(char-after))))
                  (forward-char 1)))
              (setf (alist-get id headers) data)))
          (when delete
            (delete-region start-pos (point))))))

    (let* ((cipher-raw (vconcat (alist-get 'cipher-id headers)))
           (comp-flags-raw (vconcat (alist-get 'compression-flags headers)))
           (round-flags-raw (alist-get 'transform-rounds headers))
           (stream-id-raw (alist-get 'protected-stream-id headers)))
      (setf (alist-get 'cipher headers)
            (cond ((equal cipher-raw elkee-cipher-aes256-signature)
                   'aes256)
                  ((equal cipher-raw elkee-cipher-twofish-signature)
                   'twofish)
                  ((equal cipher-raw elkee-cipher-chacha20-signature)
                   'chacha20)
                  (t (error "Unhandled cipher signature: %S" cipher-raw))))
      ;; https://github.com/dlech/KeePass2.x/a6ecd54/KeePassLib/PwEnums.cs#L27
      ;; just an enum in a too large storage because of an arch instr
      ;; optimization projected onto file storage
      ;; https://stackoverflow.com/a/10218229/5994041
      (setf (alist-get 'compression headers)
            (let ((val (elkee-read-uint comp-flags-raw elkee-32-bit)))
              (cond ((= 0 val) 'none)
                    ((= 1 val) 'gzip)
                    (t (error "Unexpected compression enum: %s" val)))))
      (when (< maj 4)
        (setf (alist-get 'rounds headers)
              (progn (let* ((len (length round-flags-raw))
                            (arr (make-vector len nil)))
                       (dotimes (idx len)
                         (aset arr idx (nth idx round-flags-raw)))
                       (elkee-read-uint arr elkee-64-bit)))))
      (when (< maj 4)
        (setf (alist-get 'stream-id headers)
              (let ((val (progn (let* ((len (length stream-id-raw))
                                       (arr (make-vector len nil)))
                                  (dotimes (idx len)
                                    (aset arr idx (nth idx stream-id-raw)))
                                  (elkee-read-uint arr elkee-32-bit)))))
                (cond ((= 0 val) 'none)
                      ((= 1 val) 'arcfourvariant)
                      ((= 2 val) 'salsa20)
                      ((= 3 val) 'chacha20)
                      (t (error "Unexpected stream enum: %s" val))))))

      (when (>= maj 4)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (dolist (item (alist-get 'kdf-parameters-raw headers))
            (insert item))
          (goto-char (point-min))
          (progn (setf (alist-get 'version (alist-get 'kdf-parameters headers))
                       (buffer-substring-no-properties
                        (point-min) (+ (point-min) 2)))
                 (delete-char 2))
          (let (break new-item item-type item-key)
            (while (not break)
              (setq new-item nil)
              (goto-char (point-min))

              (progn (setf (alist-get 'type new-item) (char-after))
                     (setq item-type (char-after))
                     (delete-char 1))

              (progn (let ((key-length-raw
                            (make-vector (/ elkee-32-bit elkee-byte) nil))
                           key-length key)
                       (dotimes (idx (/ elkee-32-bit elkee-byte))
                         (aset key-length-raw idx (char-after))
                         (delete-char 1))
                       (setq key-length
                             (elkee-read-uint key-length-raw elkee-32-bit))
                       (dotimes (idx key-length)
                         (push (char-after) key)
                         (delete-char 1))
                       (setq item-key (concat (reverse key)))
                       (setf (alist-get 'key new-item) item-key)))

              (progn (let ((val-length-raw
                            (make-vector (/ elkee-32-bit elkee-byte) nil))
                           val-length val)
                       (dotimes (idx (/ elkee-32-bit elkee-byte))
                         (aset val-length-raw idx (char-after))
                         (delete-char 1))
                       (setq val-length
                             (elkee-read-uint val-length-raw elkee-32-bit))
                       (dotimes (idx val-length)
                         (push (char-after) val)
                         (delete-char 1))
                       (setq val (reverse val))

                       (if (not (string= item-key "$UUID"))
                           (setf (alist-get 'value new-item) (concat val))
                         (setq item-key "kdf")
                         (setf (alist-get 'key new-item) item-key)
                         (setf (alist-get 'value new-item)
                               (cond ((equal val elkee-kdf-argon2-signature)
                                      'argon2)
                                     (t (error "Unexpected KDF: %s" val)))))))

              ;; only peek, no delete
              (setf (alist-get 'next-byte new-item) (char-after))

              (setf (alist-get 'value new-item)
                    (cond ((= item-type #x04) (elkee-read-uint
                                               (alist-get 'value new-item)
                                               elkee-32-bit))
                          ((= item-type #x05) (elkee-read-uint
                                               (alist-get 'value new-item)
                                               elkee-64-bit))
                          ((= item-type #x08) (elkee-read-uint
                                               (alist-get 'value new-item)
                                               elkee-32-bit))
                          ((= item-type #x0C) (elkee-read-sint
                                               (alist-get 'value new-item)
                                               elkee-32-bit))
                          ((= item-type #x0D) (elkee-read-sint
                                               (alist-get 'value new-item)
                                               elkee-64-bit))
                          ((= item-type #x42) (alist-get 'value new-item))
                          ;; todo: as utf-8
                          ((= item-type #x18) (concat (alist-get 'value new-item)))
                          (t (error "Unexpected type %s" item-type))))

              (when (= (alist-get 'next-byte new-item) 0) (setq break t))

              (setf (alist-get
                     `,(intern (alist-get 'key new-item))
                     (alist-get 'kdf-parameters headers))
                    (alist-get 'value new-item)))))))
    headers))

(defun elkee-compute-composite-password-part (text &optional encoding)
  "Compute composite key's password part from TEXT at ENCODING (or utf-8)."
  (unless encoding (setq encoding 'utf-8))
  (secure-hash 'sha256 (encode-coding-string text encoding) nil nil t))

(defun elkee-compute-composite-keyfile-part (filepath)
  "Compute composite key's keyfile part from FILEPATH."
  (ignore filepath))

(defun elkee-compute-composite-key (password keyfile)
  "Compute composite key from PASSWORD and KEYFILE."
  (let ((bytes (append
                (when (and password (> (length password) 0))
                  (elkee-compute-composite-password-part password))
                (when keyfile
                  (elkee-compute-composite-keyfile-part keyfile)))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (dolist (item bytes) (insert item))
      (secure-hash 'sha256 (buffer-string) nil nil t))))

(defconst elkee-script-argon2
  (string-join
   '("printf %s $ARGON_KEY \\"
     "| xxd -r -p \\"
     "| argon2 \\"
     "    \"$(printf %s $ARGON_SALT | xxd -r -p)\" \\"
     "    -p $ARGON_P \\"
     "    -m $ARGON_M \\"
     "    -t $ARGON_I \\"
     "    -d -r")
   "\n"))

(defun elkee-compute-transformed-key (composite-key kdf-parameters)
  "Derive transformed key from COMPOSITE-KEY and KDF-PARAMETERS.
KDF-PARAMETERS' keys for KeePass KDBX 4 (Argon2d):
\\((S . \"salt/nonce\")
 (P . <parallelism (N threads)>)
 (M . <memory usage in kilobytes>)
 (I . <number of iterations>))"
  (let ((wrapper (format "%s/%s" (if (memq system-type '(ms-dos windows-nt))
                                     "c:/temp" "/tmp")
                         "argon-wrapper"))
        (process-environment
         (append (mapcar
                  (lambda (item)
                    (format "%s=%s" (car item) (cdr item)))
                  `((ARGON_KEY
                     . ,(apply 'concat
                               (mapcar (lambda (x)
                                         (format "%02x" x))
                                       composite-key)))
                    (ARGON_SALT
                     . ,(apply 'concat
                               (mapcar (lambda (x)
                                         (format "%02x" x))
                                       (alist-get 'S kdf-parameters))))
                    (ARGON_I
                     . ,(number-to-string
                         (alist-get 'I kdf-parameters)))
                    (ARGON_M
                     . ,(number-to-string
                         (logb (/ (alist-get 'M kdf-parameters) 1024))))
                    (ARGON_P
                     . ,(number-to-string
                         (alist-get 'P kdf-parameters)))))
                 process-environment))
        proc-status)
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert composite-key)
            (with-temp-file wrapper
              (insert elkee-script-argon2))
            (setq proc-status
                  (call-process
                   "sh" nil (get-buffer-create "*elkee-argon*") nil wrapper)))
          (when (and proc-status (> proc-status 0))
            (error "Argon2 script crashed: %S %s"
                   (with-current-buffer (get-buffer-create "*elkee-argon*")
                     (buffer-string))
                   (number-to-string
                    (floor
                     (/ (alist-get 'M kdf-parameters) 1024)))))
          (with-current-buffer (get-buffer-create "*elkee-argon*")
            (set-buffer-multibyte nil)
            (let ((hex-str (buffer-substring-no-properties
                            (point-min) (+ (point-min) 64))))
              (mapcar (lambda (idx)
                        (string-to-number
                         (substring hex-str (* idx 2) (+ (* idx 2) 2)) 16))
                      (number-sequence 0 (1- (/ (length hex-str) 2)))))))
      (kill-buffer (get-buffer-create "*elkee-argon*"))
      (delete-file wrapper))))

(defun elkee-compute-master-key (seed transformed-key)
  "Derive master key out of SEED and TRANSFORMED-KEY."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (dolist (item (append seed transformed-key)) (insert item))
    (secure-hash 'sha256 (buffer-string) nil nil t)))

(defun elkee-parse-encrypted-buffer (buff &optional delete start-pos)
  "Parse encrypted bytes from buffer BUFF.
Optional argument DELETE destroys buffer data while reading.
Optional argument START-POS marks position to start processing from."
  (let (blocks)
    (let* ((size (/ elkee-32-bit elkee-byte))
           (block-length-raw (make-vector size nil))
           break hmac-hash block-length block-data)
      (with-current-buffer buff
        (save-excursion
          (unless start-pos
            (setq start-pos (point-min)))

          (goto-char start-pos)
          (while (not break)
            (setq hmac-hash nil)
            (dotimes (_ 32)
              (push (char-after) hmac-hash)
              (forward-char 1))
            (setq hmac-hash (reverse hmac-hash))

            ;; block length
            (dotimes (idx size)
              (aset block-length-raw idx (char-after))
              (forward-char 1))

            (setq block-length (elkee-read-uint block-length-raw elkee-32-bit))
            (if (= block-length 0)
                (setq break t)
              (dotimes (_ block-length)
                (push (char-after) block-data)
                (forward-char 1))
              (push (reverse block-data) blocks)))
          (when delete
            (delete-region (point-min) (point))))))
    (setq blocks (reverse blocks))
    blocks))

(cl-defstruct elkee-database
  "Struct holding all the available info about KeePass database."
  (version nil :type 'list :documentation "Major and other parts of version.")
  (headers nil :type 'alist :documentation "Mapping for available headers.")
  (key "" :type 'string :documentation "Transformed key for the database.")
  (encrypted-blocks nil :type 'list :documentation "List of raw data blocks.")
  (decrypted-blocks nil :type 'list :documentation "List of raw data blocks.")
  (xml-headers nil :type 'alist :documentation "Protected XML stream headers.")
  (xml "" :type 'string :documentation "Decrypted serialized XML.")
  (xml-unsafe "" :type 'string :documentation "Unprotected, unsafe XML."))

(defun elkee-aes256-decrypt-block (key iv data)
  "Decrypt AES256-CBC-encrypted DATA block with KEY and IV."
  (apply 'string (kaesar--cbc-decrypt
                  (vconcat data) (kaesar--expand-to-block-key (vconcat key))
                  (vconcat iv))))

(defun elkee-aes256-decrypt-buffer (kdbx buff &optional delete start-pos)
  "Decrypt AES256-encrypted .kdbx contents of BUFF into KDBX struct.
Optional argument DELETE destroys buffer data while reading.
Optional argument START-POS marks position to start processing from."
  (with-current-buffer buff
    (save-excursion
      (let ((encrypted-blocks (elkee-parse-encrypted-buffer
                               buff delete start-pos))
            decrypted-blocks)
        (setf (elkee-database-encrypted-blocks kdbx) encrypted-blocks)
        (dolist (block encrypted-blocks)
          (push (elkee-aes256-decrypt-block
                 (elkee-database-key kdbx)
                 (alist-get 'encryption-iv (elkee-database-headers kdbx))
                 block)
                decrypted-blocks))
        (setq decrypted-blocks (reverse decrypted-blocks))
        (setf (elkee-database-decrypted-blocks kdbx) decrypted-blocks)
        decrypted-blocks))))

(defun elkee-decrypt-buffer (kdbx buff &optional delete start-pos)
  "Decrypt .kdbx contents of BUFF into KDBX struct.
Optional argument DELETE destroys buffer data while reading.
Optional argument START-POS marks position to start processing from."
  (let* ((headers (elkee-database-headers kdbx))
         (cipher (alist-get 'cipher headers))
         decrypted-bytes)
    ;; should exhaust the buffer completely
    (with-current-buffer buff
      (save-excursion
        (cond ((eq cipher 'aes256)
               (elkee-aes256-decrypt-buffer kdbx buff delete start-pos))
              (t (error "Unhandled cipher: %s" cipher)))))

    (with-temp-buffer
      (set-buffer-multibyte nil)
      (dolist (block (elkee-database-decrypted-blocks kdbx))
        (dolist (byte (string-to-list block)) (insert byte))
        (when (alist-get 'compression headers)
          (unless (zlib-decompress-region (point-min) (point-max))
            (error "Bad decompression")))

        (let (headers break new-item item-type)
          (while (not break)
            (setq new-item nil)
            (goto-char (point-min))

            (progn (setf (alist-get 'type new-item)
                         (let ((type-raw (char-after)))
                           (cond ((= type-raw #x00) 'end)
                                 ((= type-raw #x01) 'protected-stream-id)
                                 ((= type-raw #x02) 'protected-stream-key)
                                 ((= type-raw #x03) 'binary)
                                 (t (error "Unhandled type: %s" type-raw)))))
                   (delete-char 1))

            (progn
              (let ((data-length-raw (make-vector 4 nil)) data-length data)
                (dotimes (idx 4)
                  (aset data-length-raw idx (char-after))
                  (delete-char 1))
                (setq data-length
                      (elkee-read-uint data-length-raw elkee-32-bit))

                (dotimes (idx data-length)
                  (push (char-after) data)
                  (delete-char 1))
                (setq data (reverse data))

                (if (eq (alist-get 'type new-item) 'protected-stream-id)
                    (let ((stream-id
                           (elkee-read-uint (vconcat data) elkee-32-bit)))
                      (setq data (cond ((= stream-id #x00) 'none)
                                       ((= stream-id #x01) 'arc4variant)
                                       ((= stream-id #x02) 'salsa20)
                                       ((= stream-id #x03) 'chacha20)))))
                (setf (alist-get 'data new-item) data)))

            (let ((type (alist-get 'type new-item)))
              (when (eq type 'end) (setq break t))

              (setf (alist-get (alist-get 'type new-item) headers)
                    (alist-get 'data new-item))))
          (setf (elkee-database-xml-headers kdbx) headers)))
      (buffer-string))))

(defun elkee-unprotect-xml-get-creds (kdbx)
  "Calculate credentials for unprotecting XML fields from KDBX struct."
  (let* ((key-hash
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (dolist (item (alist-get
                           'protected-stream-key
                           (elkee-database-xml-headers kdbx)))
              (insert item))
            (secure-hash 'sha512 (buffer-string) nil nil t)))
         (key (string-to-list (substring key-hash 0 32)))
         (nonce (string-to-list (substring key-hash 32 44))))
    (cons key nonce)))

(defun elkee-unprotect-xml (kdbx)
  "Unprotect XML of KeePass database retrieved from KDBX struct."
  (let* ((creds (elkee-unprotect-xml-get-creds kdbx))
         (key (car creds))
         (nonce (cdr creds))
         (case-fold-search t)
         (anchor " protected=\"true\"")
         result)
    (with-temp-buffer
      (insert (elkee-database-xml kdbx))
      (goto-char (point-min))
      (catch 'done
        (while (re-search-forward anchor nil t)
          (replace-match "")
          (let ((start 0) (end 0) tmp)
            (if (re-search-forward ">" nil t)
                (setq start (1+ (match-beginning 0)))
              (throw 'done "Bad value start"))
            (if (re-search-forward "</value>" nil t)
                (setq end (match-beginning 0))
              (throw 'done "Bad value end"))
            (setq tmp (elchacha-encrypt-decrypt
                       (apply 'vector key) (apply 'vector nonce)
                       (apply 'vector (string-to-list
                                       (base64-decode-string
                                        (buffer-substring-no-properties
                                         start end))))))
            (goto-char start)
            (delete-char (- end start))
            (insert (base64-encode-string
                     (apply 'string (mapcar (lambda (x) x) tmp)))))))
      (string-replace "\11" "        " (buffer-string)))))

(defun elkee-read-buffer (password keyfile &optional unprotect)
  "Destructively read the current buffer into a KeePass database structure.
Argument PASSWORD is plaintext/string password
Argument KEYFILE is path to a keyfile.
Optional argument UNPROTECT stores unsafe, unprotected data in KDBX struct."
  (let ((signature (elkee-parse-signature-buffer (current-buffer) t))
        (kdbx (make-elkee-database))
        version headers)
    (unless (equal signature elkee-signature)
      (signal 'elkee-unsupported-file signature))
    (setq version (elkee-parse-version-buffer (current-buffer) t))
    (setf (elkee-database-version kdbx) version)

    (setq headers (elkee-parse-headers-buffer
                   (current-buffer) (car version) (cadr version) t))
    (setf (elkee-database-headers kdbx) headers)

    (when (>= (car version) 4)
      (setf (elkee-database-key kdbx)
            (elkee-compute-master-key
             (alist-get 'master-seed headers)
             (elkee-compute-transformed-key
              (elkee-compute-composite-key password keyfile)
              (alist-get 'kdf-parameters headers))))
      (delete-char (+ 32 ;; todo: header bytes checksum
                      32 ;; todo: credentials checksum
                      ))
      (setf (elkee-database-xml kdbx)
            (elkee-decrypt-buffer kdbx (current-buffer) t))
      (when unprotect
        (setf (elkee-database-xml-unsafe kdbx) (elkee-unprotect-xml kdbx))))
    kdbx))

(defun elkee--list-creds-grouped (xml)
  "Parse credentials as alist from unprotected KeePass XML document."
  (let (result)
    (dolist (group (cdr-safe (alist-get 'Root xml)))
      (let ((group-name (car-safe (cdr-safe (alist-get 'Name group)))))
        (when group-name
          (dolist (entry group)
            (when (eq 'Entry (car-safe entry))
              (let (entry-item)
                (dolist (string entry)
                  (when (eq 'String (car-safe string))
                    (let ((name (car-safe (cdr-safe (alist-get 'Key string)))))
                      (setq entry-item
                            (plist-put entry-item
                                       (intern (format ":%s" (downcase name)))
                                       (let ((value
                                              (car-safe
                                               (cdr-safe
                                                (alist-get 'Value string)))))
                                         (if (string= "Password" name)
                                             (base64-decode-string value)
                                           value)))))))
                (when entry-item
                  (push entry-item
                        (alist-get (intern group-name) result)))))))))
    result))

(defun elkee--list-creds-flat (xml)
  "Parse credentials as flat list from unprotected KeePass XML document."
  (let (result)
    (dolist (group (cdr-safe (alist-get 'Root xml)))
      (let ((group-name (car-safe (cdr-safe (alist-get 'Name group)))))
        (when group-name
          (dolist (entry group)
            (when (eq 'Entry (car-safe entry))
              (let (entry-item)
                (dolist (string entry)
                  (when (eq 'String (car-safe string))
                    (let ((name (car-safe (cdr-safe (alist-get 'Key string)))))
                      (setq entry-item
                            (plist-put entry-item
                                       (intern (format ":%s" (downcase name)))
                                       (let ((value
                                              (car-safe
                                               (cdr-safe
                                                (alist-get 'Value string)))))
                                         (if (string= "Password" name)
                                             (base64-decode-string value)
                                           value)))))))
                (when entry-item
                  (push (setq entry-item
                              (plist-put entry-item :group group-name))
                        result))))))))
    result))

(defun elkee--find-creds (xml &rest selectors)
  "Parse credentials as flat list from unprotected KeePass XML document.
Optional argument SELECTORS is a plist with keys.

On a selector, if specified alone, returns all matching entries, and if
specified in combination with other selectors, narrows the selection down.

A selector is a regexp passed into `string-match'.

SELECTORS:
* :group
* :title
* :username
* :url"
  (let ((include 0) (required 0)
        (keys '(:group :title :username :url))
        result)
    (dolist (key keys)
      (when (plist-member selectors key)
        (setq required (1+ required))))
    (dolist (item (elkee--list-creds-flat xml))
      (setq include 0)
      (dolist (key keys)
        (let* ((group (plist-get selectors key)))
          (when (and group (string-match-p group (or (plist-get item key) "")))
            (setq include (1+ include)))))
      (when (>= include required)
        (setq result (push item result))))
    result))

(defun elkee-list-creds-buffer (buffer password keyfile &rest opts)
  "Destructively read BUFFER to retrieve KeePass creds from.
Argument PASSWORD is plaintext/string password
Argument KEYFILE is path to a keyfile.
Optional argument OPTS is a plist with options:

OPTS:
* :group - when non-nil returns ALIST ((group (entry) (entry2)) ...)"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (elkee-database-xml-unsafe
             (with-current-buffer buffer
               (elkee-read-buffer password keyfile t))))
    (cond ((plist-get opts :group)
           (elkee--list-creds-grouped (libxml-parse-xml-region)))
          (t (elkee--list-creds-flat (libxml-parse-xml-region))))))

(defun elkee-read (filepath password keyfile &optional unprotect)
  "Read FILEPATH into a KeePass database structure.
Argument PASSWORD is plaintext/string password
Argument KEYFILE is path to a keyfile.
Optional argument UNPROTECT stores unsafe, unprotected data in KDBX struct."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq-local buffer-file-coding-system 'binary)
    (insert-file-contents-literally filepath nil)
    (elkee-read-buffer password keyfile unprotect)))

(defun elkee-list-creds (filepath password keyfile &rest opts)
  "Read FILEPATH to retrieve KeePass creds from.
Argument PASSWORD is plaintext/string password
Argument KEYFILE is path to a keyfile.
Optional argument OPTS is a plist with options:

OPTS:
* :group - when non-nil returns ALIST ((group (entry) (entry2)) ...)"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (elkee-database-xml-unsafe
             (elkee-read filepath password keyfile t)))
    (cond ((plist-get opts :group)
           (elkee--list-creds-grouped (libxml-parse-xml-region)))
          (t (elkee--list-creds-flat (libxml-parse-xml-region))))))

(defun elkee-find-creds-buffer (buffer password keyfile &rest selectors)
  "Destructively read BUFFER to find KeePass creds in.
Argument PASSWORD is plaintext/string password
Argument KEYFILE is path to a keyfile.
Optional argument SELECTORS is a plist with keys.

On a selector, if specified alone, returns all matching entries, and if
specified in combination with other selectors, narrows the selection down.

A selector is a regexp passed into `string-match'.

SELECTORS:
* :group
* :title
* :username
* :url"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (elkee-database-xml-unsafe
             (with-current-buffer buffer
               (elkee-read-buffer password keyfile t))))
    (apply 'elkee--find-creds `(,(libxml-parse-xml-region) ,@selectors))))

(defun elkee-find-creds (filepath password keyfile &rest selectors)
  "Read FILEPATH to find KeePass creds in.
Argument PASSWORD is plaintext/string password
Argument KEYFILE is path to a keyfile.
Optional argument SELECTORS is a plist with keys.

On a selector, if specified alone, returns all matching entries, and if
specified in combination with other selectors, narrows the selection down.

A selector is a regexp passed into `string-match'.

SELECTORS:
* :group
* :title
* :username
* :url"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (elkee-database-xml-unsafe
             (elkee-read filepath password keyfile t)))
    (apply 'elkee--find-creds `(,(libxml-parse-xml-region) ,@selectors))))

(provide 'elkee)
;;; elkee.el ends here
