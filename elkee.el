;;; elkee.el --- Keepass client -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, keepass, client
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
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

(defun elkee-read-uint (data bitness &optional offset)
  "Read an unsigned integer of BITNESS from DATA at OFFSET."
  (let ((offset (or offset 0))
        (result 0))
    (dotimes (idx (/ bitness elkee-byte))
      (setq result
            (+ result (lsh (aref data (+ idx offset)) (* idx elkee-byte)))))
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

(provide 'elkee)
;;; elkee.el ends here
