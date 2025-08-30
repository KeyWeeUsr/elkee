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

(defconst elkee-byte 8
  "Bits in a byte.")

(defun elkee-read-uint16 (data &optional offset)
  "Read an unsigned 16-bit integer from DATA at OFFSET."
  (+ (lsh (aref data (or offset 0)) 0)
     (lsh (aref data (1+ (or offset 0))) elkee-byte)))

(defun elkee-read-uint32 (data &optional offset)
  "Read an unsigned 32-bit integer from DATA at OFFSET."
  (let ((offset (or offset 0)))
    (+ (lsh (aref data (+ 0 offset)) (* 0 elkee-byte))
       (lsh (aref data (+ 1 offset)) (* 1 elkee-byte))
       (lsh (aref data (+ 2 offset)) (* 2 elkee-byte))
       (lsh (aref data (+ 3 offset)) (* 3 elkee-byte)))))

(defun elkee-read-uint64 (data &optional offset)
  "Read an unsigned 64-bit integer from DATA at OFFSET."
  (let ((offset (or offset 0)))
    (+ (lsh (aref data (+ 0 offset)) (* 0 elkee-byte))
       (lsh (aref data (+ 1 offset)) (* 1 elkee-byte))
       (lsh (aref data (+ 2 offset)) (* 2 elkee-byte))
       (lsh (aref data (+ 3 offset)) (* 3 elkee-byte))
       (lsh (aref data (+ 4 offset)) (* 4 elkee-byte))
       (lsh (aref data (+ 5 offset)) (* 5 elkee-byte))
       (lsh (aref data (+ 6 offset)) (* 6 elkee-byte))
       (lsh (aref data (+ 7 offset)) (* 7 elkee-byte)))))

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

      (let* ((size (/ 16 elkee-byte))
             (minor-bytes (make-vector size nil))
             (major-bytes (make-vector size nil))
             last-pos version)
        (goto-char start-pos)
        (dotimes (idx size)
          (aset minor-bytes idx (char-after))
          (forward-char 1))
        (push (elkee-read-uint16 minor-bytes) version)
        (when delete
          (delete-region start-pos (+ start-pos size))
          (goto-char start-pos))

        (dotimes (idx size)
          (aset major-bytes idx (char-after))
          (forward-char 1))
        (push (elkee-read-uint16 major-bytes) version)
        (when delete
          (delete-region start-pos (+ start-pos size)))
        version))))

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
