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
