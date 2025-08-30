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
