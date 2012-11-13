;;; assembla-lib.el --- A library that provides an easier interface to Assemblas API.

;; Copyright (C) 2012 Dan LaManna

;; Author: Dan LaManna <dan.lamanna@gmail.com>
;; Keywords: assembla api

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
(require 'furl)

(defconst assembla-api-url "https://api.assembla.com/v1")

(defgroup assembla-lib nil
  "Using Assembla API from Emacs."
  :prefix "assembla-lib-"
  :group 'tools)

(defcustom assembla-api-key nil
  "Assembla API Key"
  :group 'assembla-lib
  :type  'string)

(defcustom assembla-api-key-secret nil
  "Assembla API Key Secret"
  :group 'assembla-lib
  :type  'string)

(defvar assembla-buffer-name "-- assembla --")

(defun assembla-format-api-url(uri type)
  (format "%s/%s.%s" assembla-api-url uri type))

(defun assembla-credentials-set()
  "Checks to see if custom variables required for API calls are set."
  (and (and (boundp 'assembla-api-key)
	    (not (eq assembla-api-key nil)))
       (and (boundp 'assembla-api-key-secret)
	    (not (eq assembla-api-key-secret nil)))))

(defun assembla-get(uri type callback)
  "Retrieves Assembla URI asynchronously and calls `callback' when finished.
   `uri' and `type' get passed to `assembla-format-api-url' to form the retrieve
   URL."
  (if (not (assembla-credentials-set))
      (message "Use M-x customize to set assembla-lib settings.")
    (let ((url (assembla-format-api-url uri type)))
      (furl-with-header "X-Api-Key" assembla-api-key
	(furl-with-header "X-Api-Secret" assembla-api-key-secret
	  (furl-retrieve url callback))))))

(defun assembla-post-or-put(uri type data-str request-method callback)
  "POST/PUT (determined by `request-method') the data in `data-str' to Assembla URI asynchronously and calls `callback' when finished.
   `data-str' must be formatted in `type'"
  (let ((url (assembla-format-api-url uri type))
	(url-request-data data-str)
	(url-request-method request-method)
	(url-request-extra-headers
	 `(("Content-Type" . ,(format "application/%s" type))
	   ("X-Api-Key"    . ,assembla-api-key)
	   ("X-Api-Secret" . ,assembla-api-key-secret))))
    (url-retrieve url callback)))

(defun assembla-delete(uri type callback)
  "Sends asynchronous DELETE request to Assembla URI."
  (let ((url (assembla-format-api-url uri type))
	(url-request-method "DELETE")
	(url-request-extra-headers
	 `(("X-Api-Key"    . ,assembla-api-key)
	   ("X-Api-Secret" . ,assembla-api-key-secret))))
    (url-retrieve url callback)))

(provide 'assembla-lib)
