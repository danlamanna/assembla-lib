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

(defcustom assembla-cache-enabled t
  "Setting to nil will disable caching of all responses."
  :group 'assembla-lib
  :type  'boolean)

(defcustom assembla-cache-dir "~/.emacs.d/tmp/assembla/cache"
  "Path to store cached Assembla API responses."
  :group 'assembla-lib
  :type  'string)

(defcustom assembla-cache-duration-default 3600 ; 1 hour
  "Duration in seconds a response will be cached by default."
  :group 'assembla-lib
  :type  'integer)

;; utils
(defun assembla-format-api-url(uri type)
  (format "%s/%s.%s" assembla-api-url uri type))

(defun assembla-credentials-set()
  "Checks to see if custom variables required for API calls are set."
  (and (and (boundp 'assembla-api-key)
	    (not (eq assembla-api-key nil)))
       (and (boundp 'assembla-api-key-secret)
	    (not (eq assembla-api-key-secret nil)))))

;; cache utils
(defun assembla-invalidate-caches())

(defun assembla-cache-response(url response &optional duration)
  "Caches `response' in a file named after an MD5 hash of `url', a unix timestamp, and the duration to store it.
   Stores file in `assembla-cache-dir' which is created if it doesn't exist.

   Nothing will be cached if `assembla-cache-enabled' is `nil', or `duration' is < 1."
  (unless (or (not assembla-cache-enabled)
	      (not duration)
	      (<   duration 1))
    (let* ((url-hash       (md5 url))
	   (unix-timestamp (format-time-string "%s"))
	   (cache-duration (or duration assembla-cache-duration-default))
	   (cache-file     (format "%s/%s.%s.%d.cache" assembla-cache-dir url-hash unix-timestamp cache-duration)))
      (if (not (file-directory-p assembla-cache-dir))
	  (make-directory assembla-cache-dir t))
      (with-temp-file cache-file
	(insert (format "%s" response))))))

(defun assembla-has-cache(url)  )

;; request utils
(defun assembla-get(uri type callback &optional use-cache &optional cache-duration)
  "Retrieves Assembla URI asynchronously and calls `callback' when finished.
   `uri' and `type' get passed to `assembla-format-api-url' to form the retrieve
   URL.

   If `use-cache' is t, `assembla-cache-enabled' is t, and `assembla-has-cache' returns
   a response, no HTTP request will be sent, and `callback' will be applied to the cached response.

   `use-cache' and `cache-duration' are never both utilized, if it returns a cached response via `use-cache',
    it won't cache. On the other hand if it can't or doesn't `use-cache', and `assembla-cache-enabled' is t,
    and `cache-duration' or `assembla-cache-duration-default' are > 1, it will cache the response."
  (lexical-let* ((url            (assembla-format-api-url uri type))
		 (callback       callback)
		 (cached         (and assembla-cache-enabled use-cache (assembla-has-cache url)))
		 (do-cache       (and assembla-cache-enabled (or cache-duration assembla-cache-duration-default)))
		 (cache-duration (or cache-duration assembla-cache-duration-default)))
    (if cached
	(funcall callback (assembla-get-cache (assembla-format-api-url uri type)))
      (if (not (assembla-credentials-set))
	  (message "Use M-x customize to set assembla-lib settings.")
	(furl-with-header "X-Api-Key" assembla-api-key
	  (furl-with-header "X-Api-Secret" assembla-api-key-secret
	    (furl-retrieve url (lambda(response)
				 (if do-cache
				     (assembla-cache-response url response cache-duration))
				 (funcall callback response)))))))))

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
