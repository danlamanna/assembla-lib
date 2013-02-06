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

;;; Commentary:
;; A very much so work in progress.

;;; Code:

(defconst asl/api-url "https://api.assembla.com/v1")

(defgroup assembla-lib nil
  "Using Assembla API from Emacs."
  :prefix "assembla-lib-"
  :group 'tools)

(defcustom asl/api-key nil
  "Assembla API Key."
  :group 'assembla-lib
  :type  'string)

(defcustom asl/api-key-secret nil
  "Assembla API Key Secret."
  :group 'assembla-lib
  :type  'string)

(defcustom asl/cache-enabled nil
  "Setting to nil will disable caching of all responses."
  :group 'assembla-lib
  :type  'boolean)

(defcustom asl/cache-dir "~/.emacs.d/tmp/assembla/cache"
  "Path to store cached Assembla API responses."
  :group 'assembla-lib
  :type  'string)

(defcustom asl/cache-duration-default 3600 ; 1 hour
  "Duration in seconds a response will be cached by default."
  :group 'assembla-lib
  :type  'integer)

(defcustom asl/developer-mode nil
  "Set to t, this will never invalidate cache, offline development ftw."
  :group 'assembla-lib
  :type  'boolean)

(defcustom asl/all-space-ids nil
  "A function to return a list of all space ids, for helpers."
  :group 'assembla-lib
  :type 'function)

(defcustom asl/all-ticket-ids nil
  "A function to return a list of all ticket ids, car being the ticket
   id, and cdr being the space-id."
  :group 'assembla-lib
  :type 'function)

;; utils
(defun asl/format-api-url(uri type)
  (format "%s/%s.%s" asl/api-url uri type))

(defun asl/check-credentials()
  "Checks to see if custom variables required for API calls are set."
  (unless (and (and (boundp 'asl/api-key)
                    (not (eq asl/api-key nil)))
               (and (boundp 'asl/api-key-secret)
                    (not (eq asl/api-key-secret nil))))
    (error "Assembla-lib credentials not set")))

;; cache utils
(defun asl/invalidate-caches()
  "Gets all cache files stored in `asl/cache-dir', cycles through, and if a cache
   file is expired (its storage timestamp + duration to cache is less than the present time)
   it gets deleted.

   Note `asl/developer-mode' will cause the body of this function to never run."
  (unless asl/developer-mode
    ;; regex for cache files should be much more thorough, to ensure other areas don't fail "[a-z0-9]{32}\.[0-9]{10,12}\.[0-9]+\.cache" ?
    (let ((cache-files (directory-files asl/cache-dir nil "cache$")))
      (dolist (cache-file cache-files)
        (let* ((file-meta (split-string cache-file "\\."))
               (timestamp (string-to-number (car (cdr file-meta))))
               (duration  (string-to-number (car (cdr (cdr file-meta)))))
               (current   (string-to-number (format-time-string "%s"))))
          (when (< (+ timestamp duration) current)
            (delete-file (format "%s/%s" asl/cache-dir cache-file))))))))

(defun asl/invalidate-uri-cache(uri type)
  "Forcibly invalidates cache related to a specific URI and TYPE
   ignoring when the cache is supposed to expire.

   Note: `asl/developer-mode' will cause the body of this function
   to never run, similar to `asl/invalidate-caches'."
  (unless asl/developer-mode
    (let* ((url      (asl/format-api-url uri type))
           (url-hash (md5 url))
           (uri-cache-files (directory-files asl/cache-dir nil (format "^%s" url-hash))))
      (message url-hash)
      (dolist (cache-file uri-cache-files)
        (delete-file (format "%s/%s" asl/cache-dir cache-file))))))

(defun asl/has-cache(url)
  "Calls `asl/invalidate-caches', then checks if any files exist
   in `asl/cache-dir' starting with the MD5 hash of URL.
   Returns list of matching cache files, or nil."
  (asl/invalidate-caches)
  (directory-files asl/cache-dir nil (format "^%s" (md5 url))))

(defun asl/get-cache(url &optional safe)
  "Returns the contents of a cache file it assumes exists, unless SAFE is set to
   a non-nil value, in which case it will call `asl/has-cache' invalidating
   old caches, and then recurse with SAFE set to nil."
  (if (not (eq safe nil))
      (if (asl/has-cache url) ; if it has a cache file, jump to non safe
          (asl/get-cache url nil))
                                        ; non safe, just get the cache file
    (let ((cache-file (car (last (directory-files asl/cache-dir t (format "^%s" (md5 url)))))))
      (with-temp-buffer
        (insert-file-contents cache-file)
        (buffer-string)))))

(defun asl/cache-response(url response &optional duration)
  "Caches RESPONSE in a file named after an MD5 hash of URL, a unix timestamp,
   and the duration to store it. Stores file in `asl/cache-dir' which is
   created if it doesn't exist.

   Nothing will be cached if `asl/cache-enabled' is `nil', or DURATION is < 1."
  (unless (or (not asl/cache-enabled)
              (<   duration 1))
    (let* ((url-hash       (md5 url))
           (unix-timestamp (format-time-string "%s"))
           (cache-duration (or duration asl/cache-duration-default))
           (cache-file     (format "%s/%s.%s.%d.cache" asl/cache-dir url-hash unix-timestamp cache-duration)))
      (if (not (file-directory-p asl/cache-dir))
          (make-directory asl/cache-dir t))
      (with-temp-file cache-file
        (setq buffer-file-coding-system 'raw-text)
        (insert (format "%s" response))))))

;; request utils
(defun asl/get(uri type callback &optional use-cache &optional cache-duration)
  "Retrieves Assembla URI synchronously and calls CALLBACK when finished.
   URI and TYPE get passed to `asl/format-api-url' to form the retrieve
   URL.

   If USE-CACHE is t, `asl/cache-enabled' is t, and `asl/has-cache'
   returns a response, no HTTP request will be sent, and CALLBACK will be
   applied to the cached response.

   USE-CACHE and CACHE-DURATION are never both utilized, if it returns a
   cached response via USE-CACHE it won't cache anything new. On the other
   hand if it can't or doesn't utilize USE-CACHE, and `asl/cache-enabled'
   is t, and CACHE-DURATION or `asl/cache-duration-default' are > 1, it
   will cache the response."
  (asl/check-credentials)
  (lexical-let* ((url            (asl/format-api-url uri type))
                 (callback       callback)
                 (cached         (and asl/cache-enabled use-cache (asl/has-cache url)))
                 (do-cache       (and asl/cache-enabled (or cache-duration asl/cache-duration-default)))
                 (cache-duration (or cache-duration asl/cache-duration-default)))
    (if cached
        (funcall callback (asl/get-cache url))
      (with-current-buffer (url-retrieve-synchronously url)
        (let ((response (buffer-substring-no-properties url-http-end-of-headers (point-max))))
          (funcall callback response)
          (if do-cache
              (asl/cache-response url response cache-duration)))))))

(defun asl/post-or-put(uri type data-str request-method callback)
  "POST/PUT (determined by REQUEST-METHOD) the data in DATA-STR to Assembla URI
   asynchronously and calls CALLBACK when finished. DATA-STR must be formatted in TYPE."
  (let ((url (asl/format-api-url uri type))
        (url-request-data data-str)
        (url-request-method request-method)
        (url-request-extra-headers
         `(("Content-Type" . ,(format "application/%s" type))
           ("X-Api-Key"    . ,asl/api-key)
           ("X-Api-Secret" . ,asl/api-key-secret))))
    (url-retrieve url callback)))

(defun asl/delete(uri type callback)
  "Sends asynchronous DELETE request to Assembla URI."
  (let ((url (asl/format-api-url uri type))
        (url-request-method "DELETE")
        (url-request-extra-headers
         `(("X-Api-Key"    . ,asl/api-key)
           ("X-Api-Secret" . ,asl/api-key-secret))))
    (url-retrieve url callback)))

;; helpers
(defmacro for-all-spaces(&rest body)
  (when fboundp 'asl/all-space-ids
        (declare (indent 2)
                 (debug t))
        `(dolist (space-id (asl/all-space-ids))
           (progn ,@body))))

(defmacro for-all-tickets(&rest body)
  (when fboundp 'asl/all-ticket-ids
        (declare (indent 2)
                 (debug t))
        `(dolist (ticket (asl/all-ticket-ids))
           (let ((ticket-id (car ticket))
                 (space-id  (cdr ticket)))
             (progn ,@body)))))

(provide 'assembla-lib)

;;; assembla-lib.el ends here
