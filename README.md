assembla-lib
============

A library that provides an easier interface to Assemblas API.

This library is based off Assemblas new API (v1) referenced [here][1]. Right now it returns **raw** responses, in XML or JSON per Assemblas response.

Installation
----
1) Add [furl.el][2] to your load path.
2) Add assembla-lib.el to your load path.

     (require 'assembla-lib)

Use `M-x customize` to set API Key and API Key Secret

Bugs/Enhancements
----
- Adding oauth2 support?
- Removing dependency for furl.el
- Better (or any) error handling for 404 and 522 status code responses
- Better responses to PUT/POST/DELETE requests.

Usage
----
    ; Get all spaces in JSON
    (asl/get "spaces" "json" (lambda(response)
				      (with-current-buffer (get-buffer-create "assembla-test-buffer")
					  (erase-buffer)
					  (insert (format "%s" response)))))

    ; Create a basic ticket in space `space-id'
    (asl/post-or-put (format "spaces/%s/tickets" space-id) "json" "{\"ticket\":{\"summary\":\"Assembla from Emacs?!\"}}" "POST" (lambda))

Caching Requests
----
assembla-lib will let you cache requests given you've customized `asl/cache-enabled` to `t`.

Using the other custom variables, you can alter where the cache files are stored, and the default duration for cached files.

**Example**

    ;; This will get the latest list of spaces in JSON, and cache it for 1 day
    (asl/get "spaces" "json" 'some-callback nil 86400)

    ;; This will use the latest list of spaces from cache if they exist, otherwise it
    ;; will cache them for an hour.
    (asl/get "spaces" "json" 'some-other-callback t 3600)

[1]: http://api-doc.assembla.com/
[2]: http://code.google.com/p/furl-el/source/browse/furl.el