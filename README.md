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
    (assembla-get "spaces" "json" (lambda(response)
                                      (with-current-buffer (get-buffer-create "assembla-test-buffer")
                                          (erase-buffer)
                                          (insert (format "%s" response)))))

    ; Create a basic ticket in space `space-id'
    (assembla-post-or-put (format "spaces/%s/tickets" space-id) "json" "{\"ticket\":{\"summary\":\"Assembla from Emacs?!\"}}" "POST" (lambda)) 

[1]: http://api-doc.assembla.com/
[2]: http://code.google.com/p/furl-el/source/browse/furl.el