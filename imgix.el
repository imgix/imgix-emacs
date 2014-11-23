(require 'json)
(require 'ht)
(require 'url)

;; TODO: tests
;; TODO: custom mode line of current url...

;; TODO: s3 uploader...?

(defvar imgix-buffer-url "http://jackangers.imgix.net/chester.png")


(defun ht-flip (to-flip)
	(let* ((result (ht-create)))
		(ht-map (lambda (k v)
				  (ht-set! result v k))
				to-flip)
		result))


;; TODO: should these be prefixed imgix/ ?

(defun imgix-get-file-contents (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun imgix-json-decode-hash (to-decode)
  (let ((json-object-type 'hash-table))
    (json-read-from-string to-decode)))

(defun imgix-json-decode-plist (to-decode)
  (let ((json-object-type 'plist))
    (json-read-from-string to-decode)))

(defun imgix-prompt-list-pick (list)
  (interactive)
  (let ((arg (ido-completing-read "Select from list: " list)))
     arg))

(defun imgix-parse-url (url)
	(let* ((result (ht-create))
		   (parts (url-generic-parse-url url))
		   (fn-parts (split-string (url-filename parts) "?"))
		   (path (car fn-parts))
		   (query (if (eq (length fn-parts) 2) (second fn-parts) "")))

	  (ht-set! result "scheme" (url-type parts))
	  (ht-set! result "host" (url-host parts))
	  (ht-set! result "path" path)
	  (ht-set! result "query" query)
	result))

(defun imgix-build-qs (lookup)
	(let* ((result '()))
		(ht-map (lambda (k v)
					(add-to-list 'result (concat k "=" v)))
				lookup)
		(mapconcat 'identity result "&")))


(defun imgix-prompt-buffer-url ()
	(interactive)
		(setq imgix-buffer-url (read-from-minibuffer "URL: " imgix-buffer-url)))

;;
;; start scratch...
;;

(let* ((qs (ht-create))
      )
	(ht-set! qs "w" "500")
	(ht-set! qs "h" "700")
	(imgix-build-qs qs)
)

(let ((blah "one"))
	(message blah)
	(let ((blah "two"))
		(message blah))
	(message blah))


(setq ddd '())

(message (prin1-to-string ddd))
(add-to-list 'ddd "2")
(add-to-list

;;; TODO: a query parser

;(json-read-from-string (imgix-get-file-contents "params.json"))
(let* ((params-pretty-lookup (imgix-json-decode-hash (imgix-get-file-contents "params.json")))
	   (params-code-lookup (ht-flip params-pretty-lookup))
	   (param-codes (ht-keys params-pretty-lookup))
	   (param-titles (ht-values params-pretty-lookup)))
	;params
	(imgix-prompt-list-pick param-titles)
)



(split-string "/file?x=1" "?")
(second (split-string "/file?x=1" "?"))

(url-type (url-generic-parse-url "http://jackangers.imgix.net/chester.png"))
(ht-get (imgix-parse-url "http://jackangers.imgix.net/chester.png") "path")


(setq zzz '(chester cat))


(when (eq (length zzz) 2) (message "yup"))

(plist-get zzz 'chester)

(setq blah (ht-create))

(ht-set! blah "b" "blend")

(ht-get blah "b")

(url-



(ht-get (ht-flip blah) "blend")
)
(ht-map (lambda (x y) (message x)) blah)
