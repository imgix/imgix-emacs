(require 'json)
(require 'ht)
(require 'url)

(defun ht-flip (to-flip)
	(let* ((result (ht-create)))
		(ht-map (lambda (k v)
				  (ht-set! result v k))
				to-flip)
		result))

;; TODO: tests
;; TODO: custom mode line of current url...
;; TODO: should these be prefixed imgix/ or imgix-- ?
;; TODO: s3 uploader...?

(defvar imgix-buffer-url "http://jackangers.imgix.net/chester.png")
(defvar imgix-params-title-lookup (imgix-json-decode-hash (imgix-get-file-contents "params.json")))
(defvar imgix-params-code-lookup (ht-flip imgix-params-title-lookup))
(defvar imgix-params-codes (ht-keys imgix-params-title-lookup))
(defvar imgix-params-titles (ht-values imgix-params-title-lookup))
;(ht-get imgix-params-title-lookup "w")

;; (let* ((params-pretty-lookup (imgix-json-decode-hash (imgix-get-file-contents "params.json")))
;; 	   (params-code-lookup (ht-flip params-pretty-lookup))
;; 	   (param-codes (ht-keys params-pretty-lookup))
;; 	   (param-titles (ht-values params-pretty-lookup)))
;; 	;params
;; 	(imgix-prompt-list-pick param-titles)
;; )

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
	"Parse a url to a hash table via (url-generic-parse-url) but breaking up path and query"
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

(defun imgix-build-url (parts)
	(concat (ht-get parts "scheme") "://" (ht-get parts "host") (ht-get parts "path") "?" (ht-get parts "query")))

(defun imgix-build-qs (lookup)
	"Build a URL query string from a hash table key=value"
	(let* ((result '()))
		(ht-map (lambda (k v)
					(add-to-list 'result (concat k "=" v)))
				lookup)
		(mapconcat 'identity result "&")))

(defun imgix-parse-qs (qs)
	"Parse a query string into a hash table key=value&key=value"
	(let* ((result (ht-create))
		   (parts (split-string qs "&")))

	(mapc (lambda (p)
		    (let ((sides (split-string p "=")))
				(ht-set! result (first sides) (second sides))))
		  parts)
	result))

(defun imgix-update-url-param ()
	(interactive)
	(let* ((parts (imgix-parse-url imgix-buffer-url))
		   (qs-lookup (imgix-parse-qs (ht-get parts "query")))
		   (param-title-to-update (imgix-prompt-list-pick imgix-params-titles))
		   (param-code-to-update (ht-get imgix-params-code-lookup param-title-to-update))
		   (cur-param-value (ht-get qs-lookup param-code-to-update))
		   (param-value (read-from-minibuffer (concat "Value for " param-title-to-update ": ") cur-param-value)))

	(ht-set! qs-lookup param-code-to-update param-value)

	(ht-set parts "query" (imgix-build-qs qs-lookup))

	(setq imgix-buffer-url (imgix-build-url parts))
	(imgix-display-image)
))

(defun imgix-prompt-buffer-url ()
	(interactive)
		(setq imgix-buffer-url (read-from-minibuffer "URL: " imgix-buffer-url)))

(defun imgix-display-image ()
  (eww-browse-url imgix-buffer-url nil)
  (switch-to-buffer "*eww*")
  (delete-other-windows))


(ht-get (imgix-parse-qs "h=500&w=700") "h")
(ht-get (imgix-parse-qs "h=500&w=700") "w")
;; TODO: this will do the imgix-prompt-list-pick and then rebuilt the url and (imgix-display-image url)


;;
;; start scratch...
;;

;;;;;REFERENCE:

;; https://github.com/magit/git-modes/blob/master/git-commit-mode.el
;; https://github.com/bbatsov/emacs-lisp-style-guide

;; building qs
(mapcar (lambda (x) (concat x "--")) (split-string "w=500&h=700" "&"))

(let* ((qs (ht-create))
      )
	(ht-set! qs "w" "500")
	(ht-set! qs "h" "700")
	(imgix-build-qs qs)
)

;; opening image

;; (eww-browse-url "http://jackangers.imgix.net/chester.png?px=25" t)
;; (delete-other-windows)
;; (switch-to-buffer "*eww*")

;; (let ((blah "one"))
;; 	(message blah)
;; 	(let ((blah "two"))
;; 		(message blah))
;; 	(message blah))


;; (setq ddd '())

;; (message (prin1-to-string ddd))
;; (add-to-list 'ddd "2")
;; (add-to-list

;; ;;; TODO: a query parser

;; ;(json-read-from-string (imgix-get-file-contents "params.json"))
;; (let* ((params-pretty-lookup (imgix-json-decode-hash (imgix-get-file-contents "params.json")))
;; 	   (params-code-lookup (ht-flip params-pretty-lookup))
;; 	   (param-codes (ht-keys params-pretty-lookup))
;; 	   (param-titles (ht-values params-pretty-lookup)))
;; 	;params
;; 	(imgix-prompt-list-pick param-titles)
;; )



;; (split-string "/file?x=1" "?")
;; (second (split-string "/file?x=1" "?"))

;; (url-type (url-generic-parse-url "http://jackangers.imgix.net/chester.png"))
;; (ht-get (imgix-parse-url "http://jackangers.imgix.net/chester.png") "path")


;; (setq zzz '(chester cat))


;; (when (eq (length zzz) 2) (message "yup"))

;; (plist-get zzz 'chester)

;; (setq blah (ht-create))

;; (ht-set! blah "b" "blend")

;; (ht-get blah "b")

;; (url-



;; (ht-get (ht-flip blah) "blend")
;; )
;; (ht-map (lambda (x y) (message x)) blah)
