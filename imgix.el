;;; package --- Summary

;;; Commentary:


;;; Code:
(require 'json)
(require 'url)
(require 'ht)
(require 'dash)


(defun ht-flip (to-flip)
  "Flip keys and values for hash table TO-FLIP."
  (let* ((result (ht-create)))
    (ht-map
      (lambda (k v)
        (ht-set! result v k))
      to-flip)
    result))


(defun imgix-get-file-contents (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun imgix-json-decode-hash (to-decode)
  (let ((json-object-type 'hash-table))
    (json-read-from-string to-decode)))

;; TODO: option to to select param menu from full half buffer ( so you can read all options)
;; TODO: proper major mode
;; TODO: override *eww* with *imgix-visor* (get-buffer-create "*eww*")
;; TODO: normalize queries
;; TODO: put json files in data/ dir (or whatever the best practice is)
;; TODO: melpa for (package-install 'imgix)  !!
;; TODO: option selection for params that have options like fit/crop/etc
;; TODO: encoding!!!!
;; TODO: tests
;; TODO: special nesting of URLs for blend/mask
;; TODO: open url in default browser
;; TODO: custom mode line of current url...

;; TODO: should these be prefixed imgix/ or imgix-- ?
;; TODO: s3 uploader / source configuration...?

(setq eww-header-line-format "imgix visor - emacs edition") ;; override default *eww* buffer

(defvar imgix-buffer-url "http://jackangers.imgix.net/chester.png?w=250")
(defvar imgix-params-default-lookup (imgix-json-decode-hash (imgix-get-file-contents "default_values.json")))
(defvar imgix-params-title-lookup (imgix-json-decode-hash (imgix-get-file-contents "params.json")))
(defvar imgix-params-option-lookup (imgix-json-decode-hash (imgix-get-file-contents "param_options.json")))
;(defvar imgix-params-codes-with-options (ht-keys imgix-params-options-lookup))
(defvar imgix-params-code-lookup (ht-flip imgix-params-title-lookup))
(defvar imgix-params-codes (ht-keys imgix-params-title-lookup))
(defvar imgix-params-titles (ht-values imgix-params-title-lookup))
(defvar imgix-params-accepts-url '("mark" "mask" "blend"))
;(defvar imgix-params-accepts-multiple '("auto", "markalign", "ba")) ;; others?

(setq imgix-buffer-url "http://jackangers.imgix.net/chester.png")
(setq imgix-params-title-lookup (imgix-json-decode-hash (imgix-get-file-contents "params.json")))
(setq imgix-params-default-lookup (imgix-json-decode-hash (imgix-get-file-contents "default_values.json")))
(setq imgix-params-code-lookup (ht-flip imgix-params-title-lookup))
(setq imgix-params-codes (ht-keys imgix-params-title-lookup))
(setq imgix-params-titles (ht-values imgix-params-title-lookup))
;(ht-get imgix-params-title-lookup "w")
;(ht-get imgix-params-default-lookup "w")

;(type-of (ht-get imgix-params-option-lookup "txtalign"))


(defun imgix-json-decode-plist (to-decode)
  (let ((json-object-type 'plist))
    (json-read-from-string to-decode)))

(defun imgix-make-combos (list)
  (if (null list) '(nil)
    (let* ((a (car list))
           (d (cdr list))
           (s (combos d))
           (v (mapcar (lambda (x) (cons a x)) s)))
    (append s v))))

(defun imgix-flatten-combos (list)
  (mapcar
    (lambda (x)
      (mapconcat 'identity x ","))
    (-non-nil list)))

(defun imgix-is-url-encoded (txt)
  "is TXT url encoded"
  (and (not (string= txt (url-unhex-string txt)))))

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

(defun imgix-force-string (inp)
  (if (numberp inp)
    (number-to-string inp)
    inp))

(defun imgix-build-qs (lookup)
  "Build a URL query string from a hash table LOOKUP"
  (let* ((qs '()))
    (ht-map (lambda (k v)
              (when (and (stringp k) (stringp v)
                         (> (length k) 0) (> (length v) 0)
                         (not (string= (ht-get imgix-params-default-lookup k) v)))
				(if (member k imgix-params-accepts-url)
                  (add-to-list 'qs (concat k "=" (if (imgix-is-url-encoded v)
                                                   v
                                                   (url-hexify-string v))))
                  (add-to-list 'qs (concat k "=" v)))))
             lookup)

    (if (> (length qs) 0)
      (mapconcat 'identity qs "&")
      "")))

(defun imgix-parse-qs (qs)
  "Parse a query string into a hash table key=value&key=value"
  (let* ((parsed (ht-create))
         (parts (split-string qs "&")))

    (mapc (lambda (p)
            (let ((sides (split-string p "=")))
              (when (and (first sides) (second sides))
                (ht-set! parsed (first sides) (second sides)))))
           parts)
     parsed))

(defun imgix-update-url-param ()
  (interactive)
  (let* ((parts (imgix-parse-url imgix-buffer-url))
         (qs-lookup (imgix-parse-qs (ht-get parts "query")))
         (param-title-to-update (ido-completing-read "Select param:" imgix-params-titles))
         (param-code-to-update (ht-get imgix-params-code-lookup param-title-to-update))

         (cur-param-value (ht-get qs-lookup param-code-to-update))
         (cur-param-options (mapcar 'identity (ht-get imgix-params-option-lookup param-code-to-update)))
         (prompt-text (concat "Value for " param-title-to-update ": "))

         (param-value
           (if cur-param-options
             (progn
                ;; ensure it's at the start of the list
                (delete cur-param-value cur-param-options)
                (add-to-list 'cur-param-options cur-param-value)
                (ido-completing-read prompt-text (-non-nil cur-param-options)))
             (read-from-minibuffer
               prompt-text (if (member param-code-to-update                                                            imgix-params-accepts-url)
                             (url-unhex-string cur-param-value)
                             cur-param-value)))))

    (ht-set! qs-lookup param-code-to-update param-value)
    (ht-set parts "query" (imgix-build-qs qs-lookup))
    (setq imgix-buffer-url (imgix-build-url parts))
    (imgix-display-image)))

(defun imgix-prompt-buffer-url ()
  "Prompt the user for a full imgix image url"
  (interactive)
  (setq imgix-buffer-url (read-from-minibuffer "URL: " imgix-buffer-url))
  (imgix-display-image))

(defun imgix-display-image ()
  "Display image in emacs browser eww"
  ;;(message (concat "Displaying " imgix-buffer-url))
  ; (kill-buffer "*eww*")
  (eww-browse-url imgix-buffer-url t)
  (switch-to-buffer "*eww*")
  ;; TODO: is there an eww-onload hook???
  (run-at-time "2 sec" nil (lambda ()
  (with-current-buffer "*eww*"
    (end-of-buffer)
    (insert (concat "\n" imgix-buffer-url))))))


;; start scratch...
;;

;;;;;REFERENCE:

;; https://github.com/magit/git-modes/blob/master/git-commit-mode.el
;; https://github.com/bbatsov/emacs-lisp-style-guide

;(provide 'imgix)
;;; imgix.el ends here
