;;; imgix.el --- imgix visor emacs edition

;; Copyright (C) 2014 imgix

;; Author: imgix
;; Version: 1.0
;; Keywords: images, image processing, image editing, sepia, blur

;;; package --- Summary

;;; Commentary:

;; A minor mode for editing images in emacs via imgix

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
  "Get file contents of file FILE-PATH as string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun imgix-json-decode-hash (to-decode)
  "Get json object string TO-DECODE as a hash table."
  (let ((json-object-type 'hash-table))
    (json-read-from-string to-decode)))

(defun imgix-load-json (data-path)
  "Get file of json DATA-PATH as elisp hash table."
  (imgix-json-decode-hash (imgix-get-file-contents data-path)))


;; TODO: move/back and forth undo/redo...?
;; TODO: presets...
;; TODO: for url fields remember past fields and have those as options
;; TODO: normalize queries
;; TODO: melpa for (package-install 'imgix)  !!
;; TODO: special nesting of URLs for blend/mask
;; TODO: open url in default browser
;; TODO: custom mode line of current url...
;; TODO: should these be prefixed imgix/ or imgix-- ?
;; TODO: s3 uploader / source configuration...?
;; TODO: option to to select param menu from full half buffer ( so you can read all options)

(setq eww-header-line-format "imgix visor - emacs edition") ;; override default *eww* buffer

;;;###autoload
(defvar imgix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-u") 'imgix-update-url-param)
    (define-key map (kbd "C-c C-e") 'imgix-prompt-buffer-url)
    (define-key map (kbd "C-c C-b") 'imgix-prompt-buffer-url-base)
    map))

(defconst imgix-buffer-url "http://jackangers.imgix.net/chester.png")
(defconst imgix-params-default-lookup (imgix-load-json "data/default_values.json"))
(defconst imgix-params-title-lookup (imgix-load-json "data/params.json"))
(defconst imgix-params-option-lookup (imgix-load-json "data/param_options.json"))
(defconst imgix-params-code-lookup (ht-flip imgix-params-title-lookup))
(defconst imgix-params-codes (ht-keys imgix-params-title-lookup))
(defconst imgix-params-titles (ht-values imgix-params-title-lookup))
(defconst imgix-params-accepts-url '("mark" "mask" "blend" "txt" "txtfont"))
(defvar imgix-last-updated-param "")

;(type-of (ht-get imgix-params-option-lookup "txtalign"))

(defun imgix-force-front (item list)
  "Force an ITEM to be at the front of a LIST."
  (if (not (null item))
    (mapcar 'identity (cons item (-non-nil (-remove (lambda (x) (string= x item)) list))))
    list))

(defun imgix-json-decode-plist (to-decode)
  "Get json object string TO-DECODE as a plist."
  (let ((json-object-type 'plist))
    (json-read-from-string to-decode)))

;; (defun imgix-make-combos (list)
;;   (if (null list) '(nil)
;;     (let* ((a (car list))
;;            (d (cdr list))
;;            (s (combos d))
;;            (v (mapcar (lambda (x) (cons a x)) s)))
;;     (append s v))))

;; (defun imgix-flatten-combos (list)
;;   (mapcar
;;     (lambda (x)
;;       (mapconcat 'identity x ","))
;;     (-non-nil list)))

(defun imgix-is-url-encoded (txt)
  "Is TXT url encoded?"
  (and (not (string= txt (url-unhex-string txt)))))

(defun imgix-parse-url (url)
  "Parse a URL to a hash table via (url-generic-parse-url) but breaking up path and query."
  (let* ((result (ht-create))
         (parts (url-generic-parse-url url))
         (fn-parts (split-string (url-filename parts) "?"))
         (path (car fn-parts))
         (query (if (eq (length fn-parts) 2) (cadr fn-parts) "")))

    (ht-set! result "scheme" (url-type parts))
    (ht-set! result "host" (url-host parts))
    (ht-set! result "path" path)
    (ht-set! result "query" query)
    result))

(defun imgix-build-url (parts)
  (concat (ht-get parts "scheme") "://"
          (ht-get parts "host")
          (ht-get parts "path") "?"
          (ht-get parts "query")))

(defun imgix-force-string (inp)
  (if (numberp inp)
    (number-to-string inp)
    inp))

(defun imgix-build-qs (lookup)
  "Build a URL query string from a hash table LOOKUP"
  (let* ((qs '()))
    (ht-map (lambda (k v)
              ;; if param has a value and its not its default
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
  "Parse a query string QS into a hash table key=value&key=value."
  (let* ((parsed (ht-create))
         (parts (split-string qs "&")))

    (mapc (lambda (p)
            (let ((sides (split-string p "=")))
              (when (and (car sides) (cadr sides))
                (ht-set! parsed (car sides) (cadr sides)))))
           parts)
     parsed))

(defun imgix-update-url-param ()
  "Prompt user to help update the imgix url's params."
  (interactive)
  (let* ((parts (imgix-parse-url imgix-buffer-url))
         (qs-lookup (imgix-parse-qs (ht-get parts "query")))
         (param-title-to-update (ido-completing-read "Select param:" (imgix-force-front imgix-last-updated-param imgix-params-titles)))
         (param-code-to-update (ht-get imgix-params-code-lookup param-title-to-update))

         (cur-param-value (ht-get qs-lookup param-code-to-update))
         (cur-param-options (mapcar 'identity (ht-get imgix-params-option-lookup param-code-to-update)))
         (prompt-text (concat "Value for " param-title-to-update ": "))

         (param-value
           (if cur-param-options
		     (ido-completing-read prompt-text (imgix-force-front cur-param-value cur-param-options))
             (read-from-minibuffer
               prompt-text (if (member param-code-to-update imgix-params-accepts-url)
                             (url-unhex-string cur-param-value)
                             cur-param-value)))))

    (setq imgix-last-updated-param param-title-to-update)
    (ht-set! qs-lookup param-code-to-update param-value)
    (ht-set parts "query" (imgix-build-qs qs-lookup))
    (setq imgix-buffer-url (imgix-build-url parts))
    (imgix-display-image)))

(defun imgix-prompt-buffer-url ()
  "Prompt the user for a full imgix image url."
  (interactive)
  (setq imgix-buffer-url (read-from-minibuffer "URL: " imgix-buffer-url))
  (imgix-display-image))


(defun imgix-prompt-buffer-url-base ()
  "Prompt the user for a new base url while keeping the query string."
  (interactive)
  (let* ((parts (split-string imgix-buffer-url "?"))
         (base-url (car parts))
         (qs (if (eq (length parts) 2)
               (cadr parts)
               ""))
         (new-base (read-from-minibuffer "Base URL: " base-url)))

  (setq imgix-buffer-url (if (> (length qs) 0)
                           (concat new-base "?" qs)
                           new-base))
  (imgix-display-image)))

(defadvice eww-render (after eww-render-after activate)
  "AFTER eww-render run insert the current buffer url."
  ;; TODO: ensure this only runs when in imgix-mode and NOT always...

  ;(imgix-overtake-eww)

  (if (not (get-buffer-window-list "*eww*"))
    (switch-to-buffer "*eww*")
    (previous-buffer))

  (with-current-buffer "*eww*"
    (goto-char (point-max))
    (insert (concat "\n" imgix-buffer-url))
	(rename-buffer "*imgix*"))

  (if (not (get-buffer-window-list "*imgix*"))
    (switch-to-buffer "*imgix*")))


(defun imgix-overtake-eww ()
  (when (get-buffer "*imgix*")
    (with-current-buffer "*imgix*"
	  (rename-buffer "*eww*"))))

(defun imgix-display-image ()
  "Display image in Emacs browser eww."
  ;(kill-buffer "*eww*")

  (imgix-overtake-eww)
  (eww-browse-url imgix-buffer-url))


;;;###autoload
(define-minor-mode imgix-mode
  "Minor mode for editing images via imgix"
  :global t
  :keymap imgix-mode-map)



;;;;;REFERENCE:

;; https://github.com/magit/git-modes/blob/master/git-commit-mode.el
;; https://github.com/bbatsov/emacs-lisp-style-guide

(provide 'imgix)
;;; imgix.el ends here
