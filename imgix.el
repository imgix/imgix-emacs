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
;; TODO: docs...
;; TODO: for url fields remember past fields and have those as options
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
;;(setq imgix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-u") 'imgix-update-url-param)
    (define-key map (kbd "C-c C-e") 'imgix-prompt-buffer-url)
    (define-key map (kbd "C-c C-b") 'imgix-prompt-buffer-url-base)
    (define-key map (kbd "C-c C-o") 'imgix-open-in-browser)
    map))

(defconst imgix-buffer-url "http://jackangers.imgix.net/chester.png")
(defconst imgix-params-depends-lookup (imgix-load-json "data/param_depends.json"))
(defconst imgix-params-default-lookup (imgix-load-json "data/default_values.json"))
(defconst imgix-params-title-lookup (imgix-load-json "data/params.json"))
(defconst imgix-params-option-lookup (imgix-load-json "data/param_options.json"))
(defconst imgix-params-code-lookup (ht-flip imgix-params-title-lookup))
(defconst imgix-params-codes (ht-keys imgix-params-title-lookup))
(defconst imgix-params-titles (ht-values imgix-params-title-lookup))
(defconst imgix-params-accepts-url '("mark" "mask" "blend" "txt" "txtfont"))
(defvar imgix-last-updated-param nil)

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
	(mapc (lambda (k)
            (let ((v (ht-get lookup k)))

              ;; if param has a value and its not its default
              (when (and (stringp k) (stringp v)
                         (> (length k) 0) (> (length v) 0)
                         (not (string= (ht-get imgix-params-default-lookup k) v)))

				(if (member k imgix-params-accepts-url)
                  (add-to-list 'qs (concat k "=" (if (imgix-is-url-encoded v)
                                                   v
                                                   (url-hexify-string v))))
                  (add-to-list 'qs (concat k "=" v))))))

		  (reverse (sort (ht-keys lookup) 'string<)))

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

(defun imgix-ensure-param-depends-defined (param qs-lookup)
  "Ensures PARAM has its dependencies defined in QS-LOOKUP by prompting user."
  (let* ((param-depends (ht-get imgix-params-depends-lookup param)))
    (when param-depends
      (mapc
        (lambda (k)
          (unless (ht-get qs-lookup k)
            (imgix--prompt-param-value k qs-lookup nil)))
        param-depends))))

(defun imgix--prompt-param-value (param qs-lookup param-depends-check)
  "Prompts user for value of PARAM to set in QS-LOOKUP hash table."
  (interactive)
  (let* ((cur-param-value (ht-get qs-lookup param))
         (cur-param-options (mapcar 'identity (ht-get imgix-params-option-lookup param)))
         (param-title (ht-get imgix-params-title-lookup param))
         (prompt-text (concat "Value for " param-title ": "))

         (param-value
           (if cur-param-options
		     (ido-completing-read prompt-text (imgix-force-front cur-param-value cur-param-options))
             (read-from-minibuffer
               prompt-text (if (member param imgix-params-accepts-url)
                             (url-unhex-string cur-param-value)
                             cur-param-value)))))

    (ht-set! qs-lookup param param-value)

	;; prompt for all values of all undefined dependency params..
	(when param-depends-check
      (imgix-ensure-param-depends-defined param qs-lookup))

    qs-lookup))

(defun imgix-update-url-param ()
  "Prompt user to help update the imgix url's params."
  (interactive)
  (let* ((parts (imgix-parse-url imgix-buffer-url))
         (qs-lookup (imgix-parse-qs (ht-get parts "query")))
         (param-title (ido-completing-read "Select param:" (imgix-force-front imgix-last-updated-param imgix-params-titles)))
         (param (ht-get imgix-params-code-lookup param-title)))

	(ht-set parts "query" (imgix-build-qs (imgix--prompt-param-value param qs-lookup t)))
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

(defun imgix-get-base-url ()
  (car (split-string imgix-buffer-url "?")))

(defun imgix-open-in-browser ()
  (interactive)
  (browse-url imgix-buffer-url))

(defun imgix-display-image ()
  "Display image in Emacs browser eww."
  ;(kill-buffer "*eww*")

  (imgix-overtake-eww)
  (eww-browse-url imgix-buffer-url))

;; (defun imgix-get-url-history ()
;;   (let ((hist-path (expand-file-name (concat user-emacs-directory "imgix_visor_history"))))
;;     (if (file-exists-p hist-path)
;;       (mapcar 'identity (imgix-json-decode-hash (imgix-get-file-contents hist-path)))
;; 	  '("http://jackangers.imgix.net/chester.png"))))

;; (defun imgix-add-url-history (url)
  ;; (let ((hist-path (expand-file-name (concat user-emacs-directory "imgix_visor_history"))))
  ;;   (if (file-exists-p hist-path)
  ;;     (mapcar 'identity (imgix-json-decode-hash (imgix-get-file-contents hist-path)))
  ;; 	  '("http://jackangers.imgix.net/chester.png"))))

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
