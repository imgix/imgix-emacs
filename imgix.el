;;; imgix.el --- imgix image url editor

;; Copyright (C) 2014 imgix

;; Author: imgix
;; Version: 1.0
;; Keywords: images, image processing, image editing, sepia, blur

;;; package --- Summary

;;; Commentary:

;; A major mode for editing images in emacs via imgix

;;; Code:
(require 'json)
(require 'url)
(require 'ht)
(require 'dash)
(require 's)
(require 'shr)
(require 'browse-url)

(defgroup imgix nil
  "Use imgix to edit files in emacs via imgix."
  :prefix "imgix-"
  :group 'multimedia)

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


(defconst imgix-buffer-url "http://jackangers.imgix.net/chester.png?w=200")
(defconst imgix-params-depends-lookup (imgix-load-json "data/param_depends.json"))
(defconst imgix-params-default-lookup (imgix-load-json "data/default_values.json"))
(defconst imgix-params-title-lookup (imgix-load-json "data/params.json"))
(defconst imgix-params-option-lookup (imgix-load-json "data/param_options.json"))
(defconst imgix-params-code-lookup (ht-flip imgix-params-title-lookup))
(defconst imgix-params-codes (ht-keys imgix-params-title-lookup))
(defconst imgix-params-titles (ht-values imgix-params-title-lookup))
(defconst imgix-params-accepts-url '("mark" "mask" "blend" "txt" "txtfont"))
(defvar imgix-inline-edit-state nil)
(defvar imgix-last-updated-param nil)

;(type-of (ht-get imgix-params-option-lookup "txtalign"))

(defun imgix-save-inline-edit-state (start end url buf)
  "Save the current state for in line editing in imgix."
  (let* ((state (ht-create)))
    (ht-set! state "start" start)
    (ht-set! state "end" end)
    (ht-set! state "url" url)
    (ht-set! state "buffer" buf)
    (ht-set! state "hash" (with-current-buffer buf
                            (md5 (buffer-string))))
	(setq imgix-inline-edit-state state)))

(defun imgix-apply-inline-edit ()
  (interactive)
  (imgix--apply-inline-edit imgix-buffer-url))

(defun imgix--apply-inline-edit (new-url)
  "Finish imgix editing. This replaces selected URL with NEW-URL in that buffer and switches back to it."
  (if imgix-inline-edit-state
    (progn
      (let* ((start-hash (ht-get imgix-inline-edit-state "hash"))
             (start (ht-get imgix-inline-edit-state "start"))
             (end (ht-get imgix-inline-edit-state "end"))
             (buf (ht-get imgix-inline-edit-state "buffer"))
             (cur-hash (with-current-buffer buf
                              (md5 (buffer-string)))))

        (when (string= start-hash cur-hash)
           (with-current-buffer buf
            (goto-char start)
            (delete-region start end)
            (insert new-url))
          (switch-to-buffer buf)))

      (setq imgix-inline-edit-state nil))
     (message "No inline edit to apply.")))

;;;###autoload

;; TODO: valid URL check...
(defun imgix-edit-selected-url (start end)
  "Edit the currently selected URL in imgix."
  (interactive "r")
  (let ((url (buffer-substring start end)))
    (if (imgix-is-url url)
      (progn
        (imgix-save-inline-edit-state start end url (current-buffer))
        (setq imgix-buffer-url url)
        (imgix-display-image))
      (message "Selected text is NOT a URL."))))

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

(defun imgix-is-url (url)
  (let* ((parsed (imgix-parse-url url))
         (scheme (ht-get parsed "scheme"))
         (path (ht-get parsed "path"))
         (host (ht-get parsed "host")))
    (and path (> (length path) 0) host scheme (s-contains? "http" scheme))))

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
             (ido-completing-read prompt-text (imgix-force-front cur-param-value (append '("off") cur-param-options)))
             (read-from-minibuffer
               prompt-text (if (member param imgix-params-accepts-url)
                             (url-unhex-string cur-param-value)
                             cur-param-value)))))

    (ht-set! qs-lookup param (if (string= param-value "off")
                               (ht-get imgix-params-default-lookup param)
                               param-value))

    ;; prompt for all values of all undefined dependency params..
    (when param-depends-check
      (imgix-ensure-param-depends-defined param qs-lookup))

    qs-lookup))

(defun imgix-update-url-param ()
  "Prompt user to help update the imgix url's params."
  (interactive)
  (let* ((parts (imgix-parse-url imgix-buffer-url))
         (qs-lookup (imgix-parse-qs (ht-get parts "query")))
         (param-title (ido-completing-read "Select Param:" (imgix-force-front imgix-last-updated-param imgix-params-titles)))
         (param (ht-get imgix-params-code-lookup param-title)))

    (ht-set parts "query" (imgix-build-qs (imgix--prompt-param-value param qs-lookup t)))
    (setq imgix-last-updated-param param-title)
    (setq imgix-buffer-url (imgix-build-url parts))
    (imgix-display-image)))

(defun imgix-save-image ()
  "Save the current URL to file. Prompt for save path."
  (interactive)
  (url-copy-file imgix-buffer-url (read-file-name "Save image to: " (expand-file-name "~"))))


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
         (new-base (read-from-minibuffer "New Base URL: " base-url)))

  (setq imgix-buffer-url (if (> (length qs) 0)
                           (concat new-base "?" qs)
                           new-base))
  (imgix-display-image)))

(defun imgix-get-base-url ()
  (car (split-string imgix-buffer-url "?")))

(defun imgix-open-in-browser ()
  (interactive)
  (browse-url imgix-buffer-url))

(defun imgix-insert-url (url)
;  (read-only-mode nil)
  (erase-buffer)
  (insert (format "%s\n" url))
  (let ((shr-table-depth 1)) ;; hack to not get shr keymap to load
    (shr-tag-img nil url))
  (goto-char 0)
  (message "Loaded %s" url))
 ; (read-only-mode t))


(defun imgix-display-image ()
  "Display image in *imgix* buffer."
  (get-buffer-create "*imgix*")
  (when (not (get-buffer-window-list "*imgix*"))
    (switch-to-buffer "*imgix*"))
  (imgix-display-mode)
  (when (s-ends-with? "?" imgix-buffer-url)
    (setq imgix-buffer-url (s-replace "?" "" imgix-buffer-url)))
  (with-current-buffer "*imgix*"
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (imgix-insert-url imgix-buffer-url))))

(defvar imgix-display-mode-map (make-sparse-keymap)
  "Keymap for `imgix-display-mode'.")

(defun imgix-display-mode-keymap ()
  "Define keymap for `imgix-display-mode'."
  (define-key imgix-display-mode-map (kbd "u") 'imgix-update-url-param)
  (define-key imgix-display-mode-map (kbd "e") 'imgix-prompt-buffer-url)
  (define-key imgix-display-mode-map (kbd "b") 'imgix-prompt-buffer-url-base)
  (define-key imgix-display-mode-map (kbd "o") 'imgix-open-in-browser)
  (define-key imgix-display-mode-map (kbd "s") 'imgix-save-image)
  (define-key imgix-display-mode-map (kbd "d") 'imgix-apply-inline-edit))


;; TODO: dynamically build font-locks

;; (setq imgix-font-lock-funcs (s-join "\\|" (-sort '(lambda (x y) (< (length x) (length y))) (mapcar (lambda (x) (concat x "=")) imgix-params-codes))))
;; (setq imgix-display-mode-font-lock-keywords '((imgix-font-lock-funcs  .  font-lock-function-name-face)
;;                                               ("?\\|&" . font-lock-constant-face)))
;; (font-lock-add-keywords nil '(imgix-display-mode-font-lock-keywords))

(define-derived-mode imgix-display-mode
  fundamental-mode "imgix-display-mode"
  "Edit images via imgix"
  (imgix-display-mode-keymap)

  (font-lock-add-keywords nil '(("inv=\\|bg=\\|mask=\\|auto=\\|colors=\\|class=\\|palette=\\|markpad=\\|markalpha=\\|markalign=\\|markscale=\\|markfit=\\|markh=\\|markw=\\|mark=\\|q=\\|fm=\\|txtfit=\\|txtlineclr=\\|txtline=\\|txtpad=\\|txtshad=\\|txtalign=\\|txtclr=\\|txtsize=\\|txtfont=\\|txt=\\|bs=\\|bc=\\|bm=\\|balph=\\|ba=\\|bf=\\|bp=\\|bh=\\|bw=\\|blend=\\|px=\\|mono=\\|blur=\\|htn=\\|sepia=\\|sharp=\\|vib=\\|gam=\\|shad=\\|high=\\|exp=\\|con=\\|bri=\\|sat=\\|hue=\\|dpr=\\|or=\\|flip=\\|rot=\\|w=\\|h=\\|fit=\\|crop=" . font-lock-function-name-face)))

  (message "imgix-display-mode enabled"))

(defun imgix ()
  (interactive)
  (imgix-display-image))


(global-set-key (kbd "C-c C-u") 'imgix-edit-selected-url)
(global-set-key (kbd "C-c C-e") 'imgix)

;;;;;REFERENCE:

;; https://github.com/emacs-mirror/emacs/blob/ac34b6b2b9aec5bc262ae1f6c54036de11fa44e9/lisp/dired.el#L1960
;; https://github.com/magit/git-modes/blob/master/git-commit-mode.el
;; https://github.com/bbatsov/emacs-lisp-style-guide
;; http://stackoverflow.com/questions/14885670/create-new-mode-in-emacs/14887163#14887163
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/net/eww.el

(provide 'imgix)
;;; imgix.el ends here
