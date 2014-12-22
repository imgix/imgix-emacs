imgix-emacs
===========

An emacs package for easily, and visually, editing images via [imgix](http://www.imgix.com). You can then easily use the imgix URL in your code or save the image.


* [Dependencies](#dependencies)
* [Installation](#install)
* [Getting Started](#getting-started)

<a name="dependencies"></a>
Dependencies
------------
* Emacs 24.4.1+
* [imgix](http://www.imgix.com) account (to use your own images)

<a name="install"></a>
Installation
------------

** TODO **

<a name="getting-started"></a>
Getting Started
---------------

There are two main ways to enter the `*imgix*` buffer for editing images.

Use `imgix-edit-selected-url` to edit the imgix URL that is currently selected in a buffer. When you're done editing you can press <kbd>d</kbd> to replace the selected URL in that buffer with your edits.

Use `imgix` to enter the the `*imgix*` buffer directly.

To set up shortcut keys for these add the following to your `.emacs` or use <kbd>M</kbd>+<kbd>x</kbd> to trigger them by name.

    (global-set-key (kbd "C-c C-u") 'imgix-edit-selected-url)
    (global-set-key (kbd "C-c C-i") 'imgix)


Editing Images
--------------
When in the `*imgix*` buffer the following default key bindings are available:

* <kbd>u</kbd> - Update imgix URL params (prompts for param and value) `imgix-update-url-param`
* <kbd>e</kbd> - Edit full imgix URL `imgix-prompt-buffer-url`
* <kbd>b</kbd> - Edit base imgix URL (keep applied params onnew base) `imgix-prompt-buffer-url-base`
* <kbd>o</kbd> - Open image in your default browser `imgix-open-in-browser`
* <kbd>s</kbd> - Save image to disk `imgix-save-image`
* <kbd>d</kbd> - If entered `*imgix*` via `imgix-edit-selected-url` then replaces URL in that buffer with current `*imgix*` URL  `imgix-apply-inline-edit`

To override the default key bindings put the following your `.emacs` with your new key bindings in the `kbd` expression.

    (setq imgix-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "u") 'imgix-update-url-param)
        (define-key map (kbd "e") 'imgix-prompt-buffer-url)
        (define-key map (kbd "b") 'imgix-prompt-buffer-url-base)
        (define-key map (kbd "o") 'imgix-open-in-browser)
        (define-key map (kbd "s") 'imgix-save-image)
        (define-key map (kbd "d") 'imgix-apply-inline-edit)
        map))
