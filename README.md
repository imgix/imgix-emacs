![imgix logo](https://assets.imgix.net/imgix-logo-web-2014.pdf?page=2&fm=png&w=200&h=200)

imgix-emacs [![Build Status](https://travis-ci.org/imgix/imgix-emacs.svg?branch=master)](https://travis-ci.org/imgix/imgix-emacs) [![MELPA](http://melpa.org/packages/imgix-badge.svg)](http://melpa.org/#/imgix)
===========

![imgix emacs text example](https://jackangers.imgix.net/imgix_emacs_txt_example.gif)

An emacs package for easily, and visually, editing images via [imgix](http://www.imgix.com). You can then easily use the generated imgix URL in your code or save the image.


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

`imgix` is availabe on [MELPA](http://melpa.milkbox.net/)

Add a package archive to your `~/.emacs.d/init.el` or `~/.emacs`:

    (require 'package)
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

then run `M-x package-install <RET> imgix <RET>`


<a name="getting-started"></a>
Getting Started
---------------

There are two main ways to enter the `*imgix*` buffer for editing images.

Use `imgix-edit-url-at-url` to edit the imgix URL that is currently under the cursor. When you're done editing you can press <kbd>d</kbd> to replace the selected URL in that buffer with your edits.

Use `imgix` to enter the the `*imgix*` buffer directly.

To set up shortcut keys for these add the following to your `.emacs` or use <kbd>M</kbd>+<kbd>x</kbd> to trigger them by name.

    (global-set-key (kbd "C-c C-u") 'imgix-edit-url-at-point)
    (global-set-key (kbd "C-c C-i") 'imgix)


Editing Images
--------------
When in the `*imgix*` buffer the following default key bindings are available:

| shortcut   | about  | function  |
|---|---|---|
| <kbd>u</kbd>  | Update imgix URL params (prompts for param and value)  | `imgix-update-url-param`  |
| <kbd>e</kbd>  | Edit full imgix URL   | `imgix-prompt-buffer-url`  |
| <kbd>b</kbd>  | Edit base imgix URL (keep applied params on new base)  | `imgix-prompt-buffer-url-base`  |
| <kbd>o</kbd>  | Open image in your default browser   | `imgix-open-in-browser`  |
| <kbd>s</kbd>  | Save image to disk   | `imgix-save-image`  |
| <kbd>d</kbd>  | Replace generated imgix URL in the buffer where `imgix-edit-url-at-point` was called.  | `imgix-apply-inline-edit`  |
| <kbd>p</kbd>  | Select/Apply a preset to the current image.  | `imgix-prompt-preset-apply`  |
| <kbd>x</kbd>  | Save current imgix params as a new preset.  | `imgix-prompt-preset-save`  |


To override the default key bindings put the following your `.emacs` with your new key bindings in the `kbd` expression.

    (setq imgix-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "r") 'imgix-update-url-param) ;; to use "r" instead of "u" for updating param in *imgix*
        map))
