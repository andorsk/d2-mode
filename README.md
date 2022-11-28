<!-- [![MELPA](https://melpa.org/packages/d2-mode-badge.svg)](https://melpa.org/#/d2-mode) -->

# d2-mode

A [d2](https://github.com/andorsk/d2-mode) extension for Emacs. This was heavily
inspired (it's basically a fork) of [Mermaid Mode](https://github.com/abrochard/mermaid-mode).

**Status:** Work in Progress

## Why D2

Text to graph diagrams are awesome. I used Mermaid.js all the time and it was
fantastic, but there were a few things it couldn't do so I wanted to expand my
options.

Some things that I've noticed d2 has some interesting support in:

1. More themes
2. Code Blocks
3. More visual customization
4. More graph support
5. Autoformat

Learn more about [d2 here](https://d2lang.com/tour/intro/)

## Installation

1. Load the d2-mode.el file ( Melpa not available yet )
2. Install d2 binary from the d2 project if you plan to compile graphs in Emacs

## Usage

```text
C-c C-c - compile current file to an image
C-c C-f - compile given file to an image
C-c C-b - compile current buffer to an image
C-c C-r - compile current region to an image
C-c C-o - open in the live editor
C-c C-d - open the official doc
```

Note: All compile commands will open the output in a buffer to view the resulting image.

## Customization

### `d2` binary location

You can specify the location of `d2` with the variable `d2-location`, the default assumes you have the binary in your `PATH` (and for that you probably want/need to install [`d2`](https://github.com/andorsk/d2-mode)).

### Output format

By default `d2` will compile to svg format. You can change that by setting the variable `d2-output-format`.

### Temp directory

By default `d2-tmp-dir` points to `\tmp\`. Feel free to set it to a more appropriate location that works for you (e.g. on windows).

### Key bindings

To customize the key bindings but this into your `init.el` ...

```elisp
(defvar d2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'd2-compile)
    (define-key map (kbd "C-c C-f") 'd2-compile-file)
    (define-key map (kbd "C-c C-b") 'd2-compile-buffer)
    (define-key map (kbd "C-c C-r") 'd2-compile-region)
    (define-key map (kbd "C-c C-o") 'd2-open-browser)
    (define-key map (kbd "C-c C-d") 'd2-open-doc)
    (define-key map (kbd "C-c C-w") 'd2-compile-and-watch-file)
    (define-key map (kbd "C-c C-K") 'd2-kill-all-processes)
    (define-key map (kbd "C-c C-s") 'd2-show-processes)
    (define-key map (kbd "C-c C-k") 'd2-kill-process)
    map))
```

## Bugs & Issues

Feel free to open an issue!
