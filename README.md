[![MELPA](https://melpa.org/packages/d2-mode-badge.svg)](https://melpa.org/#/d2-mode)
[![build](https://github.com/andorsk/d2-mode/actions/workflows/build.yml/badge.svg)](https://github.com/andorsk/d2-mode/actions/workflows/build.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# d2-mode

A [d2](https://github.com/terrastruct/d2) extension for Emacs. This was heavily
inspired of [Mermaid Mode](https://github.com/abrochard/mermaid-mode).

**Status:** Alpha. Available on Melpa. 

It works you can use this to render d2 in both your browser and in an emacs buffer. See [Bugs & Issues](#bugs--issues) for known issues.

<div>
  <img height="500px" alt="image" src="https://user-images.githubusercontent.com/8604639/204498003-08bd0e05-0e0b-4d60-8d94-1ed95a4a7cd3.png">
</div>

<div>
  <img height="500px" alt="image" src="tutorial.gif">
</div>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [d2-mode](#d2-mode)
- [Why D2](#why-d2)
- [Installation](#installation)
- [Usage](#usage)
- [Customization](#customization)
- [`d2` binary location](#d2-binary-location)
- [Output format](#output-format)
- [Temp directory](#temp-directory)
- [Key bindings](#key-bindings)
- [Bonus](#bonus)
- [Roadmap](#roadmap)
- [Bugs & Issues](#bugs--issues)
- [Contributors](#contributors)

<!-- markdown-toc end -->

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

1. Load the d2-mode.el file or install from melpa: `M-x package-install d2-mode`
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

You can specify png or svg output format by putting this in your `init.el` file:

```(setq d2-output-format "<OUTPUT_FORMAT")```

Options are: 
- `.png`
- `.svg`

Default is SVG.

### Temp directory

By default `d2-tmp-dir` points to `\tmp\`. Feel free to set it to a more appropriate location that works for you (e.g. on windows).

### Key bindings

To customize the key bindings put this into your `init.el` ...

```elisp
(defvar d2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'd2-compile)
    (define-key map (kbd "C-c C-f") 'd2-compile-file)
    (define-key map (kbd "C-c C-b") 'd2-compile-buffer)
    (define-key map (kbd "C-c C-r") 'd2-compile-region)
    (define-key map (kbd "C-c C-h") 'd2-compile-file-and-browse)
    (define-key map (kbd "C-c C-j") 'd2-compile-buffer-and-browse)
    (define-key map (kbd "C-c C-k") 'd2-compile-region-and-browse)
    (define-key map (kbd "C-c C-o") 'd2-open-browser)
    (define-key map (kbd "C-x C-o") 'd2-view-current-svg)
    (define-key map (kbd "C-c C-d") 'd2-open-doc)
    map))

```

### Bonus

See the `snippets` folder for yas snippets. Start making d2 graphs today!

### Roadmap

- Even better syntax highlighting and formatting
- Watch mode support

## Bugs & Issues

- SVG's have issues rending in emacs sometimes. See
  [#13](https://github.com/andorsk/d2-mode/issues/13) and
  [#8](https://github.com/andorsk/d2-mode/issues/8) for more details. You can use the
  `*browse` commands to render it directly in your browser instead!

Feel free to open an issue and or contribute to this repo over a PR!

## Contributors

We thank anyone that decides to contribute to this repository and encourage
contributions.

<a href="https://github.com/andorsk/d2-mode/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=andorsk/d2-mode" />
</a>

Also thank you to @suliveevil for bringing up issues.
