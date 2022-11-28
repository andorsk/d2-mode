;;; d2-mode.el --- major mode for working with d2 graphs -*- lexical-binding: t; -*-

;; Author: Andor Kesselman <andor@henosisknot.com>
;; Copyright (C) 2022, Andor Kesselman
;; Heavily inspired by Mermaid Mode
;; https://github.com/abrochard/mermaid-mode
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Version: 1.0
;; Author: Andor Kesselman
;; Keywords: d2 graphs tools processes
;; URL: https://github.com/andorsk/d2-mode
;; License: GNU General Public License >= 3
;; Package-Requires: ((f "0.20.0") (emacs "25.3"))

;;; Commentary:

;; Major mode for working with d2 graphs.
;; See https://github.com/terrastruct/d2

;;; Usage:

;; Currently supporting flow charts and sequence diagrams with syntax coloring and indentation.

;; C-c C-c to compile to an image
;; C-c C-f to compile file to an image
;; C-c C-r to compile region to an image
;; C-c C-b to compile buffer to an image
;; C-c C-o to open in the live editor
;; C-c C-d to open the official doc

;;; Customization:

;; You can specify the location of `d2` with the variable `d2-bin-location`,
;; the default assumes you have the binary in your exec PATH.

;; By default `d2` will compile to `png` format.
;; You can change that by setting the variable `d2-output-format`.

;; By default `d2` will use `/tmp` to store tmp-files.
;; You can change that by setting the variable `d2-tmp-dir`.

;; This code was inspired by mermaid-mode @
;; https://github.com/abrochard/mermaid-mode


;; STATE: Currently this is a work in progress
;;
;; Code

(require 'f)
(require 'browse-url)
(require 'ob)
(require 'ob-eval)

(defgroup d2-mode nil
  "Major mode for working with d2 graphs."
  :group 'extensions
  :link '(url-link :tag "Repository" "https://github.com/andorsk/d2-mode"))

(defcustom d2-location "d2"
  "d2 binary location"
  :type 'string
  :group 'd2-mode)

(defcustom d2-output-format ".svg"
  "d2 output format."
  :group 'd2-mode
  :type 'string)

(defcustom d2-tmp-dir "/tmp/"
  "Dir for tmp files."
  :group 'd2-mode
  :type 'string)

(defcustom d2-flags ""
  "Additional flags to pass to the d2-cli."
  :group 'd2-mode
  :type 'string)

(defconst d2-font-lock-keywords
  `((,(regexp-opt '("shape" "md" ) 'words) . font-lock-keyword-face)
    ("---\\|-?->*\\+?\\|==>\\|===|->" . font-lock-function-name-face)))

(defvar d2-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Comment style "%% ..."
    (modify-syntax-entry ?% ". 124" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    syntax-table)
  "Syntax table for `d2-mode'.")

(defvar org-babel-default-header-args:d2
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a d2 source block.")

(defun org-babel-execute:d2 (body params)
  "Execute command with BODY and PARAMS from src block."
  (let* ((out-file (or (cdr (assoc :file params))
                       (error "Mermaid requires a \":file\" header argument")))
         (temp-file (org-babel-temp-file "d2-"))
         (cmd (concat (shell-quote-argument d2-location)
                      " -o " (org-babel-process-file-name out-file)
                      " -i " temp-file
                      " " d2-flags)))
    (with-temp-file temp-file (insert body))
    (org-babel-eval cmd "")
    nil))

(defun d2--locate-declaration (str)
  "Locate a certain declaration and return the line difference and indentation.

STR is the declaration."
  (let ((l (line-number-at-pos)))
    (save-excursion
      (if (re-search-backward str (point-min) t)
          (cons (- l (line-number-at-pos)) (current-indentation))
        (cons -1 -1)))))

;; (defun d2-indent-line ()
;;   "Indent the current line."
;;   (interactive)
;;   (save-excursion
;;     (end-of-line)
;;     (let ((graph (d2--locate-declaration "^graph\\|sequenceDiagram"))
;;           (subgraph (d2--locate-declaration "subgraph \\|loop \\|alt \\|opt"))
;;           (both (d2--locate-declaration "^graph \\|^sequenceDiagram$\\|subgraph \\|loop \\|alt \\|opt"))
;;           (else (d2--locate-declaration "else "))
;;           (end (d2--locate-declaration "^ *end *$")))
;;       (indent-line-to
;;        (cond ((equal (car graph) 0) 0) ;; this is a graph declaration
;;              ((equal (car end) 0) (cdr subgraph)) ;; this is "end", indent to nearest subgraph
;;              ((equal (car subgraph) 0) (+ 4 (cdr graph))) ;; this is a subgraph
;;              ((equal (car else) 0) (cdr subgraph)) ;; this is "else:, indent to nearest alt
;;              ;; everything else
;;              ((< (car end) 0) (+ 4 (cdr both))) ;; no end in sight
;;              ((< (car both) (car end)) (+ 4 (cdr both))) ;; (sub)graph declaration closer, +4
;;              (t (cdr end)) ;; end declaration closer, same indent
;;              )))))

(defun d2-compile ()
  "Compile the current d2 file using d2."
  (interactive)
  (d2-compile-file (buffer-file-name)))

(defun d2-compile-buffer ()
  "Compile the current d2 buffer using d2."
  (interactive)
  (let* ((tmp-file-name (concat d2-tmp-dir "current-buffer.d2")))
    (write-region (point-min) (point-max) tmp-file-name)
    (d2-compile-file tmp-file-name)))

(defun d2-compile-region ()
  "Compile the current d2 region using d2."
  (interactive)
  (let* ((tmp-file-name (concat d2-tmp-dir "current-region.d2")))
    (when (use-region-p)
      (write-region (region-beginning) (region-end) tmp-file-name)
      (d2-compile-file tmp-file-name))))

(defun d2-compile-file (file-name)
  "Compile the given d2 file using d2."
  (interactive "fFilename: ")
  (let* ((input file-name)
         (output (concat (file-name-sans-extension input) d2-output-format)))
    (apply #'call-process d2-location nil "*d2*" nil (list input output))
    (display-buffer (find-file-noselect output t))))

(defun process-signal (process signal)
  (when (memq (process-status process) '(exit signal))
    (message "Do something!")
    (shell-command-sentinel process signal)))

(defun d2-compile-and-watch-file ()
  ;; async watching of file
  (interactive)
  (let* ((output-buffer (generate-new-buffer "*d2 Watch Mode*"))
       (proc (progn
               (async-shell-command "/opt/homebrew/bin/d2 -w /Users/akmb2/Downloads/input.d2 /tmp/output2.svg" output-buffer)
               (get-buffer-process output-buffer))))
  (if (process-live-p proc)
      (set-process-sentinel proc #'process-signal)
    (message "No process running."))))

(defun d2-kill-all-processes ()
  ((interactive))
  (message "not implemented"))

(defun d2-kill-processes ()
  ((interactive))
  (message "not implemented"))

(defun d2-show-proccesses ()
  (interactive)
  (message "not implemented"))

(defun d2-open-doc ()
  "Open the d2 home page and doc."
  (interactive)
  (browse-url "https://github.com/terrastruct/d2"))

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

;;;###autoload
(define-derived-mode d2-mode prog-mode "d2"
  :syntax-table d2-syntax-table
  (setq-local font-lock-defaults '(d2-font-lock-keywords))
  (setq-local indent-line-function 'd2-indent-line)
  (setq-local comment-start "%%")
  (setq-local comment-end "")
  (setq-local comment-start-skip "%%+ *"))

(provide 'd2-mode)

;;; d2-mode.el ends here
