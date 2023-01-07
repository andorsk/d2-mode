;;; d2-mode.el --- Major mode for working with d2 graphs -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "26.1"))

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

;; By default `d2` will compile to `svg` format.
;; You can change that by setting the variable `d2-output-format`.

;; By default `d2` will use `/tmp` to store tmp-files.
;; You can change that by setting the variable `d2-tmp-dir`.

;; This code was heavily inspired by mermaid-mode @
;; https://github.com/abrochard/mermaid-mode

;; STATE: Alpha. Up for review by Melpa.
;;

(require 'browse-url)
(require 'ob)
(require 'ob-eval)

;;; Code:

(defgroup d2 nil
  "Major mode for working with d2 graphs."
  :group 'extensions
  :link '(url-link :tag "Repository" "https://github.com/andorsk/d2-mode"))

(defcustom d2-location "d2"
  "D2 binary location."
  :type 'string
  :group 'd2)

(defcustom d2-output-format ".svg"
  "D2 output format."
  :group 'd2
  :type 'string)

(defcustom d2-tmp-dir (temporary-file-directory)
  "Dir for tmp files."
  :group 'd2
  :type 'string)

(defcustom d2-flags ""
  "Additional flags to pass to the d2-cli."
  :group 'd2
  :type 'string)

(defconst d2-font-lock-keywords
  `((,(regexp-opt '("shape" "md" ) 'words) . font-lock-keyword-face)
    ("---\\|-?->*\\+?\\|==>\\|===|->" . font-lock-variable-name-face)
    (":\\|{\\|}\\|\|\\|+" . font-lock-builtin-face)
    (,(regexp-opt '("go" "js") 'lang) .  font-lock-preprocessor-face)
    (,(regexp-opt '("class" "string" ) 'words2) .  font-lock-type-face)))

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
                       (error "D2 requires a \":file\" header argument")))
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

(defcustom d2fmt-args nil
  "Additional arguments to pass to d2fmt."
  :type '(repeat string)
  :group 'd2)

(defcustom d2fmt-show-errors 'buffer
  "Where to display d2fmt error output.
It can either be displayed in its own buffer, in the echo area, or not at all.
Please note that Emacs outputs to the echo area when writing
files and will overwrite d2fmt's echo output if used from inside
a `before-save-hook'."
  :type '(choice
          (const :tag "Own buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'd2)

(defun d2-compile ()
  "Compile the current d2 file using d2."
  (interactive)
  (d2-compile-file (buffer-file-name)))

(defun d2-compile-buffer ()
  "Compile the current d2 buffer using d2."
  (interactive)
  (let* ((tmp-file-name (concat d2-tmp-dir "current-buffer.d2")))
    (write-region (point-min) (point-max) tmp-file-name)
    (d2-compile-file-with-options tmp-file-name)))

(defun d2-compile-region (&optional browse)
  "Compile the current d2 region using d2.
Optional argument BROWSE locate a certain declaration."
  (interactive)
  (let* ((tmp-file-name (concat d2-tmp-dir "current-region.d2")))
    (when (use-region-p)
      (write-region (region-beginning) (region-end) tmp-file-name)
      (d2-compile-file tmp-file-name browse))))

(defun d2-compile-region-and-browse ()
  "Compile the current d2 region using d2 and browse on browser."
  (interactive)
  (d2-compile-region t))

(defun d2-compile-file (file-name &optional browse)
  "Compile the given d2 file using d2. BROWSE option determine opening method.
Argument FILE-NAME the input file."
  (interactive "fFilename: ")
  (let* ((input file-name)
         (output (concat (file-name-sans-extension input) d2-output-format)))
    (apply #'call-process d2-location nil "*d2*" nil (list input output))
    (if (equal browse t)
      (progn
        (d2-browse-file output))
      (progn
        (display-buffer (find-file-noselect output t))))))

(defun d2-view-current-svg ()
  "View the current svg in the browser."
  (interactive)
  (d2-browse-file buffer-file-name))

(defun d2-browse-file (file-name)
  "View a file in the browser.
Argument FILE-NAME the input file."
  (interactive "fFilename: ")
  (let ((url file-name))
    (browse-url url)))

(defun d2-compile-file-with-options (file-name &optional browse)
  "Compile file with options.
Argument FILE-NAME the input file.
Optional argument BROWSE whether to open the browser."
  (interactive "fFilename: ")
  (message "compiling with current options")
  (d2-compile-file file-name browse))

(defun d2-compile-buffer-and-browse()
  "Compile buffer and browse."
  (interactive)
  (d2-compile-file-with-options buffer-file-name t))

(defun d2-open-doc ()
  "Open the d2 home page and doc."
  (interactive)
  (browse-url "https://github.com/terrastruct/d2"))


(defun d2fmt()
  "Format the current buffer according to the formatting tool.
Code was heavily inspired by gofmt.
https://github.com/dominikh/go-mode.el/\
blob/166dfb1e090233c4609a50c2ec9f57f113c1da72/go-mode.el
The tool used can be set via ‘d2-location’ (default: d2fmt) and additional
arguments can be set as a list via ‘d2fmt-args’."

  (interactive)
  (let ((tmpfile (make-nearby-temp-file "d2fmt" nil ".d2"))
       (patchbuf (get-buffer-create "*d2fmt patch*"))
       (errbuf (if d2fmt-show-errors (get-buffer-create "*d2fmt Errors*")))
       (coding-system-for-read 'utf-8)
       (coding-system-for-write 'utf-8)
       our-d2fmt-args)

  (unwind-protect
      (save-restriction
        (widen)
        (if errbuf
             (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
        (with-current-buffer patchbuf
            (erase-buffer))

        (write-region nil nil tmpfile)

        (message "Calling d2fmt: %s %s" d2-location our-d2fmt-args)
        (if (zerop (apply #'process-file d2-location nil errbuf nil our-d2fmt-args))
            (progn
              (if (zerop (let ((local-copy (file-local-copy tmpfile)))
                           (unwind-protect
                               (call-process-region
                                  (point-min) (point-max) "diff" nil patchbuf
                                  nil "-n" "-" (or local-copy tmpfile))
                               (when local-copy (delete-file local-copy)))))
                           (message "Buffer is already d2fmt")
                        (message "Applied d2fmt"))
              (if errbuf (d2--kill-error-buffer errbuf)))
          (message "Could not apply d2fmt")
          (if errbuf (d2fmt--process-errors (buffer-file-name) tmpfile errbuf))))
          (kill-buffer patchbuf)
          (delete-file tmpfile))))



(defun d2fmt--kill-error-buffer (errbuf)
  "Utility function that kill the error buffer.
Argument ERRBUF the buffer which contains the error message."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

(defun d2fmt--process-errors (filename tmpfile errbuf)
  "Utility function to process errors
Argument FILENAME the input file.
Argument TMPFILE the path to the temporary file.
Argument TMPFILE error buffer."
  (with-current-buffer errbuf
    (if (eq d2fmt-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (d2fmt--kill-error-buffer errbuf))
      ;; Convert the d2fmt stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "d2fmt errors:\n")
      (let ((truefile
             (while (search-forward-regexp
                (concat "^\\(" (regexp-quote (file-local-name truefile))
                        "\\):")
                nil t)
          (replace-match (file-name-nondirectory filename) t t nil 1)))
      (compilation-mode)
      (display-buffer errbuf))))
(defvar d2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'd2-compile)
    (define-key map (kbd "C-c C-f") 'd2-compile-file)
    (define-key map (kbd "C-c C-b") 'd2-compile-buffer)
    (define-key map (kbd "C-c C-r") 'd2-compile-region)
    (define-key map (kbd "C-c C-m") 'd2-compile-file-and-browse)
    (define-key map (kbd "C-c C-j") 'd2-compile-buffer-and-browse)
    (define-key map (kbd "C-c C-k") 'd2-compile-region-and-browse)
    (define-key map (kbd "C-c C-o") 'd2-open-browser)
    (define-key map (kbd "C-x C-o") 'd2-view-current-svg)
    (define-key map (kbd "C-c C-d") 'd2-open-doc)
    map))

;;;###autoload
(define-derived-mode d2-mode prog-mode "d2"
  :syntax-table d2-syntax-table
  (setq-local font-lock-defaults '(d2-font-lock-keywords))
  (setq-local comment-start "%%")
  (setq-local comment-end "")
  (setq-local comment-start-skip "%%+ *"))


(defun d2fmt-before-save ()
  "Add this to .emacs to run d2fmt on the current buffer when saving:
\(add-hook 'before-save-hook 'd2fmt-before-save).
Note that this will cause ‘d2-mode’ to get loaded the first time
you save any file, kind of defeating the point of autoloading."

  (interactive)
  (when (eq major-mode 'd2-mode) (d2fmt)))


(provide 'd2-mode)

;;; d2-mode.el ends here
