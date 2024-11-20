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

(defcustom d2-flags nil
  "Additional flags to pass to the d2-cli."
  :group 'd2
  :type '(repeat string))

(defcustom d2-indent 2
  "Number of columns to indent d2 blocks."
  :group 'd2
  :type 'integer
  :safe #'integerp)

(defconst d2-font-lock-keywords
  `((,(regexp-opt '("shape" "md" ) 'words) . font-lock-keyword-face)
    ("---\\|-?->*\\+?\\|==>\\|===|->" . font-lock-variable-name-face)
    ("#.*" .  font-lock-comment-face )
    (":\\|{\\|}\\|\|\\|+" . font-lock-builtin-face)
    (,(regexp-opt '("go" "js") 'lang) .  font-lock-preprocessor-face)
    (,(regexp-opt '("class" "string" ) 'words2) .  font-lock-type-face)))

(defvar d2-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Comment style "# ..."
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
         (cmd (mapconcat #'shell-quote-argument
                         (append (list d2-location
                                       temp-file
                                       (org-babel-process-file-name out-file))
                                 d2-flags)
                         " ")))
    (with-temp-file temp-file (insert body))
    (org-babel-eval cmd "")
    nil))

(defun d2--locate-declaration (str tag)
  "Locate a certain declaration and return the line difference and indentation.
STR is the declaration.
TAG is a symbol, which is prepended to the declaration data."
  (let ((l (line-number-at-pos)))
    (save-excursion
      (if (re-search-backward str (point-min) t)
          (cons tag (cons (- l (line-number-at-pos)) (current-indentation)))
        (cons tag (cons -1 -1))))))

(defun d2--combine-tokens-by-level (tokens)
  "Combine TOKENS that are at the same level.
Collect the tags from both into a list.
Use the newer token's line and column."
  (reverse
   (cl-reduce (lambda (acc el)
                (if (and (not (null acc))
                         (equal (d2--decl-line (car acc))
                                (d2--decl-line el)))
                    ;; when tokens are on the same line, combine them
                    (progn
                      (setf (car acc)
                            ;; we don't care about duplicate tags
                            (cons (cons (d2--decl-tag el) (d2--decl-tag (car acc)))
                                  (cons (d2--decl-line el)
                                        (d2--decl-column el))))
                      acc)
                  ;; otherwise add a new token; set its tag to be a list
                  (cons (cons (list (d2--decl-tag el))
                              (cons (d2--decl-line el)
                                    (d2--decl-column el)))
                        acc)))
              tokens
              :initial-value ())))

(defun d2--sort-tokens-by-line (tokens)
  "Sort TOKENS by line number."
  (sort tokens
        (lambda (token1 token2)
          (< (d2--decl-line token1)
                        (d2--decl-line token2)))))

(defun d2--filter-not-found-tokens (tokens)
  "Filter TOKENS that were not detected.
They have a line number of -1"
  (cl-remove-if
   (lambda (token) (< (d2--decl-line token) 0))
   tokens))

(defun d2--parse-from-line ()
  "Parse by starting at current line and searching backward."
  (let ((node (d2--locate-declaration
               (rx (group (one-or-more (any alnum "_")))
                   (? (group ":"
                             (one-or-more space)))
                   (? (group
                       (one-or-more (not (any ?{ ?}))))))
               'node))
        (subnode-start (d2--locate-declaration
                        (rx (group (one-or-more (any alnum "_" "<->"
                                                     "->" "--" "<-"))
                                   (? (group ":" (one-or-more space)))
                                   (? (group (one-or-more (not (any ?{ ?})))))
                                   "{"))
                        'subnode))
        (end (d2--locate-declaration "^ *} *$"
                                     'end))
        (connection (d2--locate-declaration
                     (rx (group (one-or-more (any alnum "_" "-"))
                                (any "->"
                                     "<->"
                                     "--"
                                     "<-")
                                (? (seq
                                    ":"
                                    (one-or-more space)))
                                (? (one-or-more graph))))
                     'connection)))
    (list node subnode-start end connection)))

;;; declaration part handling
(defun d2--decl-tag (decl)
  "Get tag from the declaration DECL.  It is the list head."
  (car decl))
(defun d2--decl-line (decl)
  "Get the text of the declaration DECL.  It is the second item in the list."
  (cadr decl))
(gv-define-setter d2--decl-line (val decl)
  "Allow line VAL to be setf in declaration DECL."
  `(setf (cadr ,decl) ,val))
(defun d2--decl-column (decl)
  "Get the column number from DECL.  It is the third item."
  (cddr decl))
(defun d2--decl-tags-contain (decl tag)
  "Check if any of the tags in DECL matches TAG."
  (seq-find (lambda (tag2)
              (equal tag2 tag))
            (d2--decl-tag decl)))

(defun d2--calculate-desired-indentation ()
  "Calculate indentation of the current line.
Scan the tokens backwards and accumulate a list.
Only one token from each type is in this list.
To accommodate nested structures, we scan twice.
This way two tokens of the same type can be detected in sequence.
Once this information is collected, the indentation is chosen based
on the types of the current and previous token,
and the indentation of the previous line."
  (save-excursion
    (end-of-line)
    ;; sort tokens by line number (distance from current line)
    ;; and calculate the final indentation
    (let* ((ordered-tokens (d2--combine-tokens-by-level
                            (d2--sort-tokens-by-line
                             (append (d2--filter-not-found-tokens (d2--parse-from-line))
                                     ;; also collect tokens starting at previous line
                                     ;; otherwise successive tokens of the same type
                                     ;; may not be detected
                                     (if (equal (line-number-at-pos) 1)
                                         ()
                                       (progn (forward-line -1)
                                              (end-of-line)
                                              (mapcar (lambda (token)
                                                        ;; increment line number
                                                        ;; to make it relative to starting line
                                                        (cl-incf (d2--decl-line token))
                                                        token)
                                                      (d2--filter-not-found-tokens
                                                       (d2--parse-from-line)))))))))
           (current-token (car ordered-tokens))
           (previous-token (cadr ordered-tokens)))
      (message "tokens %s" (mapcar (lambda (token) (d2--decl-tag token)) ordered-tokens))
      (cond ((and (d2--decl-tags-contain current-token 'node)
                  (null previous-token))
             0)

            ((and (d2--decl-tags-contain current-token 'node)
                  (d2--decl-tags-contain previous-token 'subnode))
             (+ d2-indent (d2--decl-column previous-token)))

            ((and (d2--decl-tags-contain current-token 'subnode)
                  (d2--decl-tags-contain previous-token 'subnode))
             (+ d2-indent (d2--decl-column previous-token)))


            ((and (d2--decl-tags-contain current-token 'node)
                  (d2--decl-tags-contain previous-token 'node))
             (d2--decl-column previous-token))

            ((and (d2--decl-tags-contain current-token 'node)
                  (d2--decl-tags-contain previous-token 'end))
             (d2--decl-column previous-token))

            ((and (d2--decl-tags-contain current-token 'end)
                  (d2--decl-tags-contain previous-token 'end))
             (max (- (d2--decl-column previous-token) d2-indent) 0))

            ((and (d2--decl-tags-contain current-token 'end)
                  (d2--decl-tags-contain previous-token 'subnode))
             (d2--decl-column previous-token))

            ((and (d2--decl-tags-contain current-token 'end)
                  (d2--decl-tags-contain previous-token 'node))
             (max (- (d2--decl-column previous-token) d2-indent) 0))

            (t (progn (message "uknown syntax %s" current-token)
                      (d2--decl-column current-token)))))))

(defun d2-indent-line ()
  "This is the actual function called by Emacs to indent."
  (interactive)
  (indent-line-to (d2--calculate-desired-indentation)))

(defcustom d2format-args nil
  "Additional arguments to pass to d2format."
  :type '(repeat string)
  :group 'd2)

(defcustom d2format-show-errors 'buffer
  "Where to display d2format error output.
It can either be displayed in its own buffer, in the echo area, or not at all.
Please note that Emacs outputs to the echo area when writing
files and will overwrite d2format's echo output if used from inside
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
    (apply #'call-process d2-location nil "*d2*" nil (append (list input output) d2-flags))
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


(defun d2format()
  "Format the current buffer according to the formatting tool.
Code was heavily inspired by gofmt.
https://github.com/dominikh/go-mode.el/\
blob/166dfb1e090233c4609a50c2ec9f57f113c1da72/go-mode.el
The tool used can be set via ‘d2-location’ (default: d2format) and additional
arguments can be set as a list via ‘d2format-args’."

  (interactive)
  (let ((tmpfile (make-nearby-temp-file "d2format" nil ".d2"))
       (patchbuf (get-buffer-create "*d2format patch*"))
       (errbuf (if d2format-show-errors (get-buffer-create "*d2format Errors*")))
       (coding-system-for-read 'utf-8)
       (our-d2format-args "")
       (coding-system-for-write 'utf-8))

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

        (message "Calling d2format: %s %s" d2-location "fmt")

        (if (zerop (apply #'process-file d2-location nil errbuf nil ""))
            (message "processing file")
            (progn
              (if (zerop (let ((local-copy (file-local-copy tmpfile)))
                           (message (concat "running local copy " local-copy))
                           (unwind-protect
                               (call-process-region
                                  (point-min) (point-max) "diff" nil patchbuf
                                  nil "-n" "-" (or local-copy tmpfile))
                               (when local-copy (delete-file local-copy)))))
                           (message "Buffer is already d2format")
                        (message "Applied d2format"))
              (if errbuf (d2format--kill-error-buffer errbuf)))
          (message "Could not apply d2format")
          (if errbuf (d2format--process-errors (buffer-file-name) tmpfile errbuf))))
          (kill-buffer patchbuf)
          (delete-file tmpfile))))

(defun d2format--kill-error-buffer (errbuf)
  "Utility function that kill the error buffer.
Argument ERRBUF the buffer which contains the error message."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

(defun d2format--process-errors (filename tmpfile errbuf)
  "Utility function that provides error handling.
Argument FILENAME the input file.
Argument TMPFILE the path to the temporary file.
Argument ERRBUF error buffer."
  (with-current-buffer errbuf
    (if (eq d2format-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (d2format--kill-error-buffer errbuf))
      ;; Convert the d2format stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "d2format errors:\n")
      (let ((truefile tmpfile))
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
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#")
  (setq-local indent-line-function 'd2-indent-line))

(defun d2format-before-save ()
  "Add this to .emacs to run d2format on the current buffer when saving:
\=(add-hook 'before-save-hook 'd2format-before-save).
Note that this will cause ‘d2-mode’ to get loaded the first time
you save any file, kind of defeating the point of autoloading."

  (interactive)
  (when (eq major-mode 'd2-mode) (d2format)))


(provide 'd2-mode)

;;; d2-mode.el ends here
