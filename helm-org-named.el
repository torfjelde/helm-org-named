;;; helm-org-named.el --- Helm completion for named blocks with Org-mode  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022  tor

;; Author: Tor Erlend Fjelde <tor.github@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)

(require 'helm)
(require 'helm-lib)

(require 'org)
(require 'org-element)

(defgroup helm-org-named nil
  "Helm interface for named blocks in Org-files."
  :group 'helm)

(defcustom helm-org-named-column-separator "  "
  "Separator for columns in completion listing."
  :type 'string
  :group 'helm-org-named)

(defcustom helm-org-named-directories nil
  "Files to search for named blocks in."
  :type 'list
  :group 'helm-org-named)

(defcustom helm-org-named-files nil
  "Files to search for named blocks in."
  :type 'list
  :group 'helm-org-named)

(defcustom helm-org-named-include-current-file t
  "Non-nil means that the current file will also be used for completion.
When nil, only those specified by `helm-org-named-files' is used."
  :group 'helm-org-named
  :type 'boolean)

(defcustom helm-org-named-full-frame t
  "Non-nil means `helm-org-named-insert' uses the entire window.
When nil, the window will split."
  :group 'helm-org-named
  :type 'boolean)

(defcustom helm-org-named-candidate-limit 99999
  "Number of candidates to display in `helm-org-named-insert'."
  :group 'helm-org-named
  :type 'integer)

;; Copied from `org-ref-label-re'.
(defvar helm-org-named-label-regexps-inner
  (rx (group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%~"))))
  "Regexp for labels.")

;; Copied from `org-ref-ref-label-regexps'.
(defvar helm-org-named-label-regexps
  (list
   (concat ":ID:\\s-+" helm-org-named-label-regexps-inner "\\_>")
   ;; CUSTOM_ID in a heading
   (concat ":CUSTOM_ID:\\s-+" helm-org-named-label-regexps-inner "\\_>")
   ;; #+name
   (concat "^\\s-*#\\+name:\\s-+" helm-org-named-label-regexps-inner "\\_>")
   ;; ;; labels in latex
   ;; (concat "\\\\label{" helm-org-named-label-regexps-inner "}")
   ;; ;; A target, code copied from org-target-regexp and group 1 numbered.
   ;; (let ((border "[^<>\n\r \t]"))
   ;;   (format "<<\\(?1:%s\\|%s[^<>\n\r]*%s\\)>>"
   ;;       border border border))
   ;; ;; A label link
   ;; (concat "label:" helm-org-named-label-regexps-inner "\\_>")
   ;; "\\\\lstset{.*label=\\(?1:.*?\\),.*}"
   )
  "List of regular expressions to labels.
The label should always be in group 1.")

;; recursively find .org files in provided directory
;; modified from an Emacs Lisp Intro example
;; https://github.com/suvayu/.emacs.d/blob/93904ab424fa2d3202637dcbbc9e0fabe5d7dbd9/lisp/nifty.el#L450-L471
(defun helm-org-named--find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
	 (case-fold-search t)	      ; filesystems are case sensitive
	 (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
	 (filext (or filext "org$\\\|org_archive"))
	 (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
	 (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir) ; regular files
	(if (string-match fileregex file-or-dir) ; org files
	    (push file-or-dir org-file-list)))
       ((file-directory-p file-or-dir)
	(dolist (org-file (helm-org-named--find-org-file-recursively file-or-dir filext)
			  org-file-list) ; add files found to result
	  (push org-file org-file-list)))))))

;; Inspired by `org-ref-get-labels'.
;; TODO: Add support for headings with IDs, etc.
(defun helm-org-named--org-element-get-labels ()
  "Get all labels in the current buffer.

Uses `helm-org-named-label-regexps' to search and then
`org-element-context' to extract the context of the match."
  (let ((case-fold-search t)
	    (rx (string-join helm-org-named-label-regexps "\\|"))
	    (labels '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward rx nil t)
        ;; Parse the context of the match using `org-element-context'.
	    (cl-pushnew
         (save-excursion
           (goto-char (match-end 0))
           (org-element-context))
         labels))
       )
    ;; reverse so they are in the order we find them.
    (delete-dups (reverse labels))))

(defun helm-org-named-candidates--find-file (file)
    "Get all labels present in FILE.

This method works by opening FILE in a buffer, followed by
a call to `helm-org-named--org-element-get-labels' to extract the labels.
This is very 'accurate', in the sense that it replicates the enironvemnt in
which the lables are viewed by the reader, but it can be quite slow, e.g.
if has significant setup-files in a org-file.

This is in contrast to `helm-org-named-candidates--temp-buffer-with-insertion'
which uses a temporary buffer and inserts the contents of FILE, thus messing up
potential references to files in the buffer."
  ;; This block will
  ;; 1. Open or select existing buffer with file using `find-file-noselect'.
  ;; 2. Execute `helm-org-named--org-element-get-labels' in the resulting `buffer'.
  ;; 3. If `buffer' wasn't already open, kill it so as to return the to the "state"
  ;;    of emacs that was before we ran this command.
  (let* ((need-to-kill-p (not (find-buffer-visiting file)))
         ;; `find-file-noselect' is like `find-file' but doesn't put the resulting buffer into focus.
         (buffer (find-file-noselect file))
         (labels (with-current-buffer buffer
                   (-map
                    (lambda (el)
                      (let ((link (concat file "::" (plist-get (cadr el) :name))))
                        ;; `helm' will use the first element in the `cons' list to
                        ;; autocomplete, and then we can format the result using `helm-org-named-candidates-formatter'.
                        (cons link el)))
                    (helm-org-named--org-element-get-labels)))))
    (when need-to-kill-p
      (kill-buffer-if-not-modified buffer))
    labels))

(defun helm-org-named-candidates--temp-buffer-with-insertion (file)
  "Get all labels present in FILE.

This method works by inserting the contents of FILE into a temporary buffer,
activates `org-mode', and then calls `helm-org-named--org-element-get-labels'.
See `helm-org-named-candidates--temp-buffer-with-insertion-raw' for a small
discussion of the pros and cons.

Note that setup-files, etc. referenced using relative paths in FILE will
now be incorrect, and hence you might see lots of warnings that it's unable
to open files. In most cases this is of no relevance to the matching of labels."
  ;; This block will
  ;; 1. Open a temporary buffer and insert the contents of `file' into it.
  ;; 2. Enable `org-mode'.
  ;; 3. Disable `font-lock-mode' (hopefully, to improve performance).
  ;; 4. Execute `helm-org-named--org-element-get-labels' in the temporary buffer.
  (with-temp-buffer
    (insert-file-contents file)
    ;; Enable `org-mode' to helm `org-element'.
    ;; HACK: Supress messages because there will be a bunch of "Unable to read file ..."
    ;; due to the temporary buffer not working with from the correct path.
    (let ((inhibit-message t))
        (org-mode))
    ;; Disable `font-lock-mode' for, hopefully, improved performance.
    ;; NOTE: This might be "too late", as `org-mode' has already enabled `font-lock-mode',
    ;; i.e. the performance hit has already been taken.
    (font-lock-mode -1)
    (-map
     (lambda (el)
       (let ((link (concat file "::" (plist-get (cadr el) :name))))
         ;; `helm' will use the first element in the `cons' list to
         ;; autocomplete, and then we can format the result using `helm-org-named-candidates-formatter'.
         (cons link el)))
     (helm-org-named--org-element-get-labels))))

(defun helm-org-named-candidates--temp-buffer-with-insertion-raw (file)
  "Get all labels present in FILE.

This method works by inserting the contents of FILE into a temporary buffer
and then calling `helm-org-named--org-element-get-labels'. As a result, it
is fast but not incredibly accurate in how it interprets the context of the
labels in FILE. This is in contrast to `helm-org-named-candidates--temp-buffer-with-insertion'
which does the same but also activates `org-mode' in the temporary buffer
before parsing.

Both this and `helm-org-named-candidates--temp-buffer-with-insertion' suffers
from the fact that relative paths in FILE will now be incorrect. Usually this
is of no relevance to matching of labels, but you might see lots of warnings
regarding files that cannot be loaded during the process. In contrast
`helm-org-named-candidates--find-file' does not suffer from this issue,
but instead has much worse computational performance."
  ;; This block will
  ;; 1. Open a temporary buffer and insert the contents of `file' into it.
  ;; 2. Enable `org-mode'.
  ;; 3. Disable `font-lock-mode' (hopefully, to improve performance).
  ;; 4. Execute `helm-org-named--org-element-get-labels' in the temporary buffer.
  (with-temp-buffer
    (insert-file-contents file)
    (-map
     (lambda (el)
       (let ((link (concat file "::" (plist-get (cadr el) :name))))
         ;; `helm' will use the first element in the `cons' list to
         ;; autocomplete, and then we can format the result using `helm-org-named-candidates-formatter'.
         (cons link el)))
     (helm-org-named--org-element-get-labels))))

(defun helm-org-named-candidates ()
  "Get all labels present in FILE."
  (let* ((current-filename (with-helm-current-buffer buffer-file-name))
         (default-files (append
                         helm-org-named-files
                         (-mapcat (lambda (dir) (helm-org-named--find-org-file-recursively dir)) helm-org-named-directories)))
         (files (if (and helm-org-named-include-current-file current-filename)
                    (cons current-filename default-files)
                  default-files)))
    (-mapcat #'helm-org-named-candidates--temp-buffer-with-insertion (-uniq files))))

(defun helm-org-named-candidates-formatter--default (&block-type)
  (format ""))

(defun helm-org-named-candidates-formatter--special-block (properties)
  ;; TODO: Use `org-element' or something to extract properties like `:title' from the block itself.
  (if-let ((type (plist-get properties :type)))
      (propertize (format "[%s]" type) 'face 'org-meta-line)
    (helm-org-named-candidates-formatter--default)))

(defun helm-org-named-candidates-formatter--src-block (properties)
  (if-let ((lang (plist-get properties :language)))
      (propertize (format "[src:%s]" lang) 'face 'org-meta-line)
    (propertize "[src]" 'face 'org-meta-line)))

(defun helm-org-named-candidates-formatter--visual (link element-type properties)
  (let* ((path-and-name (split-string link "::"))
         (path (car path-and-name))
         (name (cadr path-and-name)))
    ;; TODO: Figure out how to do the aligning properly rather than using
    ;; fixed numbers like below.
    (concat
     (helm-substring-by-width (propertize name 'face 'helm-ff-file) 70)
     helm-org-named-column-separator
     (concat
      (helm-substring-by-width
       (cl-case element-type
         ('special-block (helm-org-named-candidates-formatter--special-block properties))
         ('src-block (helm-org-named-candidates-formatter--src-block properties))
         (t (helm-org-named-candidates-formatter--default element-type)))
       20)
      helm-org-named-column-separator
      (propertize path 'face 'helm-ff-directory))
     )))

(defun helm-org-named-candidates-formatter (candidates)
  "Format CANDIDATES for display in helm."
  (-map (lambda (candidate)
          (let ((link (car candidate))
                (element-type (cadr candidate))
                (properties (caddr candidate)))
            ;; Also return `candidate' so we can make use of this information later, e.g. when inserting.
            ;; `helm' will only show the `car' of the list.
            (cons
             (helm-org-named-candidates-formatter--visual link element-type properties)
             candidate))
          )
        candidates))

(defun helm-org-named--candidate-description (default-description)
  "Prompts in minibuffer for a description, with the default value being DEFAULT-DESCRIPTION."
  (let ((description (read-from-minibuffer "Description: " default-description)))
    (if (string-empty-p description)
        nil
      description)))

(defun helm-org-named--insert-candidate (candidate)
  "Insert CANDIDATE as a org-link at point."
  (let* ((link (car candidate))
        (path-and-name (split-string link "::"))
        (path (car path-and-name))
        (name (cadr path-and-name)))
    (if-let ((_ (eq major-mode 'org-mode))
               (description (helm-org-named--candidate-description name)))
        (insert
         ;; If the selected `candidate' is from the current file, we just make it a simple link
         ;; rather than a `file:' link.
         ;; AFAIK there's no drawback to doing this + some functionality handles file-links
         ;; differently, e.g. in LaTeX export a `file:' link will be considered an external link
         ;; irregardless of whether it's pointing to the current file or not.
         (if (f-equal-p path (with-helm-current-buffer (buffer-file-name)))
             (format "[[%s][%s]]" name description)
           (format "[[%s:%s][%s]]" "file" link description)))
        (insert (format "%s:%s" "file" link)))))

(defun helm-org-named--open-candidate (candidate)
  "Open CANDIDATE as an org-link."
  (let ((link (car candidate)))
    (org-link-open-from-string (format "%s:%s" "file" link))))

(defconst helm-org-named-source
  (helm-build-sync-source "org-blog named blocks"
    :candidates #'helm-org-named-candidates
    :candidate-transformer #'helm-org-named-candidates-formatter
    :action (helm-make-actions
             "Insert" #'helm-org-named--insert-candidate
             "Open" 'helm-org-named--open-candidate))
  "Source for searching named blocks in Org files.")

;;;###autoload
(defun helm-org-named (arg)
  "Search for named blocks in Org files.

With a `C-u' prefix argument ARG, search for named blocks in the current directory.

With a `C-u C-u' prefix argument ARG, search for named blocks in the current file.
"
  (interactive "p")
  ;; We're using `dynamic' scoping here so this should simply override the
  ;; values of `helm-org-named-files' and `helm-org-named-directories' as desired.
  ;; TODO: Figure out if there's a better way to do this.
  (let ((helm-org-named-files (cond
                               ((= arg 1) helm-org-named-files)
                               ((= arg 4) (list (with-helm-current-buffer buffer-file-name)))
                               ((= arg 16) (list (with-helm-current-buffer buffer-file-name)))))
        (helm-org-named-directories (cond
                                     ((= arg 1) helm-org-named-directories)
                                     ((= arg 4) (list (file-name-directory (with-helm-current-buffer buffer-file-name))))
                                     ((= arg 16) '()))))
    (helm :sources helm-org-named-source
          :buffer "*org-blog*"
          :prompt "Name: "
          :candidate-number-limit helm-org-named-candidate-limit
          :full-frame helm-org-named-full-frame)))

(provide 'helm-org-named)
;;; helm-org-named.el ends here
