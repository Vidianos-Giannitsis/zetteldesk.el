;;; zetteldesk-remark.el --- Org-Remark integration for zetteldesk.el   -*- lexical-binding: t; -*-

;; Author: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; Maintainer: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; URL: https://github.com/Vidianos-Giannitsis/zetteldesk-remark.el
;; Package-Requires: ((zetteldesk "1.0") (org-remark "1.0") (emacs "27.2"))
;; Created: 22nd March 2022
;; License: GPL-3.0
;; Version: 0.2

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines some functions for zetteldesk.el which help with
;; integrating it with org-remark.  Org-remark requires the buffer from
;; which its called to be associated with a file.  However the
;; zetteldesk-scratch buffer is not associated with a file.  Therefore,
;; some special things need to be done to allow for this integration
;; to work.  However, I consider that this is a good implementation of
;; such behaviour.

;;; Code:

(require 'zetteldesk)
(require 'org-remark)

;; -- Helper Functions/Variables --

(defcustom zetteldesk-remark-title nil
  "Title to use in `zetteldesk-remark-highlight-get-title'.

Initialised to nil and given a value when turning on
`zetteldesk-remark-mode' through `zetteldesk-remark-set-title'."
  :type 'string
  :group 'zetteldesk)

(defcustom zetteldesk-remark-notes-file
  (concat org-roam-directory "zetteldesk-margin-notes.org")
  "Default value of `org-remark-notes-file-name' for zetteldesk functions.

When turning on `zetteldesk-remark-mode' this variable's value is
set to the value of `org-remark-notes-file-name' through
`zetteldesk-remark-set-notes-file'.  This is stored as a variable
so you can customise it if you don't like its default value."
  :type 'string
  :group 'zetteldesk)

(defconst zetteldesk-remark-default-notes "marginalia.org"
  "Constant that stores the default value of `org-remark-notes-file-name'.

Zetteldesk-remark, for its own purposes changes the value of that
variable with `zetteldesk-remark-set-notes-file' to make it more
sensible in its own workflow.  However, the original value needs
to be restored after the package is done.  For this reason this
constant stores that value.")

(defun zetteldesk-remark-set-title ()
  "Set the value of `zetteldesk-remark-title' from a minibuffer prompt."
  (setq zetteldesk-remark-title (read-string "Zetteldesk Remark Title: ")))

(defun zetteldesk-remark-set-notes-file ()
  "Helper function to set `org-remark-notes-file-name''s value.

This is the value the zetteldesk-remark functions expect and this
function is run when `zetteldesk-remark-mode' is turned on."
  (setq org-remark-notes-file-name zetteldesk-remark-notes-file))

(defun zetteldesk-remark-reset-notes-file ()
  "Reset `org-remark-notes-file-name' to its default value.

This is a helper function for zetteldesk-remark to reset the
value of that variable after turning off
`zetteldesk-remark-mode'."
  (setq org-remark-notes-file-name zetteldesk-remark-default-notes))

;;;###autoload
(define-minor-mode zetteldesk-remark-mode
  "Toggle the `zetteldesk-remark-mode'.

This mode initialises the value of `zetteldesk-remark-title', an
important variable for using org-remark in buffers not associated
to a file."
  :init-value nil
  :global t
  :group 'zetteldesk
  :lighter " zetteldesk-remark"
  (if zetteldesk-remark-mode
      (progn (zetteldesk-remark-set-notes-file)
	     (zetteldesk-remark-set-title))
    (zetteldesk-remark-reset-notes-file)))

(defun zetteldesk-remark-top-level-heading-title ()
  "Get the title of the top-level org heading.

This is a helper function for `zetteldesk-remark-highlight-mark'.
That function assumes that the heading's title is the title of a
node, which is true only for top level headings in
*zetteldesk-scratch*.  This ensures that the title it takes is
always that of a top level heading."
  (save-excursion
    (while (not (= (car (org-heading-components)) 1))
      (org-previous-visible-heading 1))
    (nth 4 (org-heading-components))))

(defun zetteldesk-remark-highlight-get-title ()
  "Variation of `org-remark-highlight-get-title' for zetteldesk-remark.el.

If the buffer is not associated to a file name, this function
should not return nil, but the value of
`zetteldesk-remark-title'.  This is to be able to use
`zetteldesk-remark-mark' in the *zetteldesk-scratch*."
  (if (buffer-file-name)
      (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE"))))
	  (file-name-sans-extension
	   (file-name-nondirectory (buffer-file-name))))
    zetteldesk-remark-title))

(defun zetteldesk-remark-highlight-save
    (filename beg end props &optional title node-title)
  "Variation of `org-remark-highlight-save' for zetteldesk-remark.el.

It has a bonus optional parameter NODE-TITLE which acts as the
name of the second level heading which will store the item and to
not completely lose the items that were marked during the call to
`zetteldesk-remark-mark' they are placed in a quote block right
after the second level heading.  Besides that it acts the same as
`org-remark-highlight-save'.  However,
`zetteldesk-remark-highlight-mark' the function that calls this,
gives it quite different arguments than
`org-remark-highlight-mark' would.  For more details refer to its
docstring.

FILENAME, BEG, END, PROPS and TITLE are the same as in
`org-remark-highlight-save'."
  (let* ((filename (org-remark-source-get-file-name filename))
	 (id (plist-get props 'org-remark-id))
	 (text (org-with-wide-buffer (buffer-substring-no-properties beg end)))
	 (notes-buf (find-file-noselect (org-remark-notes-get-file-name)))
	 (main-buf (current-buffer))
	 (line-num (org-current-line beg))
	 (orgid (org-remark-highlight-get-org-id beg)))
    (with-current-buffer notes-buf
      ;;`org-with-wide-buffer is a macro that should work for non-Org file'
      (org-with-wide-buffer
       (let ((file-headline (or (org-find-property
				 org-remark-prop-source-file filename)
				(progn
				  ;; If file-headline does not exist,
				  ;; create one at the bottom
				  (goto-char (point-max))
				  ;; Ensure to be in the beginning of
				  ;; line to add a new headline
				  (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
				  (insert "* " title "\n")
				  (org-set-property org-remark-prop-source-file filename)
				  (org-up-heading-safe) (point))))
	     (id-headline (org-find-property org-remark-prop-id id)))
	 ;; Add org-remark-link with updated line-num as a property
	 (plist-put props "org-remark-link" (concat
					     "[[file:"
					     filename
					     (when line-num (format "::%d" line-num))
					     "]]"))
	 (if id-headline
	     (progn
	       (goto-char id-headline)
	       ;; Update the existing headline and position properties
	       ;; Don't update the headline text when it already exists
	       ;; Let the user decide how to manage the headlines
	       ;; (org-edit-headline text)
	       ;; FIXME update the line-num in a normal link if any
	       (org-remark-notes-set-properties beg end props))
	   ;; No headline with the marginal notes ID property. Create a new one
	   ;; at the end of the file's entry
	   (goto-char file-headline)
	   (org-narrow-to-subtree)
	   (goto-char (point-max))
	   ;; Ensure to be in the beginning of line to add a new headline
	   (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
	   ;; Create a headline
	   ;; Add a properties
	   (insert "** " node-title "\n")
	   (insert "#+begin_quote\n" text "\n" "#+end_quote\n")
	   (org-remark-notes-set-properties beg end props)
	   (when (and orgid org-remark-use-org-id)
	       (insert "[[id:" orgid "]" "[" title "]]")))))
      (cond
       ;; fix GH issue #19
       ;; Temporarily remove `org-remark-save' from the `after-save-hook'
       ;; When the marginal notes buffer is the main buffer
       ((eq notes-buf main-buf)
	(remove-hook 'after-save-hook #'org-remark-save t)
	(save-buffer)
	(add-hook 'after-save-hook #'org-remark-save nil t))
       ;; When marginal notes buffer is separate from the main buffer, save the
       ;; notes buffer
       ((buffer-modified-p)
	(save-buffer)))
      t)))

;; -- The main functions --

(defun zetteldesk-remark-highlight-mark
    (beg end &optional id mode label face properties)
  "Variation of `org-remark-highlight-mark' for zetteldesk-remark.el.

The main difference is that the zetteldesk alternative to some of
the org-remark functions are run.  This
`zetteldesk-remark-highlight-save' instead of
`org-remark-highlight-save' and
`zetteldesk-remark-highlight-get-title' instead of
`org-remark-highlight-get-title'.  Also, when ran, this function
activates `zetteldesk-remark-mode' which runs some useful
initialization functions that other functions of the package
expect.

In running the function, filename is no longer taken from the
function `buffer-file-name' but from the node whose title is the
current heading's title, the title is a `concat' of the string
*zetteldesk-scratch* and the value of
`zetteldesk-remark-highlight-get-title'.  Lastly, this gives
`zetteldesk-remark-highlight-save''s final argument which is the
title of the node that is associated with this section.

This function is meant to be run in the *zetteldesk-scratch*
buffer so evalutating it elsewhere might lead to errors.  For
regular buffers associated to a file you should just use the
default `org-remark-highlight-mark' as this version relies
heavlily on the structure of *zetteldesk-scratch*.

Arguments BEG, END, ID, MODE, LABEL, FACE and PROPERTIES are all
identical to those in `org-remark-highlight-mark'."
  ;; Ensure to turn on the local minor mode
  (unless org-remark-mode (org-remark-mode +1)
	  zetteldesk-remark-mode (zetteldesk-remark-mode +1))
  ;; When highlights are toggled hidden, only the new one gets highlighted in
  ;; the wrong toggle state.
  (when org-remark-highlights-hidden (org-remark-highlights-show))
  ;; Add highlight to the text
  (org-with-wide-buffer
   (let ((ov (make-overlay beg end nil :front-advance))
	 ;; UUID is too long; does not have to be the full length
	 (id (if id id (substring (org-id-uuid) 0 8))))
     (overlay-put ov 'face (if face face 'org-remark-highlighter))
     (while properties
       (let ((prop (pop properties))
	     (val (pop properties)))
	 (overlay-put ov prop val)))
     (when label (overlay-put ov 'org-remark-label label))
     (overlay-put ov 'org-remark-id id)
     ;; Keep track of the overlay in a local variable. It's a list that is
     ;; guaranteed to contain only org-remark overlays as opposed to the one
     ;; returned by `overlay-lists' that lists any overlays.
     (push ov org-remark-highlights)
     ;; for mode, nil and :change result in saving the highlight.  :load
     ;; bypasses save.
     (unless (eq mode :load)
       (let* ((node-title (zetteldesk-remark-top-level-heading-title))
	      (node (org-roam-node-from-title-or-alias node-title))
	      (filename (org-roam-node-file node)))
	 (if filename
	     (zetteldesk-remark-highlight-save filename
					       beg end
					       (overlay-properties ov)
					       (concat "*zetteldesk-scratch* "
						       (zetteldesk-remark-highlight-get-title))
					       node-title)
	   (message "org-remark: Highlights not saved; buffer is not visiting a file"))))))
  (deactivate-mark)
  (org-remark-highlights-housekeep)
  (org-remark-highlights-sort))

(defun zetteldesk-remark-mark (beg end &optional id mode)
  "Variation of `org-remark-mark' for zetteldesk-remark.el.

The only difference is that `zetteldesk-remark-highlight-mark' is
run instead of `org-remark-highlight-mark'.  For details on what
the differences are, refer to its docstring, while for details
on the arguments BEG, END, ID and MODE refer to
`org-remark-mark'."
  (interactive (org-remark-region-or-word))
  ;; FIXME
  ;; Adding "nil" is different to removing a prop
  ;; This will do for now
  (zetteldesk-remark-highlight-mark beg end id mode
				    nil nil
				    (list "org-remark-label" "nil")))

(defun zetteldesk-remark-switch-to-margin-notes ()
  "Helper function to go to the zetteldesk-margin-notes file.

If `org-remark-mark' is called through its wrapper function
`zetteldesk-remark-mark', it sets `org-remark-notes-file-name' to
a specific file, which is meant to be used with all margin notes
coming from zetteldesk-scratch.  This function switches to that
file."
  (interactive)
  (pop-to-buffer (find-file zetteldesk-remark-notes-file)))

(provide 'zetteldesk-remark)
;;; zetteldesk-remark.el ends here
