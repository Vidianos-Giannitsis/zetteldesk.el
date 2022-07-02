;;; zetteldesk.el --- A revision and outlining tool for org-roam  -*- lexical-binding: t; -*-

;; Author: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; Maintainer: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; URL: https://github.com/Vidianos-Giannitsis/zetteldesk.el
;; Package-Requires: ((emacs "27.1") (org-roam "2.0"))
;; Tested on Emacs 27.2 and org-roam v2.2
;; Created: 6th February 2022
;; License: GPL-3.0
;; Version: 1.0.1

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

;; This package provides various filter functions to "traverse" your
;; org-roam database more smoothly.  I use it for doing revision in
;; lessons and I personally find it highly efficient for this
;; purpose.  Its also good to just sort out your thoughts.  In general,
;; org-roam lacks a good framework for filtering your nodes although
;; it has all the backend for doing that.

;;; Code:

;; -- DEPENDENCIES --
;; These are some functions that are needed for the package to work as
;; it should, but that are not specific to the package

(require 'org-roam)

(cl-defmethod org-roam-node-buffer ((node org-roam-node))
  "Access slot \"buffer\" of org-roam-node struct CL-X.
NODE is an org-roam-node"
  (let ((buffer (get-file-buffer (org-roam-node-file node))))
    buffer))

(cl-defmethod org-roam-node-backlinkscount-number ((node org-roam-node))
    "Access slot \"backlinks\" of org-roam-node struct CL-X.

This is identical to `org-roam-node-backlinkscount' with the
    difference that it returns a number instead of a fromatted
    string. This is to be used in
    `zetteldesk-roam-node-sort-by-backlinks'. NODE is an org-roam-node."
    (let* ((count (caar (org-roam-db-query
			 [:select (funcall count source)
				  :from links
				  :where (= dest $s1)
				  :and (= type "id")]
			 (org-roam-node-id node)))))
      count))

(defun zetteldesk-roam-node-poi-or-moc-p (NODE)
  "Check if NODE has the tag POI or the tag MOC.  Return t if it does."
  (or (string-equal (car (org-roam-node-tags NODE)) "POI")
      (string-equal (car (org-roam-node-tags NODE)) "MOC")))

(defun zetteldesk-roam-backlink-query ()
  "Simple org-roam query function.
Stores the IDs of all the files that link to the node at point.
This is a modified part of the `org-roam-backlinks-get' function
keeping only the part necessary for `org-roam-backlink-files' to
work as this is a complimentary function to that"
  (org-roam-db-query
   [:select [source dest]
	    :from links
	    :where (= dest $s1)
	    :and (= type "id")]
   (org-roam-node-id (org-roam-node-at-point))))

(defun zetteldesk-roam-backlink-query* (NODE)
  "Run `zetteldesk-roam-backlink-query' with NODE.

Instead of returning a list of the backlinks of
`org-roam-node-at-point', find the backlinks of NODE.  This is
handy in cases where NODE is read through `org-roam-node-read'
and doesn't have to be the `current-buffer'"
  (org-roam-db-query
	[:select [source dest]
		 :from links
		 :where (= dest $s1)
		 :and (= type "id")]
	(org-roam-node-id NODE)))

(defun zetteldesk-roam-node-sort-by-backlinks (completion-a completion-b)
  "Sorting function for org-roam that sort the list of nodes.
Sorting is done accordint to the number of backlinks.  This is
the sorting function in `org-roam-node-find-by-backlinks' and
`zetteldesk-node-insert-sort-backlinks'.  Arguments COMPLETION-A
and COMPLETION-B are same as in
`org-roam-node-read-sort-by-file-atime'"
  (let ((node-a (cdr completion-a))
	(node-b (cdr completion-b)))
    (>= (org-roam-node-backlinkscount-number node-a)
	(org-roam-node-backlinkscount-number node-b))))

;; -- PREDICATE FUNCTIONS --
;; This section contains the predicate functions the package uses. The
;; core of the package is that it provides well filtered completion
;; menus as chosen by the user, so obviously predicate functions are
;; core to the package

(defcustom zetteldesk-desktop
  "default"
  "Buffer local variable.
Checks if the current buffer is part of the zetteldesk.  A buffer
is part of the zetteldesk only if the value of this variable is
not its default value in that buffer."
  :type 'string
  :group 'zetteldesk
  :local t)

(defconst zetteldesk-desktop-default
  "default"
  "Default value of `zetteldesk-desktop'.

This stores the default value through a `defconst' so it can't be
modified to do a comparison between it and the current value of
`zetteldesk-desktop'.")

(defun zetteldesk-p (BUFFER)
  "Check if BUFFER is part of the current `zetteldesk-desktop'."
  (not (equal zetteldesk-desktop-default
	      (buffer-local-value 'zetteldesk-desktop (cdr BUFFER)))))

(defun zetteldesk-buffer-p (BUFFER)
  "Check if BUFFER is part of the current `zetteldesk-desktop'.
Then check if BUFFER is not associated to an org-roam file.

Org-roam file buffers are better viewed with `org-roam-node-file'
so this function filters down the database to non org-roam
zetteldesk buffers.  This is what is used to create the filter
function `zetteldesk-switch-to-buffer'"
  (and (zetteldesk-p BUFFER) (not (org-roam-buffer-p (cdr BUFFER)))))

(defun zetteldesk-node-p (NODE)
  "Check if NODE is associated with an open buffer.
If it is, check if that buffer is part of the current
`zetteldesk-desktop'.  If it isn't, return nil.

This function is used as a filter function to create
`zetteldesk-node-find' which is a filtered view of
`org-roam-node-find'"
  (if (org-roam-node-buffer NODE)
      (not (equal zetteldesk-desktop-default
		  (buffer-local-value 'zetteldesk-desktop (org-roam-node-buffer NODE))))
    nil))

(defmacro zetteldesk-mode-buffer-p (BUFFER MODE)
  "Check if BUFFER is part of the `zetteldesk-desktop' and in `major-mode' MODE.

This macro is meant to be used to write filter functions to be
passed to `read-buffer' variants such as
`zetteldesk-org-buffer-p' which is used in
`zetteldesk-insert-org-file-contents'.  BUFFER is in the form
required for `read-buffer' while MODE should be a symbol such as
'org-mode."
  `(and (zetteldesk-buffer-p ,BUFFER)
	(equal (buffer-local-value 'major-mode (cdr ,BUFFER)) ,MODE)))

(defun zetteldesk-org-buffer-p (BUFFER)
  "Check if BUFFER is part of the current `zetteldesk-desktop'.
Then check if the file is an org file but not one that belongs to
org-roam.

This is used as the filter function for
`zetteldesk-insert-org-file-contents' which prompts for an org
file, but as `zetteldesk-insert-node-contents' is a superior
version for org-roam nodes, that function should not prompts for
those files"
  (zetteldesk-mode-buffer-p BUFFER 'org-mode))

(defun zetteldesk-pdf-p (BUFFER)
  "Check if BUFFER is part of the current `zetteldesk-desktop' and also a pdf file."
  (zetteldesk-mode-buffer-p BUFFER 'pdf-view-mode))

;; -- ADD/REMOVE THINGS IN THE ZETTELDESK --
;; To get a system where the user can get multiple filtered views of
;; the nodes/buffers of their choice, obviously you need functions
;; that allow the user to add them to something. And then, you also
;; need ways to remove things, because people make mistakes.

(defmacro zetteldesk--add-buffer (BUFFER)
  "Add BUFFER to the current `zetteldesk-desktop'.

This is a low-level macro used in all zetteldesk-add
functions.  Given BUFFER it creates the code required to add the
buffer to the zetteldesk.  For example all
`zetteldesk-add-to-desktop' is, is an interactive call to this
macro.  Other functions need more stuff, but deep-down they all
use this macro."
  `(with-current-buffer ,BUFFER
     (setq-local zetteldesk-desktop "foo")))

(defun zetteldesk-add-to-desktop (BUFFER)
  "Add BUFFER to the current `zetteldesk-desktop'."
  (interactive "b")
  (zetteldesk--add-buffer BUFFER))

(defun zetteldesk-add-node-to-desktop (NODE)
  "Add NODE to the `zetteldesk-desktop'.
If there isn't a buffer associated to it, create it.  NODE is an
org-roam node read through `org-roam-node-read'"
  (interactive (list (org-roam-node-read)))
  (let ((buffer (org-roam-node-buffer NODE))
	 (file (org-roam-node-file NODE))
	 (org-startup-with-latex-preview nil))
    (if buffer
	(zetteldesk--add-buffer buffer)
      (zetteldesk--add-buffer (find-file-noselect file)))))

(defun zetteldesk-add-poi-or-moc-backlink-to-desktop ()
  "Prompt the user to select an org-roam node that has a specific tag.
Looks for the POI or MOC tag (filtering done with
`zetteldesk-roam-node-poi-or-moc-p') and collects its ID and backlinks.
Then, prompt the user to select one of its backlinks and add that
to the zetteldesk."
  (interactive)
  (let* ((source (org-roam-node-read nil #'zetteldesk-roam-node-poi-or-moc-p))
	 (source-id (org-roam-node-id source))
	 (backlinks (zetteldesk-roam-backlink-query* source)))
    (zetteldesk-add-node-to-desktop
     (org-roam-node-read nil (lambda (NODE)
			       (let* ((id (org-roam-node-id NODE))
				      (id-list (list id source-id)))
				 (member id-list backlinks)))))))

(defun zetteldesk-add-backlinks-to-desktop ()
  "Add the current buffer and all its backlinks to the `zetteldesk-desktop'.

This function queries the database for all the nodes that link to
the current node with the `zetteldesk-roam-backlink-query' function and
then recursively checks if there is an open buffer associated
with them, and if so adds it to the `zetteldesk-desktop'"
  (interactive)
  (setq-local zetteldesk-desktop "foo")
  (let ((backlinks (length (zetteldesk-roam-backlink-query)))
	(org-startup-with-latex-preview nil))
    (dotimes (number backlinks)
      (let* ((id (car (nth number (zetteldesk-roam-backlink-query))))
	      (node (org-roam-node-from-id id))
	      (buffer (org-roam-node-buffer node))
	      (file (org-roam-node-file node)))
	(if buffer
	    (zetteldesk--add-buffer buffer)
	  (zetteldesk--add-buffer (find-file-noselect file)))))))

(defmacro zetteldesk--remove-buffer (BUFFER)
  "Remove BUFFER from the current `zetteldesk-desktop'.

This is a low-level macro used in all zetteldesk-remove
functions.  This function is identical in logic to
`zetteldesk--add-buffer', however it is for removing thingss
instead of adding."
  `(with-current-buffer ,BUFFER
     (kill-local-variable 'zetteldesk-desktop)))

(defun zetteldesk-remove-from-desktop (BUFFER)
  "Remove BUFFER from the current `zetteldesk-desktop'."
  (interactive "b")
  (zetteldesk--remove-buffer BUFFER))

(defun zetteldesk-remove-node-from-desktop (NODE)
  "Remove NODE from the `zetteldesk-desktop'.
NODE is an org-roam node
and is read through `org-roam-node-read'"
  (interactive (list (org-roam-node-read nil #'zetteldesk-node-p)))
  (let ((buffer (org-roam-node-buffer NODE)))
    (zetteldesk--remove-buffer buffer)))

(defun zetteldesk-remove-backlinks-from-desktop ()
  "Remove from the `zetteldesk-desktop', the current buffer and its backlinks.

This function is essentially a carbon copy of
`zetteldesk-add-backlinks-to-desktop' but instead of adding the
buffer to the desktop it removes it."
  (interactive)
  (kill-local-variable 'zetteldesk-desktop)
  (let ((backlinks (length (zetteldesk-roam-backlink-query))))
    (dotimes (number backlinks)
      (let* ((id (car (nth number (zetteldesk-roam-backlink-query))))
	      (node (org-roam-node-from-id id))
	      (buffer (org-roam-node-buffer node)))
	(when buffer
	  (zetteldesk--remove-buffer buffer))))))

;; -- FILTER FUNCTIONS --
;; This section is about defining all the functions that show you the
;; filtered results of all your nodes/buffers

(defun zetteldesk-switch-to-buffer ()
  "Execute a filtered `switch-to-buffer'.
The filter is done using `zetteldesk-buffer-p' to show only
buffers that are part of the current `zetteldesk-desktop' and not
`org-roam-node's."
  (interactive)
  (switch-to-buffer (read-buffer "Zetteldesk Buffers: " nil nil #'zetteldesk-buffer-p)))

(defun zetteldesk-node-find ()
  "Execute a filtered `org-roam-node-find'.
The filter is done using `zetteldesk-node-p' to show only nodes that are
part of the current `zetteldesk-desktop'"
  (interactive)
  (org-roam-node-find nil nil #'zetteldesk-node-p))

(defun zetteldesk-node-insert ()
  "Execute a filtered `org-roam-node-insert'.
The filter is done using `zetteldesk-node-p' to show only nodes that are
part of the current `zetteldesk-desktop'"
  (interactive)
  (org-roam-node-insert #'zetteldesk-node-p))

(defun zetteldesk-node-insert-sort-backlinks ()
  "Select a node that is part of the zetteldesk.
The function used is `org-roam-node-read' in a UI sorted by the
  number of backlinks.  Insert a link in the current buffer to
  the selected node.

This function essentially has the core functionality of
`org-roam-node-insert', but it uses `org-roam-node-read' instead
as only that can take a sort-function.  Some files may be
important to their topic, but not MOCs or POIs, so this function
acts essentially as a complimentary function to
`zetteldesk-node-insert-if-poi-or-moc' to check if that one
missed something you want to include."
  (interactive)
  (let* ((node (org-roam-node-read nil #'zetteldesk-node-p #'zetteldesk-roam-node-sort-by-backlinks))
	 (id (org-roam-node-id node))
	 (description (org-roam-node-formatted node)))
    (insert (org-link-make-string
	     (concat "id:" id)
	     description))))

;; -- *ZETTELDESK-SCRATCH* --
;; This is the section where it all comes together. The
;; zetteldesk-scratch buffer is a special buffer defined here on which
;; you drop all your stuff. Its what molds the whole workflow together

(defcustom zetteldesk-kb-map (make-sparse-keymap)
  "This variable is the keymap for `zetteldesk-mode'."
  :type 'keymap
  :group 'zetteldesk)

(defcustom zetteldesk-insert-scratch-or-current-buffer t
  "Customization variable.
Decides whethere zetteldesk-insert functions will insert to the
*zetteldesk-scratch* buffer or the current buffer.  Default value
is t which makes those functions insert to the scratch.  Setting
it to nil will make those functions insert to the current buffer,
for whichever usecase you might want"
  :type 'string
  :group 'zetteldesk)

(defun zetteldesk--create-scratch-buffer ()
  "Create the zetteldesk-scratch buffer and put it in `org-mode'."
  (let ((buffer (generate-new-buffer "*zetteldesk-scratch*"))
	(org-startup-with-latex-preview nil))
    (with-current-buffer buffer
      (org-mode))))

;;;###autoload
(define-minor-mode zetteldesk-mode
  "Toggles the global `zetteldesk-mode'.

When turned on, this mode initialises the *zetteldesk-scratch*
buffer, a useful part of the whole zetteldesk workflow."
  :init-value nil
  :global t
  :group 'zetteldesk
  :keymap zetteldesk-kb-map
  :lighter " zetteldesk"
  (when zetteldesk-mode
    (zetteldesk--create-scratch-buffer)))

(defmacro zetteldesk-insert-location ()
  "Find the location the zetteldesk-insert functions should insert to.

The decision is made depending on the variable
`zetteldesk-insert-scratch-or-current-buffer'.  Check its
docstring for more info.  This is used in all zetteldesk-insert
functions to decide if the insertion should happen in
*zetteldesk-scratch or the current buffer."
  `(if zetteldesk-insert-scratch-or-current-buffer
		  "*zetteldesk-scratch*"
		(current-buffer)))

(defmacro zetteldesk-insert-switch-to-scratch (arg)
  "Switch to the *zetteldesk-scratch* if ARG is the `\\[universal-argument]'.

All the zetteldesk-insert functions have a similar logic of
switching to the *zetteldesk-scratch* buffer in a split if given
a `\\[universal-argument]'.  To avoid repetition, this macro
expands to the needed code."
  `(when (equal ,arg '(4))
     (switch-to-buffer-other-window "*zetteldesk-scratch*")))

(defmacro zetteldesk--replace-title ()
  "Replace \"#+title: \" with \"* \".

A lot of the zetteldesk-insert functions need this functionality
so I implemented it as a simple macro."
  `(while (search-forward "#+title: " nil t)
     (replace-match "* " nil t)))

(defun zetteldesk-switch-to-scratch-buffer (&optional arg)
  "Open the zetteldesk-scratch buffer in a split with the current buffer.

Optionally, if given optional argument ARG, which needs to be the
`\\[universal-argument]' switch to the *zetteldesk-scratch*
buffer without issuing a split."
  (interactive "P")
  (if (equal arg '(4))
      (switch-to-buffer "*zetteldesk-scratch*")
    (switch-to-buffer-other-window "*zetteldesk-scratch*")))

(defun zetteldesk-node-insert-if-poi-or-moc ()
  "Filter `org-roam-node-list' to show zetteldesk-nodes only.
Then filter that to only those that have the POI or MOC tag with
`zetteldesk-node-p' and `zetteldesk-roam-node-poi-or-moc-p'.  Then
insert a link to every one of those nodes and seperate them with
commas"
  (interactive)
  (let* ((init_list (org-roam-node-list))
	 (zetteldesk_nodes (cl-remove-if-not #'zetteldesk-node-p init_list))
	 (nodes_poi (cl-remove-if-not #'zetteldesk-roam-node-poi-or-moc-p zetteldesk_nodes)))
    (while nodes_poi
      (let* ((node (car nodes_poi))
	     (description (org-roam-node-formatted node)))
	(insert (org-link-make-string
		 (concat "id:" (org-roam-node-id node))
		 description))
	(insert ", "))
      (setq nodes_poi (cdr nodes_poi)))))

(defun zetteldesk-insert-node-contents (&optional arg)
  "Select a node that is part of the current `zetteldesk-desktop'.
Add a link to it at point and then insert its contents to the
bottom of the *zetteldesk-scratch* buffer after inserting a
newline there.  Remove the first 67 characters which is the
properties section if it only contains the ID of the node as its
unneeded and change the string #+title to a top level heading as
its more practical when inserting the contents of multiple files.

If given the optional argument ARG, which needs to be the
`\\[universal-argument]' also switch to the *zetteldesk-scratch*
buffer in a split."
  (interactive "P")
  (let* ((node (org-roam-node-read nil #'zetteldesk-node-p))
	 (file (org-roam-node-file node))
	 (description (org-roam-node-formatted node))
	 (location (zetteldesk-insert-location)))
    (insert (org-link-make-string
	     (concat "id:" (org-roam-node-id node))
	     description))
    (with-current-buffer location
      (goto-char (point-max))
      (newline)
      (insert-file-contents file nil 67)
      (zetteldesk--replace-title)))
  (zetteldesk-insert-switch-to-scratch arg))

(defun zetteldesk-insert-node-contents-without-link ()
  "\"Sister function\" of `zetteldesk-insert-node-contents'.
Finds a node that is part of the `zetteldesk-desktop' and inserts its
contents to the bottom of the zetteldesk-scratch buffer. This
function differentiates itself, by the fact that it doesn't
insert an ID link to the node in the current buffer and by the
fact that it switches to the scratch buffer in a split without
needing a `\\[universal-argument]'.

For me, it makes sense a lot of the time to insert a link to the
node inthe current buffer, especially if its an outlining
buffer. But sometimes its not handy, and so, I just made this
second iteration to fix that issue."
  (interactive)
  (let* ((node (org-roam-node-read nil #'zetteldesk-node-p))
	 (location (zetteldesk-insert-location))
	 (file (org-roam-node-file node)))
    (with-current-buffer location
      (goto-char (point-max))
      (newline)
      (insert-file-contents file nil 67)
      (zetteldesk--replace-title)))
  (switch-to-buffer-other-window "*zetteldesk-scratch*"))

(defun zetteldesk-insert-org-file-contents (&optional arg)
  "Select an org buffer that is in the `zetteldesk-desktop'.
Insert its contents to the *zetteldesk-scratch* buffer, make its
 title a top level heading and demote all of its headings by one
 level (since the title now acts as a top level heading).  Note
 that org-roam nodes are not shown

Optional argument ARG, if given needs to be a
`\\[universal-argument]' and it switches to the *zetteldesk-scratch*
buffer in a split"
  (interactive "P")
  (let* ((buffer (set-buffer (read-buffer "Zetteldesk Buffers: " nil nil #'zetteldesk-org-buffer-p)))
	 (location (zetteldesk-insert-location))
	 (file (buffer-file-name buffer)))
    (set-buffer location)
    (goto-char (point-max))
    (save-excursion
      (newline)
      (insert-file-contents file))
    (save-excursion
      (while (not (org-next-visible-heading 1))
	(org-metaright)))
    (zetteldesk--replace-title))
  (zetteldesk-insert-switch-to-scratch arg))

(defun zetteldesk-insert-link-to-pdf (&optional arg)
  "Select a pdf file that is part of the `zetteldesk-desktop'.
Prompt for a page in that pdf (defaults to page 1 if you don't
care about the page).

Then, in the zetteldesk-scratch buffer, insert at `point-max' a
newline and then a new heading with its name consisting of the
string \"Supportive Material - \" then the pdfs name, without the
file structure or the extension and then the string
\"(PDF)\".  Then, insert a newline, the string \"Link to PDF: \"
and then a link to the chosen pdf, in the correct page, with the
description being the pdfs name without the file structure or the
extension.  Note that `org-pdftools-setup-link' needs to be run
for pdf links to work (which this uses).

Optionally, if given optional argument ARG which is the
`\\[universal-argument]' save the highlighted region in a
variable and insert it after the heading but before the pdf link.
This functionality serves the purpose of adding a \"description\"
sort of thing to the pdf. Typically, when citing a pdf as
supplementary info to an argument, there is something specific
you want to take from the pdf. Therefore, this optional addition,
adds that to the scratch buffer so you remember why it was
useful."
  (interactive "P")
  (let* ((pdf-buffer (set-buffer (read-buffer "Zetteldesk Pdfs: " nil nil #'zetteldesk-pdf-p)))
	 (file (buffer-file-name pdf-buffer))
	 (location (zetteldesk-insert-location))
	 (page (read-from-minibuffer "Page: " "1"))
	 (description (file-name-nondirectory (file-name-sans-extension file))))
    (with-current-buffer location
      (goto-char (point-max))
      (newline)
      (org-insert-heading)
      (insert "Supportive Material - " description " (PDF)")
      (newline)
      (when (equal arg '(4))
	(let ((contents (buffer-substring (mark) (point))))
	  (insert contents)
	  (newline)))
      (insert "Link to PDF: "
	      (org-link-make-string
	       (concat "pdf:" file "::" page)
	       description)))))

(defcustom zetteldesk-saved-states '()
  "List of lists of buffers storing saved states of `zetteldesk-desktop'.

Each item in the list is a list of buffers. The function
`zetteldesk-save-state' inserts the code to add the list of
buffers currently in the `zetteldesk-desktop' to this list."
  :type 'list
  :group 'zetteldesk)

(defcustom zetteldesk-saved-state-file
  (concat user-emacs-directory "libs/zetteldesk-saves.el")
  "Location in which zetteldesk.el saves its desktops.

Saving is done with `zetteldesk-save-state' and is stored in this
file so it can be restored in later sessions with
`zetteldesk-restore-desktop'."
  :type 'string
  :group 'zetteldesk)

(defun zetteldesk-p* (BUFFER)
  "Check if BUFFER is part of the current `zetteldesk-desktop'."
  (not (equal zetteldesk-desktop-default
	      (buffer-local-value 'zetteldesk-desktop BUFFER))))

(defun zetteldesk-buffer-list ()
  "Make a list of all buffers that are part of the current `zetteldesk-desktop'.

Also prompt for the identifier of the desktop so it can be
recognized. This function is not meant to be used directly but be
passed to `zetteldesk-save-state'. The identifier is to be used
when restoring the desktop with `zetteldesk-restore-desktop'."
  (let ((identifier (read-string "Identifier for desktop: ")))
    (cons identifier
	  (cl-loop for buffer in (buffer-list)
		   if (zetteldesk-p* buffer)
		   collect (buffer-file-name buffer)))))

(defun zetteldesk-save-state ()
  "Save the state of the current `zetteldesk-desktop'.

This function uses `zetteldesk-buffer-list' as its main helper
function. It collects a list of files who are part of the curent
`zetteldesk-desktop' and adds an identifier to them so the state
can be later restored. What it does is insert an `add-to-list'
statement to the file that `zetteldesk-saved-state-file' points
to, which adds the contents of the list returned by
`zetteldesk-buffer-list' to `zetteldesk-saved-states'.

If `zetteldesk-saved-state-file' is required in your init.el,
zetteldesk will remember your saved states and it will be able to
restore them with `zetteldesk-restore-desktop'."
  (interactive)
  (with-current-buffer (find-file-noselect zetteldesk-saved-state-file)
    (goto-char (point-max))
    (previous-line)
    (insert (format "%S" `(add-to-list 'zetteldesk-saved-states ',(zetteldesk-buffer-list))))
    (newline)))

(defun zetteldesk-add-file-to-desktop (FILE)
  "Add FILE to the current `zetteldesk-desktop'.

If FILE is not associated to a buffer, read it in a buffer and
add the resulting buffer to the `zetteldesk-desktop'. This
function is not meant to be used interactively as it would be
impractical. It is primarily for use in
`zetteldesk-restore-desktop'."
  (let ((buffer (get-file-buffer FILE))
	(org-startup-with-latex-preview nil))
    (if buffer
	(zetteldesk--add-buffer buffer)
      (zetteldesk--add-buffer (find-file-noselect FILE)))))

(defun zetteldesk-restore-desktop ()
  "Restore a saved state of the `zetteldesk-desktop'.

The state must be saved to `zetteldesk-saved-states' using
`zetteldesk-save-state' and the user is prompted to select a
state using its identifier."
  (interactive)
  (mapcar #'zetteldesk-add-file-to-desktop
	  (cdr (assoc (completing-read "Save-State: " zetteldesk-saved-states)
		      zetteldesk-saved-states))))

(provide 'zetteldesk)
;;; zetteldesk.el ends here
