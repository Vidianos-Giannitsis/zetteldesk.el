;;; zetteldesk-info.el --- A zetteldesk extension for interacting with the info program   -*- lexical-binding: t; -*-

;; Author: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; Maintainer: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; URL: https://github.com/Vidianos-Giannitsis/zetteldesk-info.el
;; Package-Requires: ((zetteldesk "0.4") (emacs "27.1"))
;; Created: 6th April 2022
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

;; This package provides some optional improvements to zetteldesk.el.
;; Specifically it introduces mechanisms for handling nodes from the
;; Info program built in to Emacs, which isn't trivial as its a
;; persistent buffer and so the typical predicates used are not
;; available for use here

;;; Code:

(require 'zetteldesk)

;; -- Info Nodes --
(defcustom zetteldesk-info-nodes '()
	"List of info nodes that are part of the zetteldesk.
Initialised as an empty list"
	:type 'list
	:group 'zetteldesk)

(defun zetteldesk-info-add-info-node-to-desktop ()
	"Find the current info-node.
Then add its name to the list of the variable
`zetteldesk-info-nodes'"
	(interactive)
	(add-to-list 'zetteldesk-info-nodes (Info-copy-current-node-name)))

(defun zetteldesk-info-remove-info-node-from-desktop ()
	"Remove an info-node from the `zetteldesk-desktop'.
The node is selected through a `completing-read' menu of
`zetteldesk-info-nodes'"
	(interactive)
	(setq zetteldesk-info-nodes (remove
				     (completing-read "Info Nodes: " zetteldesk-info-nodes)
				     zetteldesk-info-nodes)))

(defun zetteldesk-info-goto-node ()
	"Zetteldesk filter function for `Info-goto-node'.

Prompts the user to select a node from the list
`zetteldesk-info-nodes' and jumps to that node"
	(interactive)
	(Info-goto-node (completing-read "Nodes: " zetteldesk-info-nodes)))

(defun zetteldesk-info-insert-contents (&optional arg)
	"Select an info node that is part of the current `zetteldesk-desktop'.
Uses a `completing-read' prompt for the selection.

Then, in the *zetteldesk-scratch* buffer, go to the end of the
buffer, insert a newline and a heading of the form \"Supportive
Material - \" the node's name \"(Info)\" akin to what is done in
`zetteldesk-insert-link-to-pdf'.  Then, insert the contents of the
chosen info node, removing the first 2 lines which have the
contextual links of the buffer, as they are not functional
outside of the info buffer.  Also insert a link with the title
\"See this node in its context\" which opens the node inside the
info program. Finally, restore the buffer from which this
function was called. Ideally, this wouldn't require a
switch-to-buffer statement, but the function `Info-goto-node'
used for this function switches the visible buffer to the info
node and I couldn't find an alternative that only makes it
current for editing operations, but doesn't change the visible
buffer to it.

I find the link to the actual info buffer is useful as a lot of
the time, you might want to insert the buffer so you can store it
with other useful information inside the zetteldesk-scratch
buffer, but then, you are interested in looking into the other
nodes of the manual you were reading.

Optional argument ARG which is a `\\[universal-argument]' switch to the
zetteldesk-scratch buffer in a split."
	(interactive "P")
	(let ((info_node (completing-read "Nodes: " zetteldesk-info-nodes))
	      (location (zetteldesk-insert-location))
	      (buffer (current-buffer)))
	  (Info-goto-node info_node)
	  (with-current-buffer location
	    (goto-char (point-max))
	    (newline)
	    (org-insert-heading)
	    (insert "Supportive Material - " info_node " (Info)")
	    (newline)
	    (save-excursion (insert-buffer-substring "*info*")
			    (insert
			     (org-link-make-string
			      (concat "elisp:(Info-goto-node \"" info_node "\")")
			      "See this node in its context")))
	    (kill-whole-line 2))
	  (switch-to-buffer buffer)
	  (zetteldesk-insert-switch-to-scratch arg)))

(provide 'zetteldesk-info)
;;; zetteldesk-info.el ends here
