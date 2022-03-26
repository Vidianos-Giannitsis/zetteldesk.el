;;; zetteldesk-ref.el --- A zetteldesk extension for use with the Info
;;; program and literature nodes

;; Author: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; Maintaner: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; URL: https://github.com/Vidianos-Giannitsis/zetteldesk-ref.el
;; Package-Requires: ((zetteldesk "0.2") (bibtex-completion))
;; Created: 27th March 2022
;; License: GPL-3.0

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

;; This package provides some optional improvements to
;; zetteldesk.el. Specifically it introduces mechanisms for handling
;; nodes from the Info program built in to emacs and literature notes
;; associated to a bibtex entry which are mostly powered by
;; org-noter. This code makes these reference materials interface
;; better with the zetteldesk.

;;; Code:

;; -- Reference Nodes from Bibtex Entries --
(defun zetteldesk-note-refs-p ()
  "Predicate function that finds all bibtex completion candidates with a note.

Checks if every candidate has the \"=has-note=\" tag using
`assoc' and if it does, collects that candidate"
  (cl-loop for ref in (bibtex-completion-candidates)
	   if (assoc "=has-note=" ref)
	   collect ref))

(defun zetteldesk-citekey-from-refs ()
  "Function that finds the \"=key=\" tag from a list of candidates.

The list is collected with `zetteldesk-note-refs-p' which is a
list of candidates that have notes. Collects it using `assoc'."
  (cl-loop for ref in (zetteldesk-note-refs-p)
	   collect (assoc "=key=" ref)))

(defun zetteldesk-node-from-refs ()
  "Function that collects a list of ref nodes.

The nodes are collected from their citekey using
`org-roam-node-from-ref', while the citekeys themselves are
collected from `zetteldesk-citekey-from-refs'."
  (cl-loop for ref in (zetteldesk-citekey-from-refs)
	   collect (org-roam-node-from-ref (concat "cite:" (cdr ref)))))

(defun org-roam-node-read--completions* (node-list &optional filter-fn sort-fn)
  "Runs `org-roam-node-read--completions' with NODE-LIST being a list of nodes.

Typically, the function takes `org-roam-node-list' as the initial
list of nodes and creates the alist `org-roam-node-read'
uses. However, it can be helpful to supply the list of nodes
yourself, when the predicate function used cannot be inferred
through a filter function of the form this function
takes. FILTER-FN and SORT-FN are the same as in
`org-roam-node-read--completions'. The resulting alist is to be
used with `org-roam-node-read*'."
  (let* ((template (org-roam-node--process-display-format org-roam-node-display-template))
	 (nodes node-list)
	 (nodes (mapcar (lambda (node)
			  (org-roam-node-read--to-candidate node template)) nodes))
	 (nodes (if filter-fn
		    (cl-remove-if-not
		     (lambda (n) (funcall filter-fn (cdr n)))
		     nodes)
		  nodes))
	 (sort-fn (or sort-fn
		      (when org-roam-node-default-sort
			(intern (concat "org-roam-node-read-sort-by-"
					(symbol-name org-roam-node-default-sort))))))
	 (nodes (if sort-fn (seq-sort sort-fn nodes)
		  nodes)))
    nodes))

(defun org-roam-node-read* (node-list &optional initial-input filter-fn sort-fn require-match prompt)
  "Runs `org-roam-node-read' with the nodes supplied by NODE-LIST.

NODE-LIST is a list of nodes passed to
`org-roam-node-read--completions*', which creates an alist of
nodes with the proper formatting to be used in this
function. This is for those cases where it is helpful to use your
own list of nodes, because a predicate function can not filter
them in the way you want easily."
  (let* ((nodes (org-roam-node-read--completions* node-list filter-fn sort-fn))
	 (prompt (or prompt "Node: "))
	 (node (completing-read
		prompt
		(lambda (string pred action)
		  (if (eq action 'metadata)
		      `(metadata
			;; Preserve sorting in the completion UI if a sort-fn is used
			,@(when sort-fn
			    '((display-sort-function . identity)
			      (cycle-sort-function . identity)))
			(annotation-function
			 . ,(lambda (title)
			      (funcall org-roam-node-annotation-function
				       (get-text-property 0 'node title))))
			(category . org-roam-node))
		    (complete-with-action action nodes string pred)))
		nil require-match initial-input 'org-roam-node-history)))
    (or (cdr (assoc node nodes))
	(org-roam-node-create :title node))))

(defun zetteldesk-find-ref-node ()
  "Execute a filtered version of `ivy-bibtex-with-notes'.

This does not exactly run `ivy-bibtex-with-notes' as that doesn't
have a way to filter things. It collects a list of nodes which
are reference nodes linked to a bibtex entry through
`zetteldesk-node-from-refs' and passes it to
`org-roam-node-read*' which is a modified version of
`org-roam-node-read' which takes a list of nodes as an
argument. Since this required a rewrite of `org-roam-node-read',
finding the file is done indirectly and not through
`org-roam-node-file'."
  (interactive)
  (find-file (org-roam-node-file (org-roam-node-read* (zetteldesk-node-from-refs) nil #'zetteldesk-node-p))))

(defun zetteldesk-insert-ref-node-contents (&optional arg)
  "Select a node that is part of the current `zetteldesk' and a ref node.
Ref nodes are nodes that refer to reference material such as an
article. These are gathered with `zetteldesk-node-from-refs' and
shown to the user through `org-roam-node-read*' filtered
according to `zetteldesk-node-p'.

After selection, insert its citekey at point for future
reference, then in the location determined by
`zetteldesk-insert-location' (typically *zetteldesk-scratch*), go
to `point-max', insert a newline and then insert the contents of
the selected node but remove the first 4 lines which is the
unneeded property drawer. After, indent all headings by one level
and replace the #+title: with an asterisk. Finally, enter a
newline after the title, where the string \"Bibtex entry for
node: \" is entered suffixed by the citekey of the entry.

If given the optional argument ARG, which needs to be the
`\\[universal-argument]' also switch to the *zetteldesk-scratch*
buffer in a split."
  (interactive "P")
  (let* ((node
	  (org-roam-node-read* (zetteldesk-node-from-refs) nil #'zetteldesk-node-p))
	 (file (org-roam-node-file node))
	 (location (zetteldesk-insert-location))
	 (citekey (concat "cite:" (car (org-roam-node-refs node)))))
    (insert citekey)
    (set-buffer location)
    (goto-char (point-max))
    (save-excursion
      (insert-file-contents file))
    (kill-whole-line 4)
    (newline)
    (save-excursion
      (while (not (org-next-visible-heading 1))
	(org-metaright)))
    (replace-string "#+title: " "* ")
    (end-of-line 1)
    (newline)
    (insert "Bibtex entry for node: "
	    citekey))
  (zetteldesk-insert-switch-to-scratch arg))

;; -- Info Nodes --
(defcustom zetteldesk-info-nodes '()
  "List of info nodes that are part of the zetteldesk.
Initialised as an empty list"
  :type 'list
  :group 'zetteldesk)

(defun zetteldesk-add-info-node-to-desktop ()
  "Find the current info-node.
Then add its name to the list of the variable
`zetteldesk-info-nodes'"
  (interactive)
  (add-to-list 'zetteldesk-info-nodes (Info-copy-current-node-name)))

(defun zetteldesk-remove-info-node-from-desktop ()
  "Remove an info-node from the `zetteldesk'.
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

(defun zetteldesk-insert-info-contents (&optional arg)
  "Select an info node that is part of the current `zetteldesk'.
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

(provide 'zetteldesk-ref)
;;; zetteldesk-ref.el ends here
