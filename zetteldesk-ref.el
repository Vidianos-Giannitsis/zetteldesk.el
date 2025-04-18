;;; zetteldesk-ref.el --- A zetteldesk extension for interfacing with literature nodes   -*- lexical-binding: t; -*-

;; Author: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; Maintainer: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; URL: https://github.com/Vidianos-Giannitsis/zetteldesk-ref.el
;; Package-Requires: ((zetteldesk "1.0") (bibtex-completion "1.0") (emacs "26.1"))
;; Created: 27th March 2022
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
;; Specifically it introduces mechanisms for handling literature notes
;; associated to a bibtex entry which are mostly powered by org-noter.
;; This code makes these reference materials interface better with the
;; zetteldesk

;;; Code:

(require 'zetteldesk)
(require 'bibtex-completion)

(declare-function ivy-bibtex "ext:ivy-bibtex.el")
(declare-function helm-bibtex "ext:helm-bibtex.el")

;; -- Reference Nodes from Bibtex Entries --
(defun zetteldesk-ref-note-refs-p ()
  "Predicate function to find all bibtex completion candidates with a note.

Checks if every candidate has the \"=has-note=\" tag using
`assoc' and if it does, collects that candidate."
  (cl-loop for ref in (bibtex-completion-candidates)
		if (assoc "=has-note=" ref)
		collect ref))

(defun zetteldesk-ref-citekey-from-refs ()
  "Find the \"=key=\" tag from a list of candidates.

The list is collected with `zetteldesk-ref-note-refs-p' which is a
list of candidates that have notes. Collects it using `assoc'."
  (cl-loop for ref in (zetteldesk-ref-note-refs-p)
		collect (assoc "=key=" ref)))

(defun zetteldesk-ref-citekey-from-node ()
  "Collects the citekeys of org-roam-nodes in the `zetteldesk-desktop'.

Ignores nodes for which `org-roam-node-refs' returns nil."
  (let* ((init-list (org-roam-node-list))
	      (zetteldesk-nodes (cl-remove-if-not #'zetteldesk-node-p init-list)))
	 (cl-loop for node in zetteldesk-nodes
		  if (org-roam-node-refs node)
		  collect (car (org-roam-node-refs node)))))

(defun zetteldesk-ref-node-from-refs ()
  "Collects a list of ref nodes.

The nodes are collected from their citekey using
`org-roam-node-from-ref', while the citekeys themselves are
collected from `zetteldesk-ref-citekey-from-refs'."
  (cl-loop for ref in (zetteldesk-ref-citekey-from-refs)
		collect (org-roam-node-from-ref (concat "cite:" (cdr ref)))))

(defun zetteldesk-ref-roam-node-read--completions* (node-list &optional filter-fn sort-fn)
  "Run `org-roam-node-read--completions' with NODE-LIST being a list of nodes.

Typically, the function takes `org-roam-node-list' as the initial
list of nodes and creates the alist `org-roam-node-read'
uses.  However, it can be helpful to supply the list of nodes
yourself, when the predicate function used cannot be inferred
through a filter function of the form this function
takes.  FILTER-FN and SORT-FN are the same as in
`org-roam-node-read--completions'.  The resulting alist is to be
used with `zetteldesk-ref-roam-node-read*'."
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

(defun zetteldesk-ref-roam-node-read* (node-list &optional initial-input filter-fn sort-fn require-match prompt)
  "Run `org-roam-node-read' with the nodes supplied by NODE-LIST.

NODE-LIST is a list of nodes passed to
`zetteldesk-ref-roam-node-read--completions*', which creates an alist of
nodes with the proper formatting to be used in this
function.  This is for those cases where it is helpful to use your
own list of nodes, because a predicate function can not filter
them in the way you want easily.

INITIAL-INPUT, SORT-FN, FILTER-FN, REQUIRE-MATCH, PROMPT are the
same as in `org-roam-node-read'."
  (let* ((nodes (zetteldesk-ref-roam-node-read--completions* node-list filter-fn sort-fn))
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

(defun zetteldesk-ref-add-node-to-desktop (NODE)
  "Add NODE to the `zetteldesk-desktop'.

NODE is a literature note that is part of the org-roam
repository.  The list of such nodes is gathered with
`zetteldesk-ref-node-from-refs'."
  (interactive (list (zetteldesk-ref-roam-node-read* (zetteldesk-ref-node-from-refs))))
  (let ((buffer (org-roam-node-buffer NODE))
	     (file (org-roam-node-file NODE))
	     (org-startup-with-latex-preview nil))
	 (if buffer
	     (zetteldesk--add-buffer buffer)
	   (zetteldesk--add-buffer (find-file-noselect file)))))

(defun zetteldesk-ref-remove-node-from-desktop (NODE)
  "Remove NODE from the `zetteldesk-desktop'.

NODE is a literature note that is currently part of the
zetteldesk, meaning its part of the list generated by
`zetteldesk-ref-node-from-refs'."
  (interactive
	(list (zetteldesk-ref-roam-node-read* (zetteldesk-ref-node-from-refs) nil #'zetteldesk-node-p)))
  (let ((buffer (org-roam-node-buffer NODE)))
	 (zetteldesk--remove-buffer buffer)))

(defun zetteldesk-ref-find-ref-node ()
  "Execute a filtered version of `ivy-bibtex-with-notes' in an org-roam UI.

This does not exactly run `ivy-bibtex-with-notes' as that doesn't
have a way to filter things.  It collects a list of nodes which
are reference nodes linked to a bibtex entry through
`zetteldesk-ref-node-from-refs' and passes it to
`zetteldesk-ref-roam-node-read*' which is a modified version of
`org-roam-node-read' which takes a list of nodes as an
argument.  Since this required a rewrite of `org-roam-node-read',
finding the file is done indirectly and not through
`org-roam-node-file'."
  (interactive)
  (find-file (org-roam-node-file (zetteldesk-ref-roam-node-read* (zetteldesk-ref-node-from-refs) nil #'zetteldesk-node-p))))

(defun zetteldesk-ref-ivy-bibtex-with-notes (&optional arg)
  "Search `zetteldesk-desktop' BibTeX entries with notes using `ivy-bibtex'.

This function builds on `ivy-bibtex-with-notes', meaning it shows
a list of bibtex entries with notes, however its filtering
includes only nodes in the `zetteldesk-desktop'.

With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (cl-letf* ((candidates (zetteldesk-ref-note-refs-p))
		  ((symbol-function 'bibtex-completion-candidates)
		   (lambda ()
		     (cl-loop for ref in candidates
			      if (member (concat "cite:" (cdr (assoc "=key=" ref)))
					 (zetteldesk-ref-citekey-from-node))
			      collect ref))))
	 (ivy-bibtex arg)))

(defun zetteldesk-ref-helm-bibtex-with-notes (&optional arg)
  "Search `zetteldesk-desktop' BibTeX entries with notes using `helm-bibtex'.

This function builds on `helm-bibtex-with-notes', meaning it shows
a list of bibtex entries with notes, however its filtering
includes only nodes in the `zetteldesk-desktop'.

With a prefix ARG the cache is invalidated and the bibliography
reread."
  (interactive "P")
  (cl-letf* ((candidates (zetteldesk-ref-note-refs-p))
		  ((symbol-function 'bibtex-completion-candidates)
		   (lambda ()
		     (cl-loop for ref in candidates
			      if (member (concat "cite:" (cdr (assoc "=key=" ref)))
					 (zetteldesk-ref-citekey-from-node))
			      collect ref))))
	 (helm-bibtex arg)))

(defun zetteldesk-ref-insert-ref-node-contents (&optional arg)
  "Select a node that is part of the current `zetteldesk-desktop' and a ref node.
Ref nodes are nodes that refer to reference material such as an
article.  These are gathered with `zetteldesk-ref-node-from-refs' and
shown to the user through `zetteldesk-ref-roam-node-read*' filtered
according to `zetteldesk-node-p'.

After selection, in the location determined by
`zetteldesk-insert-location' (typically *zetteldesk-scratch*), go
to `point-max', insert a newline and then insert the contents of
the selected node but remove the first 4 lines which is the
unneeded property drawer.  After, indent all headings by one level
and replace the #+title: with an asterisk.  Finally, enter a
newline after the title, where the string \"Bibtex entry for
node: \" is entered suffixed by the citekey of the entry.

If given the optional argument ARG, which needs to be the
`\\[universal-argument]' also switch to the *zetteldesk-scratch*
buffer in a split. If given `\\[universal-argument]'
`\\[universal-argument]' also insert the citekey in the current
buffer. In `zetteldesk-insert-node-contents', inserting a link to
the node is the default behaviour and a seperate function is
implemented for when you don't want that. In this version, it
made more sense to order it this way in my opinion."
  (interactive "P")
  (let* ((node
	       (zetteldesk-ref-roam-node-read* (zetteldesk-ref-node-from-refs) nil #'zetteldesk-node-p))
	      (file (org-roam-node-file node))
	      (location (zetteldesk-insert-location))
	      (citekey (concat "cite:" (car (org-roam-node-refs node)))))
	 (when (equal arg '(16))
	   (insert citekey))
	 (set-buffer location)
	 (goto-char (point-max))
	 (save-excursion
	   (insert-file-contents file))
	 (kill-whole-line 4)
	 (newline)
	 (save-excursion
	   (while (not (org-next-visible-heading 1))
	     (org-metaright)))
	 (zetteldesk--replace-title)
	 (end-of-line 1)
	 (newline)
	 (insert "Bibtex entry for node: "
		 citekey))
  (zetteldesk-insert-switch-to-scratch arg))

(provide 'zetteldesk-ref)
;;; zetteldesk-ref.el ends here
