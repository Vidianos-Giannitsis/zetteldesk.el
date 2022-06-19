;;; zetteldesk-ref-citar.el --- A zetteldesk extension for interfacing with literature nodes  -*- lexical-binding: t; -*-

;; Author: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; Maintainer: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; URL: https://github.com/Vidianos-Giannitsis/zetteldesk-ref-citar.el
;; Package-Requires: ((zetteldesk "1.0") (citar "0.9") (emacs "26.1"))
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
;; zetteldesk. The original zetteldesk-ref package uses the
;; bibtex-completion package as its backend which is what I currently
;; use. However, Citar is another great package for managing
;; bibliography so I decided to port that package to work with Citar
;; as well.

;;; Code:

(require 'zetteldesk)
(require 'citar)

(defun zetteldesk-ref-citar-node-from-refs ()
  "Collects a list of ref nodes.

The candidates are collected using `citar--get-candidates' with
the filter `citar-has-note' and using their citekey, the
org-roam-nodes associated to those citekeys are collected with
`org-roam-node-from-ref'."
  (cl-loop for cand in (citar--get-candidates nil (citar-has-note))
	   for key = (cadr cand)
	   collect (org-roam-node-from-ref (concat "@" key))))

(defun zetteldesk-ref-citar-citekey-from-node ()
  "Collects the citekeys of org-roam-nodes in the `zetteldesk-desktop'.

Ignores nodes for which `org-roam-node-refs' returns nil."
  (let* ((init-list (org-roam-node-list))
	 (zetteldesk-nodes (cl-remove-if-not #'zetteldesk-node-p init-list)))
    (cl-loop for node in zetteldesk-nodes
	     if (org-roam-node-refs node)
	     collect (car (org-roam-node-refs node)))))

(defun zetteldesk-ref-citar-roam-node-read--completions* (node-list &optional filter-fn sort-fn)
  "Run `org-roam-node-read--completions' with NODE-LIST being a list of nodes.

Typically, the function takes `org-roam-node-list' as the initial
list of nodes and creates the alist `org-roam-node-read'
uses.  However, it can be helpful to supply the list of nodes
yourself, when the predicate function used cannot be inferred
through a filter function of the form this function
takes.  FILTER-FN and SORT-FN are the same as in
`org-roam-node-read--completions'.  The resulting alist is to be
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

(defun zetteldesk-ref-citar-roam-node-read* (node-list &optional initial-input filter-fn sort-fn require-match prompt)
  "Run `org-roam-node-read' with the nodes supplied by NODE-LIST.

NODE-LIST is a list of nodes passed to
`zetteldesk-ref-citar-roam-node-read--completions*', which creates an alist of
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

(defun zetteldesk-ref-citar-add-node-to-desktop (NODE)
  "Add NODE to the `zetteldesk-desktop'.

NODE is a literature note that is part of the org-roam
repository.  The list of such nodes is gathered with
`zetteldesk-ref-citar-node-from-refs'."
  (interactive (list (zetteldesk-ref-roam-node-read* (zetteldesk-ref-citar-node-from-refs))))
  (let ((buffer (org-roam-node-buffer NODE))
	(file (org-roam-node-file NODE))
	(org-startup-with-latex-preview nil))
    (if buffer
	(zetteldesk--add-buffer buffer)
      (zetteldesk--add-buffer (find-file-noselect file)))))

(defun zetteldesk-ref-citar-remove-node-from-desktop (NODE)
  "Remove NODE from the `zetteldesk-desktop'.

NODE is a literature note that is currently part of the
zetteldesk, meaning its part of the list generated by
`zetteldesk-ref-citar-node-from-refs'."
  (interactive
   (list (zetteldesk-ref-roam-node-read* (zetteldesk-ref-citar-node-from-refs) nil #'zetteldesk-node-p)))
  (let ((buffer (org-roam-node-buffer NODE)))
    (zetteldesk--remove-buffer buffer)))

(defun zetteldesk-ref-citar-find-ref-node ()
  "Execute a filtered version of `citar-open-notes' in an org-roam UI.

 It collects a list of nodes which are reference nodes linked to
a bibtex entry through `zetteldesk-ref-citar-node-from-refs' and passes
it to `zetteldesk-ref-roam-node-read*' which is a modified
version of `org-roam-node-read' which takes a list of nodes as an
argument.  Since this required a rewrite of `org-roam-node-read',
finding the file is done indirectly and not through
`org-roam-node-file'."
  (interactive)
  (find-file (org-roam-node-file (zetteldesk-ref-roam-node-read* (zetteldesk-ref-citar-node-from-refs) nil #'zetteldesk-node-p))))

(defun zetteldesk-ref-citar-has-note ()
  "Return predicate testing whether entry is associated to a zetteldesk node.

Return a function that takes arguments KEY and ENTRY and returns
non-nil when the entry has associated notes in
`citar-notes-paths` and that note is part of the current
`zetteldesk-desktop'. This function builds on top of the
behaviour of `citar-has-note' by making the predicate an `and'
statement between the preexisting code of that function and a
check that the citekey is a `member' of the list obtained from
`zetteldesk-ref-citar-citekey-from-node'.

Note: for performance reasons, this function should be called
once per command; the function it returns can be called
repeatedly."
  ;; Call each function in `citar-has-note-functions` to get a list of predicates
  (let ((preds (mapcar #'funcall citar-has-note-functions)))
    ;; Return a predicate that checks if `citekey` and `entry` have a note
    (lambda (citekey entry)
      ;; Call each predicate with `citekey` and `entry`; return the first non-nil result
      (and
       (seq-some (lambda (pred) (funcall pred citekey entry)) preds)
       (member (concat "cite:" citekey)
	       (zetteldesk-ref-citar-citekey-from-node))))))

(defun zetteldesk-ref-citar-open-notes (key-entry)
  "Run a filtered version of `citar-open-notes' in its own UI.

The filter used is `zetteldesk-ref-citar-has-note' which is a
predicate that checks if the entry is associated to a ref note
that is part of the `zetteldesk-desktop'. This is similar to
`'zetteldesk-ref-citar-find-ref-node' which however uses the
`org-roam' completion UI.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-ref
		      :rebuild-cache current-prefix-arg
		      :filter (zetteldesk-ref-citar-has-note))))
  (let* ((key (car key-entry))
	 (entry (cdr key-entry)))
    (if (listp citar-open-note-functions)
	(citar--open-notes key entry)
      (error "Please change the value of 'citar-open-note-functions' to a list"))))

(defun zetteldesk-ref-citar-insert-ref-node-contents (&optional arg)
  "Select a node that is part of the current `zetteldesk-desktop' and a ref node.

Ref nodes are nodes that refer to reference material such as an
article.  These are gathered with `zetteldesk-ref-citar-node-from-refs' and
shown to the user through `zetteldesk-ref-citar-roam-node-read*' filtered
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
	  (zetteldesk-ref-roam-node-read* (zetteldesk-ref-citar-node-from-refs) nil #'zetteldesk-node-p))
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

(provide 'zetteldesk-ref-citar)
;;; zetteldesk-ref-citar.el ends here
