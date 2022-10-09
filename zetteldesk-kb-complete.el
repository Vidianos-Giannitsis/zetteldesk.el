;;; zetteldesk-kb-complete.el --- Keybindings for zetteldesk.el and extensions  -*- lexical-binding: t; -*-

;; Author: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; Maintaner: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; URL: https://github.com/Vidianos-Giannitsis/zetteldesk-kb.el
;; Package-Requires: ((zetteldesk "1.0") (hydra "0.15") (major-mode-hydra "0.2") (emacs "24.1") (zetteldesk-info "0.2") (zetteldesk-ref "0.2") (zetteldesk-remark "0.2"))
;; Created: 23rd May 2022
;; License: GPL-3.0
;; Version: 0.1

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

;; This file is basically zetteldesk-kb.el, but aplified with all the
;; keybindings for the optional extensions. This is a package which
;; obviously has a lot of dependencies and is not planned to be on
;; MELPA. Its just here for the convenience of myself and anyone else
;; using all of zetteldesk.el's extensions. For more details on these
;; keybindings refer to zetteldesk-kb.

;;; Code:

(require 'zetteldesk)
(require 'zetteldesk-info)
(require 'zetteldesk-ref)
(require 'zetteldesk-ref-citar)
(require 'zetteldesk-remark)
(require 'hydra)
(require 'pretty-hydra)

;; Main Package
;; Supplementary Hydras

(pretty-hydra-define zetteldesk-add-hydra (:color blue :title "Add to Zetteldesk")
  ("Org-Roam"
   (("n" zetteldesk-add-node-to-desktop "Add Node")
    ("b" zetteldesk-add-backlinks-to-desktop "Add Current Node + All its backlinks")
    ("p" zetteldesk-add-poi-or-moc-backlink-to-desktop "Select MOC or POI - Add one of its backlinks"))

   "Other"
   (("a" zetteldesk-add-to-desktop "Add Buffer"))))

(pretty-hydra-define zetteldesk-remove-hydra (:color blue :title "Remove from Zetteldesk")
  ("Org-Roam"
   (("n" zetteldesk-remove-node-from-desktop "Remove Node")
    ("b" zetteldesk-remove-backlinks-from-desktop "Remove Current Node + All its backlinks"))

   "Other"
   (("r" zetteldesk-remove-from-desktop "Remove Buffer"))))

(pretty-hydra-define zetteldesk-insert-hydra (:color blue :title "Insert from the Zetteldesk")
  ("Org-Roam"
   (("n" zetteldesk-insert-node-contents "Link to Node and Contents in *zetteldesk-scratch*")
    ("N" zetteldesk-insert-node-contents-without-link "Node Contents in *zetteldesk-scratch")
    ("i" zetteldesk-node-insert "Link to Node")
    ("b" zetteldesk-node-insert-sort-backlinks "Link to Node - Menu sorted by Backlinks")
    ("m" zetteldesk-node-insert-if-poi-or-moc "Links to all POIs and MOCs"))

   "Supplementary Material to *zetteldesk-scratch*"
    (("p" zetteldesk-insert-link-to-pdf "Link to PDF")
     ("o" zetteldesk-insert-org-file-contents "Org File Contents"))))

;; The Primary Hydra

(pretty-hydra-define zetteldesk-main-hydra (:color blue :title "Zetteldesk Hydra")
  ("Filter Functions"
   (("b" zetteldesk-switch-to-buffer "Switch to Zetteldesk Buffer")
    ("n" zetteldesk-node-find "Find Zetteldesk Node"))

   "Add/Remove Hydras"
   (("r" zetteldesk-remove-hydra/body "Run the Removing Hydra")
    ("a" zetteldesk-add-hydra/body "Run the Adding Hydra"))

   "Inserting Things and *zetteldesk-scratch*"
   (("s" zetteldesk-switch-to-scratch-buffer "Switch to *zetteldesk-scratch*")
    ("i" zetteldesk-insert-hydra/body "Run the Insert Hydra"))

   "Saving/Restoring the Desktop"
   (("S" zetteldesk-save-state "Save the current zetteldesk-desktop")
    ("R" zetteldesk-restore-desktop "Restore a saved zetteldesk-desktop"))))

;; Set up the keybinding to call the hydra

(defcustom zetteldesk-kb-hydra-prefix (kbd "C-c z")
  "Stores the value of the keybinding that calls the main zetteldesk hydra."
  :type 'string
  :group 'zetteldesk)

(defvar zetteldesk-map
  (let ((km (make-sparse-keymap)))
    (define-key km zetteldesk-kb-hydra-prefix #'zetteldesk-main-hydra/body) km)
  "Keymap for zetteldesk.el")

;; zetteldesk-ref.el additions
;; (Note) For the functions that use the org-roam UI and just need a
;; bibliography backend, since both bibtex-completion and citar are
;; implemented and able to be used, I load the ones in citar in this
;; hydra as they appear to be faster by a small bit.
(pretty-hydra-define+ zetteldesk-insert-hydra ()
  ("Org-Roam"
   (("r" zetteldesk-ref-citar-insert-ref-node-contents "Link to citekey and Node Contents in *zetteldesk-scratch with special formatting"))))

(pretty-hydra-define zetteldesk-literature-hydra (:color blue :title "Zetteldesk Literature Nodes")
  ("Org-Roam UI"
   (("r" zetteldesk-ref-find-ref-node))

   "Helm-Bibtex UI"
   (("h" zetteldesk-ref-helm-bibtex-with-notes))

   "Ivy-Bibtex UI"
   (("i" zetteldesk-ref-ivy-bibtex-with-notes))

   "Citar UI"
   (("c" zetteldesk-ref-citar-open-note))))

(pretty-hydra-define+ zetteldesk-add-hydra ()
  ("Org-Roam"
   (("l" zetteldesk-ref-citar-add-node-to-desktop "Add Literature Node"))))

(pretty-hydra-define+ zetteldesk-remove-hydra ()
  ("Org-Roam"
   (("l" zetteldesk-ref-citar-remove-node-from-desktop "Remove Literature Node"))))

(pretty-hydra-define+ zetteldesk-main-hydra ()
  ("Filter Functions"
   (("l" zetteldesk-literature-hydra/body "Go to Zetteldesk Literature Node"))))

;; zetteldesk-remark.el

(pretty-hydra-define zetteldesk-remark-hydra (:color blue :title "Org-remark Integration")
  ("Zetteldesk Remark Functions"
   (("m" zetteldesk-remark-mark "Mark region and create margin note")
    ("s" zetteldesk-remark-switch-to-margin-notes "Switch to the margin notes file"))

   "Org Remark Functions"
   (("o" org-remark-open "Open margin note")
    ("n" org-remark-view-next "Open next margin note" :exit nil)
    ("p" org-remark-view-prev "Open previous margin note" :exit nil)
    ("v" org-remark-view "Open margin note without switching to it" :exit nil))

   "Quit"
   (("q" nil "quit"))))

(pretty-hydra-define+ zetteldesk-main-hydra ()
    ("Inserting Things and *zetteldesk-scratch*"
     (("m" zetteldesk-remark-hydra/body "Run the Zetteldesk Remark Hydra"))))

;; zetteldesk-info.el

(pretty-hydra-define+ zetteldesk-add-hydra ()
  ("Other"
   (("i" zetteldesk-info-add-info-node-to-desktop "Add Info Node"))))

(pretty-hydra-define+ zetteldesk-remove-hydra ()
  ("Other"
   (("i" zetteldesk-info-remove-info-node-from-desktop "Remove Info Node"))))

(pretty-hydra-define+ zetteldesk-main-hydra ()
  ("Filter Functions"
   (("I" zetteldesk-info-goto-node "Go to Zetteldesk Info Node"))))

(pretty-hydra-define+ zetteldesk-insert-hydra ()
  ("Supplementary Material to *zetteldesk-scratch*"
   (("I" zetteldesk-info-insert-contents "Info Node Contents + Link to context"))))

(provide 'zetteldesk-kb-complete)
;;; zetteldesk-kb-complete ends here
