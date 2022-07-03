;;; zetteldesk-kb.el --- Keybindings for zetteldesk.el  -*- lexical-binding: t; -*-

;; Author: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; Maintainer: Vidianos Giannitsis <vidianosgiannitsis@gmail.com>
;; URL: https://github.com/Vidianos-Giannitsis/zetteldesk-kb.el
;; Package-Requires: ((zetteldesk "1.0.1") (hydra "0.15") (major-mode-hydra "0.2") (emacs "24.1"))
;; Created: 3rd March 2022
;; License: GPL-3.0
;; Version: 0.3

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

;; This file defines a few hydras for the keybindings in
;; zetteldesk.el.  The hydra displays small descriptions of each
;; function to help a beginner with getting familiarised with the
;; package.  The keybindings used are based on what my personal config
;; uses, but to fit it all in a single hydra, there are some
;; differentiations.

;; I made this optional and not part of the main package as I don't
;; consider it essential, just helpful for those who want a ready set
;; of keybindings, with descriptions instead of the function names to
;; try the package out.  Due to the modularity of Emacs, I recommend
;; you set up your own keybindings either from scratch or by
;; customising these hydras so they make the most sense to you and fit
;; your mental model.  I however thought that something like this will
;; be very useful until you get the hang of the package.

;; The hydras are defined with the `pretty-hydra-define' macro from
;; the `major-mode-hydra' package as imo its end result is a very good
;; looking hydra menu, perfect for something like this.  For this
;; reason, this part of the package, relies on that package.

;;; Code:

;; Dependencies

(require 'zetteldesk)
(require 'hydra)
(require 'pretty-hydra)
;; There is also org-roam, but since this requires zetteldesk.el to be
;; loaded, that one should handle loading org-roam

;; Supplementary Hydras

(pretty-hydra-define zetteldesk-add-hydra (:color blue :title "Add to Zetteldesk")
  ("Org-Roam"
   (("n" zetteldesk-add-node-to-desktop "Add Node")
    ("b" zetteldesk-add-backlinks-to-desktop "Add Node + All its backlinks")
    ("p" zetteldesk-add-poi-or-moc-backlink-to-desktop "Select MOC or POI - Add one of its backlinks"))

   "Other"
   (("a" zetteldesk-add-to-desktop "Add Buffer"))))

(pretty-hydra-define zetteldesk-remove-hydra (:color blue :title "Remove from Zetteldesk")
  ("Org-Roam"
   (("n" zetteldesk-remove-node-from-desktop "Remove Node")
    ("b" zetteldesk-remove-backlinks-from-desktop "Remove Node + All its backlinks"))

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

(defcustom zetteldesk-kb-hydra-prefix (kbd "C-c .")
  "Stores the value of the keybinding that calls the main zetteldesk hydra."
  :type 'string
  :group 'zetteldesk)

(defvar zetteldesk-kb-map
  (let ((km (make-sparse-keymap)))
    (define-key km zetteldesk-kb-hydra-prefix #'zetteldesk-main-hydra/body) km))

;; Local variables:
;; byte-compile-docstring-max-column: 100
;; end:

(provide 'zetteldesk-kb)
;;; zetteldesk-kb.el ends here
