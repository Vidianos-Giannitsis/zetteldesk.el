
;; Setup package.el
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install and Setup Org-Roam
;; Set the org-roam directory to ~/org_roam as that is what I use, if
;; you need something different, change it before testing this
(package-install 'org-roam)

(add-hook 'after-init-hook 'org-roam-setup)
(setq org-roam-v2-ack t)

(use-package org-roam
  :config
  (setq org-roam-directory "~/org_roam"))

;; Load zetteldesk.el and turn on zetteldesk-mode to have a
;; functioning zetteldesk-scratch
(load-file "~/Zetteldesk/zetteldesk.el")
(zetteldesk-mode 1)
