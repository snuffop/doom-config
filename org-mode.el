;;; org-mode.el -*- lexical-binding: t; -*-

(after! org

  (setq org-contacts-files '("~/Nextcloud/Notes/org/contacts.org"))
  (setq org-default-notes-file (concat org-directory "0mobile.org"))
  (setq org-download-image-dir "~/Nextcloud/Notes/images/")
  (setq org-id-locations-file "~/Nextcloud/Notes/org-id-locations")
  (setq org-persp-startup-org-file "~/Nextcloud/Notes/org/0mobile.org")
  (setq org-projectile-file "todo.org")
  (setq org-roam-buffer-width 0.15)
  (setq org-roam-directory "~/Nextcloud/Notes/org/")
  (setq org-roam-index-file "~/Nextcloud/Notes/org/index.org")

  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)

)
