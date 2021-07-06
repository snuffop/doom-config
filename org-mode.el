;;; org-mode.el -*- lexical-binding: t; -*-

(after! org

  ;; Setting
  (setq org-contacts-files '("~/Nextcloud/Notes/org/contacts.org"))
  (setq org-default-notes-file (concat org-directory "0mobile.org"))
  (setq org-download-image-dir "~/Nextcloud/Notes/images/")
  (setq org-id-locations-file "~/Nextcloud/Notes/org-id-locations")
  (setq org-persp-startup-org-file "~/Nextcloud/Notes/org/0mobile.org")
  (setq org-projectile-file "todo.org")
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴"))

  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)


  ;;; Org Roam

  (setq org-roam-buffer-width 0.15)
  (setq org-roam-directory "~/Nextcloud/Notes/org/")
  (setq org-roam-index-file "~/Nextcloud/Notes/org/index.org")

   ;;; Org Roam Capture Templates
  (after! org-roam
    (custom-set-faces '(org-roam-link ((t (:inherit org-link :foreground "#F2C3BD")))))

  (setq org-roam-dailies-capture-templates
         '(
           ("d" "default" plain (function org-roam--capture-get-point)
            "%?"
            :file-name "daily/%<%Y-%m-%d>"
            ;; :head "#+TITLE: Daily Notes for %<%A, %B %d %Y>"
            :unnarrowed t)
           ("t" "Tasks" entry
            #'org-roam-capture--get-point
            "* TODO  %?"
            :file-name "daily/%<%Y-%m-%d>"
            :olp ("Tasks"))
           ("r" "Rackspace" entry
            #'org-roam-capture--get-point
            "* %<%H:%M>  %?"
            :file-name "daily/%<%Y-%m-%d>"
            :olp ("Rackspace"))
           ("j" "Journal" entry
            #'org-roam-capture--get-point
            "* %<%H:%M>  %?"
            :file-name "daily/%<%Y-%m-%d>"
            :olp ("Journal"))))

  ) ;; after org-roam
);; after org
