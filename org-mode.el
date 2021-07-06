;;; org-mode.el -*- lexical-binding: t; -*-

(after! org

;;;; Setting
  (setq org-contacts-files '("~/Nextcloud/Notes/org/contacts.org"))
  (setq org-default-notes-file (concat org-directory "0mobile.org"))
  (setq org-download-image-dir "~/Nextcloud/Notes/images/")
  (setq org-id-locations-file "~/Nextcloud/Notes/org-id-locations")
  (setq org-persp-startup-org-file "~/Nextcloud/Notes/org/0mobile.org")
  (setq org-projectile-file "todo.org")
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴"))

  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)

;;;; Templates
;;;;; Capture

  (setq org-capture-templates
        '(("t" "Task" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           (file "~/.spacemacs.d/templates/todo.orgcaptmpl"))
          ("c" "Contacts" entry (file "~/Nextcloud/Notes/org/contacts.org")
           (file "~/.spacemacs.d/templates/contact.orgcaptmpl"))
          ("p" "Protocol" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox"entry)
           "** %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("R" "Remember-mutt" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Mail")
           (file "~/.spacemacs.d/templates/org-templates/mail.orgcaptmpl"))
          ("L" "Protocol Link" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           "** %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
          ("w" "Web site" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           (file "~/.spacemacs.d/templates/org-templates/weblink.orgcaptmpl"))
          ("s" "Simple" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Popup")
           "%[~/.emacs.d/.org-popup]" :immediate-finish t :prepend t)

          ("m" "Email Workflow")
          ("mf" "Follow Up" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\nSCHEDULED:%t\n%a\n\n%i")
          ("ma" "auto Follow Up" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\n%a\n\n%i" :immediate-finish t)
          ("mF" "Follow Up With Deadline" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%a\n\n%i")
          ("mr" "Read Later" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Read Later")
          "* TODO Read  Later on %:subject\nSCHEDULED:%t\n%a\n\n%i":immediate-finish t)
          ("mm" "Masons Follow Up" entry (file+olp "~/Nextcloud/Notes/org/Masons.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject %a\nSCHEDULED:%t\n\\n%i")
          ("mR" "Workflow Rackspace")
          ("mRf" "Follow Up" entry (file+olp "~/Nextcloud/Notes/org/Rackspace.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%a\n\n%i")
          ("mRr" "Read Later" entry (file+olp "~/Nextcloud/Notes/org/Rackspace.org" "Read Later")
           "* TODO Read  Later with %:fromname on %:subject\nSCHEDULED:%t\n%a\n\n%i" :immediate-finish t)
          ))
  (setq org-protocol-default-template-key "t")


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
