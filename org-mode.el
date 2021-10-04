;;; org-mode.el --- Summary -*- lexical-binding: t -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code
;;;; ORG-MODE AFTER! ORG
(setq org-directory "~/Nextcloud/Notes/org/")
(setq org-roam-directory "~/Nextcloud/Notes/org/")
(setq org-roam-dailies/directory "daily/")
(setq org-contacts-files '("~/Nextcloud/Notes/org/contacts.org"))

(after! org

;;;;; MAIL/MUTT

  ;; (org-add-link-type "message" 'mutt-open-message)

;;;;; ORG AGENDA

  (setq  marty/org-agenda-files (list
                                 (concat org-directory "Tasks.org")
                                 (concat org-directory "Habits.org")
                                 (concat org-directory "Calendar.org")
                                 (concat org-directory "contacts.org")
                                 (concat org-directory "Someday.org")
                                 (concat org-directory "0mobile.org")
                                 "~/.cache/calendar/google.org"
                                 "~/.cache/calendar/tatjana.org"))
  (after! org-agenda

    (setq org-agenda-block-separator nil)
    (setq org-agenda-compact-blocks t)
    (setq org-agenda-files marty/org-agenda-files)
    (setq org-agenda-include-deadlines t)
    (setq org-agenda-start-on-weekday 1)
    (setq org-agenda-start-with-log-mode t)
    (setq org-agenda-tags-column 100) ;; from testing this seems to be a good value
    (setq org-agenda-window-setup 'current-window)
    (setq org-deadline-warning-days 14)

    ;; Ignore scheduled tasks in task list
    (setq org-agenda-todo-ignore-scheduled 'all)
    (setq org-agenda-todo-ignore-deadlines 'far)

    ;; Skip Finished Items
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)

    (require 'org-projectile)
    (mapcar #'(lambda (file)
                (when (file-exists-p file)
                  (push file org-agenda-files)))
            (org-projectile-todo-files)))

;;;;; PUBLISH ALIST

  (setq org-publish-project-alist
        '(
          ("NSI-Documentation-content"
           :base-directory "~/Source/NSI/NSI-Documentation/"
           :base-extension "org"
           :publishing-directory "~/Source/NSI/NSI-Documentation/docs"
           :publishing-function marty/publish
           :exclude "Archive"
           :section-numbers nil
           :with-toc nil
           :auto-sitemap t
           :sitemap-filename "filemap.org"
           :sitemap-title "& Sitemap"
           :headline-levels 10
           :auto-preamble t
           :recursive t)

          ("NSI-Documentation-images"
           :base-directory "~/Source/NSI/NSI-Documentation/images/"
           :base-extension "jpg\\|gif\\|png"
           :publishing-directory "~/Source/NSI/NSI-Documentation/docs/images/"
           :publishing-function org-publish-attachment
           :recursive t)

          ("NSI-Documentation-TVA-ScanReports-2020-images"
           :base-directory "~/Source/NSI/NSI-Documentation/TVA/ScanReports/2020/images/"
           :base-extension "jpg\\|gif\\|png"
           :publishing-directory "~/Source/NSI/NSI-Documentation/docs/TVA/ScanReports/2020/images/"
           :publishing-function org-publish-attachment
           :recursive t)

          ("NSI-Documentation-TVA-ScanReports-2020-reports"
           :base-directory "~/Source/NSI/NSI-Documentation/TVA/ScanReports/2020/reports/"
           :base-extension "ods\\|csv\\|xls\\|xslt\\|pdf"
           :publishing-directory "~/Source/NSI/NSI-Documentation/docs/TVA/ScanReports/2020/reports/"
           :publishing-function org-publish-attachment
           :recursive t)

          ("NSI-Documentation-TVA-ScanReports-2021-images"
           :base-directory "~/Source/NSI/NSI-Documentation/TVA/ScanReports/2021/images/"
           :base-extension "jpg\\|gif\\|png"
           :publishing-directory "~/Source/NSI/NSI-Documentation/docs/TVA/ScanReports/2021/images/"
           :publishing-function org-publish-attachment
           :recursive t)

          ("NSI-Documentation-TVA-ScanReports-2021-reports"
           :base-directory "~/Source/NSI/NSI-Documentation/TVA/ScanReports/2021/reports/"
           :base-extension "ods\\|csv\\|xls\\|xslt\\|pdf"
           :publishing-directory "~/Source/NSI/NSI-Documentation/docs/TVA/ScanReports/2021/reports/"
           :publishing-function org-publish-attachment
           :recursive t)

          ("NSI-Documentation-TVA-ScanReports-files"
           :base-directory "~/Source/NSI/NSI-Documentation/TVA/ScanReports/files/"
           :base-extension "ods\\|csv\\|xls\\|xslt\\|pdf"
           :publishing-directory "~/Source/NSI/NSI-Documentation/docs/TVA/ScanReports/files/"
           :publishing-function org-publish-attachment
           :recursive t)

          ("salt-master"
           :base-directory "~/Source/NSI/salt-master/"
           :base-extension "org"
           :publishing-directory "~/Source/NSI/salt-master/docs"
           :publishing-function marty/publish
           :exclude "docs"
           :section-numbers nil
           :with-toc nil
           :auto-sitemap t
           :sitemap-filename "filemap.org"
           :sitemap-title "& Sitemap"
           :headline-levels 7
           :auto-preamble t
           :recursive t)

          ("NSI-Documentation" :components ("NSI-Documentation-content" "NSI-Documentation-images" "NSI-Documentation-TVA-ScanReports-2020-images" "NSI-Documentation-TVA-ScanReports-2020-reports" "NSI-Documentation-TVA-ScanReports-2021-images" "NSI-Documentation-TVA-ScanReports-2021-reports" "NSI-Documentation-TVA-ScanReports-files"))))

;;;;; SETTING
;;;;;; BASE
  (setq org-default-notes-file (concat org-directory "0mobile.org"))
  (setq org-download-image-dir "~/Nextcloud/Notes/images/")
  (setq org-id-locations-file "~/Nextcloud/Notes/org-id-locations")
  (setq org-persp-startup-org-file "~/Nextcloud/Notes/org/0mobile.org")
  (setq org-projectile-file "todo.org")
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴"))
  (setq org-clock-sound "~/Nextcloud/Music/sounds/shipsBell.wav")

  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)

;;;;;; CLOCKING

  ;; (setq org-clock-into-drawer "clocking")
  ;; where to put the clock in and out for tracked items
  (setq org-clock-out-remove-zero-time-clocks t)

;;;;;; LOGGING AND ID

  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-icalendar-store-UID t)
  (setq org-id-track-globally t)

;;;;;; REFILE TARGETS

  ;; ;; (setq myroamfiles (directory-files "~/nextcloud/notes/org/" t "org$"))
  ;; (setq myroamdailiesfiles (directory-files "~/nextcloud/notes/org/daily/" t "org$"))
  ;;
  ;; (myroamfiles :maxlevel . 5)
  ;; (myroamdailiesfiles :maxlevel . 2)))

  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 5)))

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

;;;;;; SYMBOLS

  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (setq-default prettify-symbols-alist '(
                                         ("->"              . "→")
                                         ("->>"             . "↠")
                                         ("<-"              . "←")
                                         ("<="              . "≤")
                                         ("<|"              . "◁")
                                         ("=>"              . "⇒")
                                         (">="              . "≥")
                                         ("|>"              . "▷")
                                         ("[ ]"             . "☐")
                                         ("[-]"             . "⊡")
                                         ("[X]"             . "☑")
                                         ("lambda"          . "λ")
                                         ("#+BEGIN_EXAMPLE" . ">EG>")
                                         ("#+BEGIN_SRC"     . "†")
                                         ("#+END_EXAMPLE"   . "<EG<")
                                         ("#+END_SRC"       . "†")
                                         ("#+begin_example" . ">EG>")
                                         ("#+begin_src"     . "†")
                                         ("#+end_example"   . "<EG<")
                                         ("#+end_src"       . "†")
                                         ("[ ]"             . "☐")
                                         ("[X]"             . "☑")
                                         ("[-]"             . "❍")
                                         ))

;;;;;; TAG LIST

  (setq org-tag-alist (quote
                       ((:startgroup)
                        ("@ASITS"     . ?A)
                        ("@BillPay"   . ?B)
                        ("@RedEarth"  . ?D)
                        ("@Email"     . ?E)
                        ("@Jazney"    . ?J)
                        ("@Outside"   . ?o)
                        ("@PhoneCall" . ?p)
                        ("@Personal"  . ?P)
                        ("@Rackspace" . ?R)
                        ("@Reading"   . ?r)
                        ("@Shopping"  . ?s)
                        ("@errand"    . ?e)
                        ("@home"      . ?h)
                        ("@inside"    . ?i)
                        ("@masons"    . ?M)
                        ("@music"     . ?m)
                        ("@office"    . ?O)
                        ("@system"    . ?x)
                        ("2637E20th")
                        (:endgroup)
                        ("CANCELLED"  . ?C)
                        ("DRAFT"      . ?D)
                        ("FLAGGED"    . ?F)
                        ("HOLD"       . ?H)
                        ("IDEA"       . ?I)
                        ("NOTE"       . ?N)
                        ("PROJECT"    . ?P)
                        ("WAITING"    . ?w)
                        ("WORK"       . ?W))))

;;;;;; CAPTURE TEMPLATES

  (setq org-capture-templates
        '(("t" "Task" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           (file "~/.config/doom/templates/todo.orgcaptmpl"))
          ("c" "Contacts" entry (file-olp "~/Nextcloud/Notes/org/contacts.org" "General")
           (file "~/.config/doom/templates/contact.orgcaptmpl"))
          ("p" "Protocol" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox" entry)
           "** %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("R" "Remember-mutt" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Mail")
           (file "~/.config/doom/templates/org-templates/mail.orgcaptmpl"))
          ("L" "Protocol Link" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           "** %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
          ("w" "Web site" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           (file "~/.config/doom/templates/org-templates/weblink.orgcaptmpl"))
          ("s" "Simple" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Popup")
           "%[~/.emacs.d/.org-popup]" :immediate-finish t :prepend t)

          ("m" "Email Workflow")
          ("mf" "Follow Up" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\nSCHEDULED:%t\n%a\n\n%i")
          ("ma" "auto Follow Up" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\n%a\n\n%i" :immediate-finish t)
          ("mF" "Follow Up With Deadline" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%a\n\n%i")
          ("mr" "Read Later" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Read Later")
           "* TODO Read  Later on %:subject\nSCHEDULED:%t\n%a\n\n%i":immediate-finish t)
          ("mm" "Masons Follow Up" entry
           (file+olp "~/Nextcloud/Notes/org/Masons.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject %a\nSCHEDULED:%t\n\\n%i")
          ("mR" "Workflow Rackspace")
          ("mRf" "Follow Up" entry
           (file+olp "~/Nextcloud/Notes/org/Rackspace.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%a\n\n%i")
          ("mRr" "Read Later" entry
           (file+olp "~/Nextcloud/Notes/org/Rackspace.org" "Read Later")
           "* TODO Read  Later with %:fromname on %:subject\nSCHEDULED:%t\n%a\n\n%i" :immediate-finish t)
          ))

  (setq org-protocol-default-template-key "t")

