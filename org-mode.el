;;; org-mode.el -*- lexical-binding: t; -*-
;;;; ORG
(after! org
;;;;; Setting

  (setq org-contacts-files '("~/Nextcloud/Notes/org/contacts.org"))
  (setq org-default-notes-file (concat org-directory "0mobile.org"))
  (setq org-download-image-dir "~/Nextcloud/Notes/images/")
  (setq org-id-locations-file "~/Nextcloud/Notes/org-id-locations")
  (setq org-persp-startup-org-file "~/Nextcloud/Notes/org/0mobile.org")
  (setq org-projectile-file "todo.org")
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴"))


  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)

  ;; clocking

  (setq org-clock-into-drawer "CLOCKING")          ;; Where to put the clock in and out for tracked items
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Logging and ID

  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-icalendar-store-UID t)
  (setq org-id-track-globally t)

;;;;; org agenda

  (after! org-agenda

    (setq org-agenda-files '("~/Nextcloud/Notes/org"
                             "~/Nextcloud/Notes/org/daily"
                             "~/.cache/calendar/google.org"
                             "~/.cache/calendar/rackspace.org"
                             "~/.cache/calendar/tatjana.org"
                             "~/Nextcloud/Notes/Calendars/personal.org"
                             "~/Nextcloud/Notes/Calendars/org-mode.org"
                             "~/Nextcloud/Notes/Calendars/contact_birthdays.org"))

    (setq org-agenda-compact-blocks t)
    (setq org-agenda-include-deadlines t)
    (setq org-agenda-start-on-weekday 1)
    (setq org-agenda-start-with-log-mode t)
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

;;;;; Templates
;;;;;; Capture

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


;;;;; Org Roam

  (setq org-roam-buffer-width 0.15)
  (setq org-roam-directory "~/Nextcloud/Notes/org")
  (setq org-roam-index-file "~/Nextcloud/Notes/org/index.org")

;;;;;; Org Roam Capture Templates

  (after! org-roam

    (custom-set-faces '(org-roam-link ((t (:inherit org-link :foreground "#F2C3BD")))))

    (remove-hook! 'find-file-hook #'org-roam-open-buffer-maybe-h)

    (setq org-roam-dailies-capture-templates
          '(
            ("d" "default" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "daily/%<%Y-%m-%d>"
             ;; :head "#+TITLE: Daily Notes for %<%A, %B %d %Y>"
             :unnarrowed t)
            ("t" "Tasks" entry
             #'org-roam-capture--get-point
             "* TODO %?"
             :file-name "daily/%<%Y-%m-%d>"
             :olp ("Tasks"))
            ("r" "Rackspace" entry
             #'org-roam-capture--get-point
             "* %<%H:%M> %?"
             :file-name "daily/%<%Y-%m-%d>"
             :olp ("Rackspace"))
            ("j" "Journal" entry
             #'org-roam-capture--get-point
             "* %<%H:%M> %?"
             :file-name "daily/%<%Y-%m-%d>"
             :olp ("Journal"))))

    ) ;; after org-roam

;;;;; Publish Alist

  ;; Proprietary Stuff for work

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

;;;;; Tags

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

;;;;; Todo
;;;;;; Faces

  (setq org-todo-keyword-faces
        '(("TODO"       . org-warning)
          ("NEXT"       . (:foreground "#008080" :weight bold))
          ("STARTED"    . (:foreground "#E35DBF" :weight bold))
          ("BLOCKED"    . (:foreground "White"   :weight bold))
          ("TODELEGATE" . (:foreground "White"   :weight bold))
          ("DELEGATED"  . (:foreground "pink"    :weight bold))
          ("CANCELED"   . (:foreground "white"   :weight bold))
          ("TICKLE"     . (:foreground "White"   :weight bold))
          ("DONE"       . (:foreground "green"   :weight bold))))

;;;;;; keywords

  (setq org-todo-keywords
        '((sequence "TODO(t)"
                    "NEXT(n!)"
                    "STARTED(s!)"
                    "BLOCKED(b@/!)"
                    "TODELEGATE(g@/!)"
                    "DELEGATED(D@/!)"
                    "FOLLOWUP(f@/!)"
                    "TICKLE(T!)"
                    "|"
                    "CANCELLED(c@)"
                    "DONE(d@)")))
;;;;; Symbols

  (setq-default prettify-symbols-alist '(
                                         ("->"  .  "→")
                                         ("->>" .  "↠")
                                         ("<-"  .  "←")
                                         ("<="  . "≤")
                                         ("<|"  . "◁")
                                         ("=>"  . "⇒")
                                         (">="  . "≥")
                                         ("|>"  . "▷")
                                         ("[ ]" . "☐")
                                         ("[-]" . "⊡")
                                         ("[X]" . "☑")
                                         ("lambda" . "λ")
                                         ("#+BEGIN_EXAMPLE" . ">EG>")
                                         ("#+BEGIN_SRC" . "†")
                                         ("#+END_EXAMPLE" . "<EG<")
                                         ("#+END_SRC" . "†")
                                         ("#+begin_example" . ">EG>")
                                         ("#+begin_src" . "†")
                                         ("#+end_example" . "<EG<")
                                         ("#+end_src" . "†")
                                         ))
;;;;; Mail/Mutt

  (org-add-link-type "message" 'mutt-open-message)

;;;;; end ORG
  );; after org

;;;; org-caldav

;; (use-package org-caldav
;;   :after org
;;   :init
;;   ;; This is the sync on close function; it also prompts for save after syncing so
;;   ;; no late changes get lost
;;   (defun org-caldav-sync-at-close ()
;;     ;; (org-caldav-sync)
;;     (save-some-buffers))

;; This is the delayed sync function; it waits until emacs has been idle for
;; "secs" seconds before syncing.  The delay is important because the caldav-sync
;; can take five or ten seconds, which would be painful if it did that right at save.
;; This way it just waits until you've been idle for a while to avoid disturbing
;; the user.
;; (defvar org-caldav-sync-timer nil
;;   "Timer that `org-caldav-push-timer' used to reschedule itself, or nil.")
;; (defun org-caldav-sync-with-delay (secs)
;;   (when org-caldav-sync-timer
;;     (cancel-timer org-caldav-sync-timer))
;;   (setq org-caldav-sync-timer
;;         (run-with-idle-timer
;;          (* 1 secs) nil 'org-caldav-sync)))

;; Actual calendar configuration edit this to meet your specific needs
;; (setq org-caldav-url "https://nextcloud.dabuke.com/remote.php/dav/calendars/marty/")
;; (setq org-caldav-calendars
;;       '((:calendar-id "personal"
;;          :files ("~/Nextcloud/Notes/Calendars/personal.org")
;;          :inbox "~/Nextcloud/Notes/Calendars/personal.org")
;;         (:calendar-id "contact_birthdays"
;;          :files ("~/Nextcloud/Notes/Calendars/contact-birthdays.org")
;;          :inbox "~/Nextcloud/Notes/Calendars/contact-birthdays.org")
;;         (:calendar-id "org-mode"
;;          :files ("~/Nextcloud/Notes/Calendars/org-mode.org")
;;          :inbox "~/Nextcloud/Notes/Calendars/org-mode.org")))
;; (setq org-caldav-backup-file "~/Nextcloud/Notes/Calendars/org-caldav-backup.org")
;; (setq org-caldav-save-directory "~/Nextcloud/Notes/Calendars/")

;; :config
;; (setq org-icalendar-alarm-time 1)
;; ;; This makes sure to-do items as a category can show up on the calendar
;; (setq org-icalendar-include-todo t)
;; ;; This ensures all org "deadlines" show up, and show up as due dates
;; (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
;; ;; This ensures "scheduled" org items show up, and show up as start times
;; (setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
;; ;; Add the delayed save hook with a five minute idle timer
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when (eq major-mode 'org-mode)
;;               (org-caldav-sync-with-delay 300))))
;; Add the close emacs hook
;;(add-hook 'kill-emacs-hook 'org-caldav-sync-at-close))

;;;; Calendar

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    ;; (cfw:org-create-file-source "Google" "~/.cache/calendar/google.com" "Blue")
    (cfw:org-create-file-source "Tatjana" "~/.cache/calendar/tatjana.org" "Pink")  ; other org source
    ;; (cfw:org-create-file-source "Rackspace" "~/.cache/calendar/rackspace.org" "Red")  ; other org source
    (cfw:org-create-file-source "Next-Personal" "~/Nextcloud/Notes/Calendars/personal.org" "Blue")  ; other org source
    (cfw:org-create-file-source "Next-Birthdays" "~/Nextcloud/Notes/Calendars/contact_birthdays.org" "Brown")  ; other org source
    (cfw:org-create-file-source "Next-org-mode" "~/Nextcloud/Notes/Calendars/org-mode.org" "Brown")  ; other org source
    )))

;;;; TSfile Links

(defvar memacs-root "~/Nextcloud/Notes/memacs/")
(defvar memacs-file-pattern "files.org")

;; by John Kitchin
(defun my-handle-tsfile-link (querystring)
  ;; get a list of hits
  (let ((queryresults (split-string
                       (s-trim
                        (shell-command-to-string
                         (concat
                          "grep \""
                          querystring
                          "\" "
                          (concat memacs-root memacs-file-pattern))))
                       "\n" t)))
    ;; check length of list (number of lines)
    (cond
     ((= 0 (length queryresults))
      ;; edge case: empty query result
      (message "Sorry, no results found for query: %s" querystring))
     (t
      (with-temp-buffer
        (insert (if (= 1 (length queryresults))
                    (car queryresults)
                  (completing-read "Choose: " queryresults)))
        (org-mode)
        (goto-char (point-min))
        (org-next-link)
        (org-open-at-point "file:"))))))

(after! org
  (org-link-set-parameters
   "tsfile"
   :follow (lambda (path) (my-handle-tsfile-link path))
   :help-echo "Opens the linked file with your default application")
  )

(defun marty/dired-copy-filename-as-tsfile-link ()
  "Copy current file name with its basename as [[tsfile:<basename>]] custom org-mode link."
  (interactive)
  (dired-copy-filename-as-kill) ;; current file name to kill ring
  (let* ((filename (current-kill 0))) ;; get topmost kill ring element
    (kill-new (concat "[[tsfile:" filename "]]")) ;; write back new/modified kill ring element
    )
  )
