;; org-mode.el --- Summary -*- lexical-binding: t -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; CODE
;;;; ORG-MODE MAIN
(setq org-directory "~/Nextcloud/Notes/org/")
(setq org-roam-directory "~/Nextcloud/Notes/org/")
(setq org-roam-dailies/directory "daily/")
(setq org-contacts-files '("~/Nextcloud/Notes/org/contacts.org"))

(after! org

;;;;; MAIL/MUTT

  (org-add-link-type "message" 'mutt-open-message)

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

  ;; (setq org-clock-into-drawer "CLOCKING")
  ;; Where to put the clock in and out for tracked items
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
  ;; (setq-default prettify-symbols-alist '(
  ;;                                        ("->"              . "→")
  ;;                                        ("->>"             . "↠")
  ;;                                        ("<-"              . "←")
  ;;                                        ("<="              . "≤")
  ;;                                        ("<|"              . "◁")
  ;;                                        ("=>"              . "⇒")
  ;;                                        (">="              . "≥")
  ;;                                        ("|>"              . "▷")
  ;;                                        ("[ ]"             . "☐")
  ;;                                        ("[-]"             . "⊡")
  ;;                                        ("[X]"             . "☑")
  ;;                                        ("lambda"          . "λ")
  ;;                                        ("#+BEGIN_EXAMPLE" . ">EG>")
  ;;                                        ("#+BEGIN_SRC"     . "†")
  ;;                                        ("#+END_EXAMPLE"   . "<EG<")
  ;;                                        ("#+END_SRC"       . "†")
  ;;                                        ("#+begin_example" . ">EG>")
  ;;                                        ("#+begin_src"     . "†")
  ;;                                        ("#+end_example"   . "<EG<")
  ;;                                        ("#+end_src"       . "†")
  ;;                                        ("[ ]"             . "☐")
  ;;                                        ("[X]"             . "☑")
  ;;                                        ("[-]"             . "❍")
  ;;                                        ))
  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org Checkbox Symbol"
                             (push '("[ ]" .  "☐") prettify-symbols-alist)
                             (push '("[X]" . "☑" ) prettify-symbols-alist)
                             (push '("[-]" . "❍" ) prettify-symbols-alist)
                             (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
                             (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
                             (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
                             (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
                             (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
                             (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
                             (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
                             (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
                             (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
                             (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
                             (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
                             (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
                             (push '("#+TITLE:" . "") prettify-symbols-alist)
                             (push '("#+title:" . "") prettify-symbols-alist)
                             (push '("#+DESCRIPTION:" . "") prettify-symbols-alist)
                             (push '("#+ID:" . "") prettify-symbols-alist)
                             (push '("#+FILETAGS:" . "") prettify-symbols-alist)
                             (push '("#+filetags:" . "") prettify-symbols-alist)
                             (push '("#+ACTIVE:" . "") prettify-symbols-alist)
                             (push '("#+START_SPOILER" . "") prettify-symbols-alist)
                             (push '("#+CLOSE_SPOILER" . "") prettify-symbols-alist)
                             (push '("#+BEGIN_HIDDEN" . "") prettify-symbols-alist)
                             (push '("#+END_HIDDEN" . "") prettify-symbols-alist)
                             (push '("#+STARTUP:" . "🌟") prettify-symbols-alist)
                             (push '("#+startup:" . "🌟") prettify-symbols-alist)
                             (push '("#+CATEGORY:" . "⛏ ") prettify-symbols-alist)
                             (push '("#+category:" . "⛏ ") prettify-symbols-alist)
                             (push '("[#A]" . "⚡") prettify-symbols-alist)
                             (push '("[#B]" . "⬆") prettify-symbols-alist)
                             (push '("[#C]" . "■") prettify-symbols-alist)
                             (push '("[#D]" . "⬇") prettify-symbols-alist)
                             (push '("[#E]" . "❓") prettify-symbols-alist)
                             (push '("lambda" . "λ") prettify-symbols-alist)
                             (prettify-symbols-mode)))
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
          ("P" "Protocol" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           "** %^{Title}\n\n  Source: %u, %c\n\n  %i" :empty-lines 1)
          ("L" "Protocol Link" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           "** [[%:link][%:description]]\n")
          ("R" "Remember-mutt" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Mail")
           (file "~/.config/doom/templates/org-templates/mail.orgcaptmpl"))
          ("w" "Web site" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           (file "~/.config/doom/templates/org-templates/weblink.orgcaptmpl"))
          ("s" "Simple" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Popup")
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

;;;;;; FACES

  (custom-set-faces
   '(org-document-title ((t (:inherit outline-1 :height 1.5))))
   '(org-level-1 ((t (:inherit outline-1 :height 1.15))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.13))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   )

  (add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))

;;;;;; TODO FACES

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

;;;;;; KEYWORDS
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

  ) ;; End (after! org

;;;; ORG-ROAM

(after! org-roam

;;;;; ORG-ROAM CAPTURE TEMPLATES

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Journal"))
           :empty-lines-after 1 )
          ("t" "Tasks" entry "** TODO %? "
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Tasks"))
           :empty-lines-after 1 )
          ("r" "Rackspace" entry "** %<%H:%M> %?"
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Rackspace"))
           :empty-lines-after 1)
          ("j" "Journal" entry "** %<%H:%M> %?"
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Journal") )
           :empty-lines-after 1)))

  (setq org-roam-capture-templates
        '(("d" "default" plain
           (file "~/.config/doom/templates/roam-templates/default-capture-entry.org")
           :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n#+category: ${title}")
           :unnarrowed t)
          ("t" "tipjar" plain
           (file "~/.config/doom/templates/roam-templates/tipjar-entry.org")
           :if-new (file+head "TipJar/${slug}.org" "#+TITLE: ${title}\n#+filetags: tipjar\n#+category: tipjar\n")
           :unnarrowed t)
          ("p" "People" plain
           (file "~/.config/doom/templates/roam-templates/people-entry.org")
           :if-new (file+head "People/${slug}.org" "#+TITLE: ${title}\n#+category: people\n#+filetags: :people:\n")
           :unnarrowed t)))

;;;;; ORG-ROAM POPUP RULES
  (setq +org-roam-open-buffer-on-find-file nil)

  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .12 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .12 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
;;;;; ORG-ROAM HOOKS

  (add-hook 'find-file-hook #'roam-extra:update-todo-tag)
  (add-hook 'before-save-hook #'roam-extra:update-todo-tag)
  (advice-add 'org-agenda :before #'roam-extra:update-todo-files)

  ;; hook to be run whenever an org-roam capture completes
  (add-hook 'org-roam-capture-new-node-hook #'marty/add-other-auto-props-to-org-roam-properties)

;;;;; ORG-ROAM FUNCTIONS
;;;;;; ADD ADITIONAL PROPERTIES

  (defun marty/add-other-auto-props-to-org-roam-properties ()
    ;; if the file already exists, don't do anything, otherwise...
    (unless (file-exists-p (buffer-file-name))
      ;; if there's also a CREATION_TIME property, don't modify it
      (unless (org-find-property "CREATION_TIME")
        ;; otherwise, add a Unix epoch timestamp for CREATION_TIME prop
        ;; (this is what "%s" does - see http://doc.endlessparentheses.com/Fun/format-time-string )
        (org-roam-add-property
         (format-time-string "%s"
                             (nth 5
                                  (file-attributes (buffer-file-name))))
         "CREATION_TIME"))
      (unless (org-find-property "ORG_CREATION_TIME")
        (org-roam-add-property
         (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                             (nth 5
                                  (file-attributes (buffer-file-name))))
         "ORG_CREATION_TIME"))
      ;; similarly for AUTHOR and MAIL properties
      (unless (org-find-property "AUTHOR")
        (org-roam-add-property user-full-name "AUTHOR"))
      (unless (org-find-property "MAIL")
        (org-roam-add-property user-mail-address "MAIL"))
      ;; also add the latitude and longitude
      (unless (org-find-property "LAT_LONG")
        ;; recheck location:
        (marty/get-lat-long-from-ipinfo)
        (org-roam-add-property (concat (number-to-string calendar-latitude) "," (number-to-string calendar-longitude)) "LAT-LONG"))))

;;;;;; DAILIES GRAPHICS LINK
  (defun marty/org-roam-dailies-graphicslink ()
    " Set the Graphics Link to Today in the Pictures folder that maid pushes to."
    (interactive)
    (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
           (month (string-to-number (substring (buffer-name) 5 7)))
           (day   (string-to-number (substring (buffer-name) 8 10)))
           (datim (encode-time 0 0 0 day month year)))
      (format-time-string "[[/home/marty/Nextcloud/Pictures/2020 - 2029/%Y/%0m/Daily/%d][Graphics Link]]" datim)))

;;;;;; DAILIES TITLE
  (defun marty/org-roam-dailies-title ()
    (interactive)
    (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
           (month (string-to-number (substring (buffer-name) 5 7)))
           (day   (string-to-number (substring (buffer-name) 8 10)))
           (datim (encode-time 0 0 0 day month year)))
      (format-time-string "%A, %B %d %Y" datim)))

;;;;;; DAILIES TODO SCHEDULE
  (defun marty/org-roam-dailies-todo-schedule ()
    " Set the Date for the todo's in the dailies template "
    (interactive)
    (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
           (month (string-to-number (substring (buffer-name) 5 7)))
           (day   (string-to-number (substring (buffer-name) 8 10)))
           (datim (encode-time 0 0 0 day month year)))
      (format-time-string "SCHEDULED: [%Y-%m-%d %a 10:00]" datim)))

;;;;;; DAILIES TODO DEADLINE
  (defun marty/org-roam-dailies-todo-deadline ()
    " Set the Date for the todo's in the dailies template "
    (interactive)
    (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
           (month (string-to-number (substring (buffer-name) 5 7)))
           (day   (string-to-number (substring (buffer-name) 8 10)))
           (datim (encode-time 0 0 0 day month year)))
      (format-time-string "DEADLINE: [%Y-%m-%d %a 20:00]" datim)))

;;;;;; SYSTEMCRAFTERS INSERT IMMEDIATE
  ;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/

  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))


;;;;;; CAPTURE INBOX
  (defun marty/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                       :templates '(("i" "Inbox" plain "** %?"
                                     :if-new (file+olp "~/Nextcloud/Notes/org/0mobile.org" ("Inbox"))))))

;;;;;; MOVE TO TODAY
  ;; Move Todo's to dailies when done
  (defun marty/org-roam-move-todo-to-today ()
    (interactive)
    (let ((org-refile-keep nil) ;; Set this to t to copy the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+olp "%<%Y-%m-%d>.org" ("Tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))


;;;;;; ROAM-RG-SEARCH
  ;; Snagged from Roam discourse
  ;; https://org-roam.discourse.group/t/using-consult-ripgrep-with-org-roam-for-searching-notes/1226
  (defun marty/org-roam-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep org-roam-directory)))
;;;;;; ROAM-EXTRA
  (defun roam-extra:get-filetags ()
    (split-string (or (org-roam-get-keyword "filetags") "")))

  (defun roam-extra:add-filetag (tag)
    (let* ((new-tags (cons tag (roam-extra:get-filetags)))
           (new-tags-str (combine-and-quote-strings new-tags)))
      (org-roam-set-keyword "filetags" new-tags-str)))

  (defun roam-extra:del-filetag (tag)
    (let* ((new-tags (seq-difference (roam-extra:get-filetags) `(,tag)))
           (new-tags-str (combine-and-quote-strings new-tags)))
      (org-roam-set-keyword "filetags" new-tags-str)))

  (defun roam-extra:todo-p ()
    "Return non-nil if current buffer has any TODO entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (org-element-map
        (org-element-parse-buffer 'headline)
        'headline
      (lambda (h)
        (eq (org-element-property :todo-type h)
            'todo))
      nil 'first-match))

  (defun roam-extra:update-todo-tag ()
    "Update TODO tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (org-roam-file-p))
      (org-with-point-at 1
        (let* ((tags (roam-extra:get-filetags))
               (is-todo (roam-extra:todo-p)))
          (cond ((and is-todo (not (seq-contains-p tags "todo")))
                 (roam-extra:add-filetag "todo"))
                ((and (not is-todo) (seq-contains-p tags "todo"))
                 (roam-extra:del-filetag "todo")))))))

  (defun roam-extra:todo-files ()
    "Return a list of roam files containing todo tag."
    (org-roam-db-sync)
    (let ((todo-nodes (seq-filter (lambda (n)
                                    (seq-contains-p (org-roam-node-tags n) "todo"))
                                  (org-roam-node-list))))
      (seq-uniq (seq-map #'org-roam-node-file todo-nodes))))

  (defun roam-extra:update-todo-files (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (roam-extra:todo-files)))

;;;;; ORG-ROAM END
  )
;;;;; ORG-ROAM-MODULES
;;;;;; ORG-ROAM-UI
(use-package! org-roam-ui
  :after org-roam)
;;;; TSFILE LINKS

(after! org
  (defvar memacs-root "~/Nextcloud/Notes/memacs/")
  (defvar memacs-file-pattern "files.org")

  (with-eval-after-load 'org
    (org-link-set-parameters
     "tsfile"
     :follow (lambda (path) (my-handle-tsfile-link path))
     :help-echo "Opens the linked file with your default application"))

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

  (defun marty/dired-copy-filename-as-tsfile-link ()
    "Copy current file name with its basename as [[tsfile:<basename>]] custom org-mode link."
    (interactive)
    (dired-copy-filename-as-kill)       ;; current file name to kill ring
    (let* ((filename (current-kill 0))) ;; get topmost kill ring element
      (kill-new (concat "[[tsfile:" filename "]]")))))


;;;; ORG-MODE MODULES
;;;;; ORG-SUPER-AGENDA

(use-package! org-super-agenda
  :after org-agenda
  :commands (org-super-agenda-mode))

(after! org-agenda
  (org-super-agenda-mode)

  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-super-agenda-groups
                         '((:log t)  ; Automatically named "Log"
                           (:name "Schedule"
                            :time-grid t)
                           (:name "Today"
                            :scheduled today)
                           (:habit t)
                           (:name "Due today"
                            :deadline today)
                           (:name "Overdue"
                            :deadline past)
                           (:name "Due soon"
                            :deadline future)
                           (:name "Tickle"
                            :deadline future)
                           (:name "Unimportant"
                            :todo ("BLOCKED" "TODELEGATE" "DELEGATED" "CANCELED"
                                   :order 100)
                            (:name "Waiting..."
                             :todo "WAITING"
                             :order 98)
                            (:name "Scheduled earlier"
                             :scheduled past))))))))
          ("g" "group"
           ((agenda "" ((org-agenda-spam 'week)
                        (org-super-agenda-groups
                         '((:auto-category t))
                         )))))

          ("u" "Super view"
           ((agenda "" ((org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t)))))
            (todo "" ((org-agenda-overriding-header "Projects")
                      (org-super-agenda-groups
                       '((:name none  ; Disable super group header
                          :children todo)
                         (:discard (:anything t)))))))))))

;;;;; ORG-PANDOC
(use-package! org-pandoc-import
  :after org)

;;;;; ORG-EDNA-MODE
(after! org
  (org-edna-mode))
;;;;; ORG-TRANSCLUSION
(use-package! org-transclusion
  :defer t
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))



;; function to find latitude & longitude
;;                      (requires curl to be installed on system)
(setq calendar-latitude 0)
(setq calendar-longitude 0)
(defun marty/get-lat-long-from-ipinfo ()
  (let*
      ((latlong (substring
                 (shell-command-to-string "curl -s 'https://ipinfo.io/loc'")
                 0 -1))
       (latlong-list (split-string latlong ",")))
    (setq calendar-latitude (string-to-number (car latlong-list)))
    (setq calendar-longitude (string-to-number (cadr latlong-list)))))



(defun format-org-mode-block ()
  "Format org mode code block"
  (interactive "p")
  ;; (execute-kbd-macro (kbd "C-c ' C-x h C-M-\\ C-c '"))
  ;; (execute-kbd-macro (read-kbd-macro "C-c ' C-x h C-M-\\ C-c '"))
  (org-edit-special)
  (format-all-ensure-formatter)
  (format-all-buffer)
  (org-edit-src-exit))
