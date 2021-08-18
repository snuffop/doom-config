;;; org-mode.el --- Summary -*- lexical-binding: t -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(after! org

;; Mail/Mutt

(org-add-link-type "message" 'mutt-open-message)

;;;;; org agenda

(setq  marty/org-agenda-files (list
                               (concat org-directory "Tasks.org")
                               (concat org-directory "Habits.org")
                               (concat org-directory "Calendar.org")
                               (concat org-directory "contacts.org")
                               (concat org-directory "Someday.org")
                               (concat org-directory "0mobile.org")
                               "~/.cache/calendar/google.org"
                               "~/.cache/calendar/personal.org"
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

;;;;; Publish Alist

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

;;;;; Setting

(setq org-default-notes-file (concat org-directory "0mobile.org"))
(setq org-download-image-dir "~/Nextcloud/Notes/images/")
(setq org-id-locations-file "~/Nextcloud/Notes/org-id-locations")
(setq org-persp-startup-org-file "~/Nextcloud/Notes/org/0mobile.org")
(setq org-projectile-file "todo.org")
(setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴"))
(setq org-clock-sound "~/Nextcloud/Music/sounds/shipsBell.wav")

(remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)

;; clocking

(setq org-clock-into-drawer "CLOCKING")          ;; Where to put the clock in and out for tracked items
(setq org-clock-out-remove-zero-time-clocks t)

;; Logging and ID

(setq org-log-done t)
(setq org-log-into-drawer t)
(setq org-icalendar-store-UID t)
(setq org-id-track-globally t)

;; Refile targets

(setq myroamfiles (directory-files "~/Nextcloud/Notes/org/" t "org$"))
(setq myroamdailiesfiles (directory-files "~/Nextcloud/Notes/org/daily/" t "org$"))

(setq org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 5)
                           (myroamfiles :maxlevel . 5)
                           (myroamdailiesfiles :maxlevel . 2)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; symbols
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

;; Tag List
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

;;;;;; Capture Templates

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

;; Todo Faces
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

;; keywords
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

;; org-super-agenda

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

;; org-pandoc
(use-package! org-pandoc-import
  :after org)

;;;;; org-roam-ui
(use-package! org-roam-ui
  :after org-roam)

;; Org Roam Capture Templates
(after! org-roam
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "** %<%H:%M> Starting Notes %?"
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Journal"))
           :empty-lines-after 1 )
          ("t" "Tasks" entry "** TODO %? "
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Tasks"))
           :empty-lines-after 1 )
          ("r" "Rackspace" entry "** %<%H:%M> %?"
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Rackspace"))
           :empty-lines-after 1)
          ("j" "Journal" entry "** %<%H:%M> %?"
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Journal"))
           :empty-lines-after 1)))

  (setq org-roam-capture-templates
        '(("d" "default" plain
           (file "~/.config/doom/templates/roam-templates/default-capture-entry.org")
           :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n")
           :unnarrowed t)
          ("t" "tipjar" plain
           (file "~/.config/doom/templates/roam-templates/tipjar-entry.org")
           :if-new (file+head "TipJar/${slug}.org" "#+TITLE: ${title}\n#+filetags: tipjar\n")
           :unnarrowed t)
          )))

(after! org-roam
  (add-hook 'find-file-hook #'roam-extra:update-todo-tag)
  (add-hook 'before-save-hook #'roam-extra:update-todo-tag)
  (advice-add 'org-agenda :before #'roam-extra:update-todo-files))

;;;;; Org Roam
(after! org-roam
  (setq +org-roam-open-buffer-on-find-file nil)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .12 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .12 :height .5 :ttl nil :modeline nil :quit nil :slot 2))))

(use-package! org-transclusion
  :defer
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

;; TSfile Links

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
