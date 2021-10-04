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
  (setq org-icalendar-store-uid t)
  (setq org-id-track-globally t)

;;;;;; REFILE TARGETS

  ;; ;; (setq myroamfiles (directory-files "~/nextcloud/notes/org/" t "org$"))
  ;; (setq myroamdailiesfiles (directory-files "~/nextcloud/notes/org/daily/" t "org$"))

  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 5)
                             (myroamfiles :maxlevel . 5)
                             (myroamdailiesfiles :maxlevel . 2)))

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
                                         ("[x]"             . "☑")
                                         ("lambda"          . "λ")
                                         ("#+begin_example" . ">eg>")
                                         ("#+begin_src"     . "†")
                                         ("#+end_example"   . "<eg<")
                                         ("#+end_src"       . "†")
                                         ("#+begin_example" . ">eg>")
                                         ("#+begin_src"     . "†")
                                         ("#+end_example"   . "<eg<")
                                         ("#+end_src"       . "†")
                                         ("[ ]"             . "☐")
                                         ("[x]"             . "☑")
                                         ("[-]"             . "❍")
                                         ))

;;;;;; TAG LIST

  (setq org-tag-alist (quote
                       ((:startgroup)
                        ("@asits"     . ?a)
                        ("@billpay"   . ?b)
                        ("@redearth"  . ?d)
                        ("@email"     . ?e)
                        ("@jazney"    . ?j)
                        ("@outside"   . ?o)
                        ("@phonecall" . ?p)
                        ("@personal"  . ?p)
                        ("@rackspace" . ?r)
                        ("@reading"   . ?r)
                        ("@shopping"  . ?s)
                        ("@errand"    . ?e)
                        ("@home"      . ?h)
                        ("@inside"    . ?i)
                        ("@masons"    . ?m)
                        ("@music"     . ?m)
                        ("@office"    . ?o)
                        ("@system"    . ?x)
                        ("2637e20th")
                        (:endgroup)
                        ("cancelled"  . ?c)
                        ("draft"      . ?d)
                        ("flagged"    . ?f)
                        ("hold"       . ?h)
                        ("idea"       . ?i)
                        ("note"       . ?n)
                        ("project"    . ?p)
                        ("waiting"    . ?w)
                        ("work"       . ?w))))

;;;;;; CAPTURE TEMPLATES

  (setq org-capture-templates
        '(("t" "task" entry
           (file+olp "~/nextcloud/notes/org/0mobile.org" "inbox")
           (file "~/.config/doom/templates/todo.orgcaptmpl"))
          ("c" "contacts" entry (file-olp "~/nextcloud/notes/org/contacts.org" "general")
           (file "~/.config/doom/templates/contact.orgcaptmpl"))
          ("p" "protocol" entry
           (file+olp "~/nextcloud/notes/org/0mobile.org" "inbox" entry)
           "** %^{title}\nsource: %u, %c\n #+begin_quote\n%i\n#+end_quote\n\n\n%?")
          ("r" "remember-mutt" entry
           (file+olp "~/nextcloud/notes/org/0mobile.org" "mail")
           (file "~/.config/doom/templates/org-templates/mail.orgcaptmpl"))
          ("l" "protocol link" entry
           (file+olp "~/nextcloud/notes/org/0mobile.org" "inbox")
           "** %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
          ("w" "web site" entry
           (file+olp "~/nextcloud/notes/org/0mobile.org" "inbox")
           (file "~/.config/doom/templates/org-templates/weblink.orgcaptmpl"))
          ("s" "simple" entry
           (file+olp "~/nextcloud/notes/org/0mobile.org" "popup")
           "%[~/.emacs.d/.org-popup]" :immediate-finish t :prepend t)

          ("m" "email workflow")
          ("mf" "follow up" entry
           (file+olp "~/nextcloud/notes/org/0mobile.org" "follow up")
           "* todo follow up with %:fromname on %:subject\nscheduled:%t\n%a\n\n%i")
          ("ma" "auto follow up" entry
           (file+olp "~/nextcloud/notes/org/0mobile.org" "follow up")
           "* todo follow up with %:fromname on %:subject\n%a\n\n%i" :immediate-finish t)
          ("mf" "follow up with deadline" entry
           (file+olp "~/nextcloud/notes/org/0mobile.org" "follow up")
           "* todo follow up with %:fromname on %:subject\nscheduled:%t\ndeadline:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%a\n\n%i")
          ("mr" "read later" entry
           (file+olp "~/nextcloud/notes/org/0mobile.org" "read later")
           "* todo read  later on %:subject\nscheduled:%t\n%a\n\n%i":immediate-finish t)
          ("mm" "masons follow up" entry
           (file+olp "~/nextcloud/notes/org/masons.org" "follow up")
           "* todo follow up with %:fromname on %:subject %a\nscheduled:%t\n\\n%i")
          ("mr" "workflow rackspace")
          ("mrf" "follow up" entry
           (file+olp "~/nextcloud/notes/org/rackspace.org" "follow up")
           "* todo follow up with %:fromname on %:subject\nscheduled:%t\ndeadline:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%a\n\n%i")
          ("mrr" "read later" entry
           (file+olp "~/nextcloud/notes/org/rackspace.org" "read later")
           "* todo read  later with %:fromname on %:subject\nscheduled:%t\n%a\n\n%i" :immediate-finish t)
          ))

  (setq org-protocol-default-template-key "t")

;;;;;; TODO FACES
  (setq org-todo-keyword-faces
        '(("todo"       . org-warning)
          ("next"       . (:foreground "#008080" :weight bold))
          ("started"    . (:foreground "#e35dbf" :weight bold))
          ("blocked"    . (:foreground "white"   :weight bold))
          ("todelegate" . (:foreground "white"   :weight bold))
          ("delegated"  . (:foreground "pink"    :weight bold))
          ("canceled"   . (:foreground "white"   :weight bold))
          ("tickle"     . (:foreground "white"   :weight bold))
          ("done"       . (:foreground "green"   :weight bold))))

;;;;;; KEYWORDS
  (setq org-todo-keywords
        '((sequence "todo(t)"
                    "next(n!)"
                    "started(s!)"
                    "blocked(b@/!)"
                    "todelegate(g@/!)"
                    "delegated(d@/!)"
                    "followup(f@/!)"
                    "tickle(t!)"
                    "|"
                    "cancelled(c@)"
                    "done(d@)")))

  ) ;; end (after! org

;;;; ORG-ROAM
(after! org-roam
;;;;; ORG-ROAM CAPTURE TEMPLATES
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :if-new (file+olp "%<%y-%m-%d>.org" ("journal"))
           :empty-lines-after 1 )
          ("t" "tasks" entry "** todo %? "
           :if-new (file+olp "%<%y-%m-%d>.org" ("tasks"))
           :empty-lines-after 1 )
          ("r" "rackspace" entry "** %<%h:%m> %?"
           :if-new (file+olp "%<%y-%m-%d>.org" ("rackspace"))
           :empty-lines-after 1)
          ("j" "journal" entry "** %<%h:%m> %?"
           :if-new (file+olp "%<%y-%m-%d>.org" ("journal") )
           :empty-lines-after 1)))

  (setq org-roam-capture-templates
        '(("d" "default" plain
           (file "~/.config/doom/templates/roam-templates/default-capture-entry.org")
           :if-new (file+head "${slug}.org" "#+title: ${title}\n#+category: ${title}")
           :unnarrowed t)
          ("t" "tipjar" plain
           (file "~/.config/doom/templates/roam-templates/tipjar-entry.org")
           :if-new (file+head "tipjar/${slug}.org" "#+title: ${title}\n#+filetags: tipjar\n#+category: tipjar\n")
           :unnarrowed t)
          ("p" "people" plain
           (file "~/.config/doom/templates/roam-templates/people-entry.org")
           :if-new (file+head "people/${slug}.org" "#+title: ${title}\n#+category: people\n#+filetags: :people:\n")
           :unnarrowed t)))

;;;;; ORG-ROAM POPUP RULES
  (setq +org-roam-open-buffer-on-find-file nil)

  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .12 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .12 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
;;;;; ORG-ROAM FUNCTIONS
  (defun marty/add-other-auto-props-to-org-roam-properties ()
    ;; if the file already exists, don't do anything, otherwise...
    (unless (file-exists-p (buffer-file-name))
      ;; if there's also a creation_time property, don't modify it
      (unless (org-find-property "creation_time")
        ;; otherwise, add a unix epoch timestamp for creation_time prop
        ;; (this is what "%s" does - see http://doc.endlessparentheses.com/fun/format-time-string )
        (org-roam-add-property
         (format-time-string "%s"
                             (nth 5
                                  (file-attributes (buffer-file-name))))
         "creation_time"))
      (unless (org-find-property "org_creation_time")
        (org-roam-add-property
         (format-time-string "[%y-%m-%d %a %h:%m:%s]"
                             (nth 5
                                  (file-attributes (buffer-file-name))))
         "org_creation_time"))
      ;; similarly for author and mail properties
      (unless (org-find-property "author")
        (org-roam-add-property user-full-name "author"))
      (unless (org-find-property "mail")
        (org-roam-add-property user-mail-address "mail"))
      ;; also add the latitude and longitude
      (unless (org-find-property "lat_long")
        ;; recheck location:
        (marty/get-lat-long-from-ipinfo)
        (org-roam-add-property (concat (number-to-string calendar-latitude) "," (number-to-string calendar-longitude)) "lat-long"))))

;;;;; ORG-ROAM HOOKS
  (add-hook 'find-file-hook #'roam-extra:update-todo-tag)
  (add-hook 'before-save-hook #'roam-extra:update-todo-tag)
  (advice-add 'org-agenda :before #'roam-extra:update-todo-files)

  ;; hook to be run whenever an org-roam capture completes
  (add-hook 'org-roam-capture-new-node-hook #'marty/add-other-auto-props-to-org-roam-properties)

;;;;; FUNCTIONS
  (defun marty/org-roam-dailies-graphicslink ()
    " set the graphics link to today in the pictures folder that maid pushes to."
    (interactive)
    (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
           (month (string-to-number (substring (buffer-name) 5 7)))
           (day   (string-to-number (substring (buffer-name) 8 10)))
           (datim (encode-time 0 0 0 day month year)))
      (format-time-string "[[/home/marty/nextcloud/pictures/2020 - 2029/%y/%0m/daily/%d][graphics link]]" datim)))

  (defun marty/org-roam-dailies-title ()
    (interactive)
    (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
           (month (string-to-number (substring (buffer-name) 5 7)))
           (day   (string-to-number (substring (buffer-name) 8 10)))
           (datim (encode-time 0 0 0 day month year)))
      (format-time-string "%a, %b %d %y" datim)))

  (defun marty/org-roam-dailies-todo-schedule ()
    " set the date for the todo's in the dailies template "
    (interactive)
    (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
           (month (string-to-number (substring (buffer-name) 5 7)))
           (day   (string-to-number (substring (buffer-name) 8 10)))
           (datim (encode-time 0 0 0 day month year)))
      (format-time-string "scheduled: [%y-%m-%d %a 10:00]" datim)))

  (defun marty/org-roam-dailies-todo-deadline ()
    " set the date for the todo's in the dailies template "
    (interactive)
    (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
           (month (string-to-number (substring (buffer-name) 5 7)))
           (day   (string-to-number (substring (buffer-name) 8 10)))
           (datim (encode-time 0 0 0 day month year)))
      (format-time-string "deadline: [%y-%m-%d %a 20:00]" datim)))

  ;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/

  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "p")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))


  (defun marty/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                       :templates '(("i" "inbox" plain "** %?"
                                     :if-new (file+olp "~/nextcloud/notes/org/0mobile.org" ("inbox"))))))

  ;; move todo's to dailies when done
  (defun marty/org-roam-move-todo-to-today ()
    (interactive)
    (let ((org-refile-keep nil) ;; set this to t to copy the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+olp "%<%y-%m-%d>.org" ("tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "tasks" today-file nil pos)))))


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
    "return non-nil if current buffer has any todo entry.

todo entries marked as done are ignored, meaning the this
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
    "update todo tag in the current buffer."
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
    "return a list of roam files containing todo tag."
    (org-roam-db-sync)
    (let ((todo-nodes (seq-filter (lambda (n)
                                    (seq-contains-p (org-roam-node-tags n) "todo"))
                                  (org-roam-node-list))))
      (seq-uniq (seq-map #'org-roam-node-file todo-nodes))))

  (defun roam-extra:update-todo-files (&rest _)
    "update the value of `org-agenda-files'."
    (setq org-agenda-files (roam-extra:todo-files)))

;;;;; ORG-ROAM END
  )
;;;;; ORG-ROAM-MODULES
;;;;;; ORG-ROAM-UI
(use-package! org-roam-ui
  :after org-roam)
;;;;;; ORG-ROAM-TIMESTAMPS
(use-package! org-roam-timestamps
  :after org-roam
  :config (org-roam-timestamps-mode))

;;;; TSFILE LINKS

(after! org
  (defvar memacs-root "~/nextcloud/notes/memacs/")
  (defvar memacs-file-pattern "files.org")

  (with-eval-after-load 'org
    (org-link-set-parameters
     "tsfile"
     :follow (lambda (path) (my-handle-tsfile-link path))
     :help-echo "opens the linked file with your default application"))

  ;; by john kitchin
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
        (message "sorry, no results found for query: %s" querystring))
       (t
        (with-temp-buffer
          (insert (if (= 1 (length queryresults))
                      (car queryresults)
                    (completing-read "choose: " queryresults)))
          (org-mode)
          (goto-char (point-min))
          (org-next-link)
          (org-open-at-point "file:"))))))

  (defun marty/dired-copy-filename-as-tsfile-link ()
    "copy current file name with its basename as [[tsfile:<basename>]] custom org-mode link."
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
        '(("o" "overview"
           ((agenda "" ((org-super-agenda-groups
                         '((:log t)  ; automatically named "log"
                           (:name "schedule"
                            :time-grid t)
                           (:name "today"
                            :scheduled today)
                           (:habit t)
                           (:name "due today"
                            :deadline today)
                           (:name "overdue"
                            :deadline past)
                           (:name "due soon"
                            :deadline future)
                           (:name "tickle"
                            :deadline future)
                           (:name "unimportant"
                            :todo ("blocked" "todelegate" "delegated" "canceled"
                                   :order 100)
                            (:name "waiting..."
                             :todo "waiting"
                             :order 98)
                            (:name "scheduled earlier"
                             :scheduled past))))))))
          ("g" "group"
           ((agenda "" ((org-agenda-spam 'week)
                        (org-super-agenda-groups
                         '((:auto-category t))
                         )))))

          ("u" "super view"
           ((agenda "" ((org-super-agenda-groups
                         '((:name "today"
                            :time-grid t)))))
            (todo "" ((org-agenda-overriding-header "projects")
                      (org-super-agenda-groups
                       '((:name none  ; disable super group header
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
   :desc "org transclusion mode" "t" #'org-transclusion-mode))



;;;;; FUNCTION TO FIND LATITUDE & LONGITUDE
;;  (requires curl to be installed on system)
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
