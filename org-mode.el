;;; org-mode.el --- summary -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2021, marty buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; org-mode configuration
;;

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
                                 (concat org-directory "0mobile.org")))

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

  (defun marty/publish (a b c)
    (setq org-export-with-toc t)
    (org-html-publish-to-html a b c))

  (require 'find-lisp)
  (defun marty/publish-NSI-Documentation (a b c)
    (setq org-export-with-toc t)
    (let ((org-id-extra-files (find-lisp-find-files "~/Source/NSI/NSI-Documentation/" "\.org$")))
      (org-html-publish-to-html a b c)))

  (setq org-publish-project-alist
        '(
          ("NSI-Documentation-content"
           :base-directory "~/Source/NSI/NSI-Documentation/"
           :base-extension "org"
           :publishing-directory "~/Source/NSI/NSI-Documentation/docs"
           :publishing-function marty/publish-NSI-Documentation
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
  (setq org-startup-with-inline-images t)  ; Show Inline Images

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

  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 5)))

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)


;;;;;; SYMBOLS

  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org Checkbox Symbol"
                             (push '("#+ACTIVE:"            . ""  ) prettify-symbols-alist)
                             (push '("#+BEGIN_EXAMPLE"      . "↦"  ) prettify-symbols-alist)
                             (push '("#+BEGIN_HIDDEN"       . ""  ) prettify-symbols-alist)
                             (push '("#+BEGIN_QUOTE"        . "↦"  ) prettify-symbols-alist)
                             (push '("#+BEGIN_SRC"          . "↦"  ) prettify-symbols-alist)
                             (push '("#+CATEGORY:"          . "⛏ "  ) prettify-symbols-alist)
                             (push '("#+CLOSE_SPOILER"      . ""  ) prettify-symbols-alist)
                             (push '("#+END_EXAMPLE"        . "⇤"  ) prettify-symbols-alist)
                             (push '("#+END_HIDDEN"         . ""  ) prettify-symbols-alist)
                             (push '("#+END_QUOTE"          . "⇤"  ) prettify-symbols-alist)
                             (push '("#+END_SRC"            . "⇤"  ) prettify-symbols-alist)
                             (push '("#+FILETAGS:"          . ""  ) prettify-symbols-alist)
                             (push '("#+ID:"                . ""  ) prettify-symbols-alist)
                             (push '("#+STARTUP:"           . "🌟"  ) prettify-symbols-alist)
                             (push '("#+START_SPOILER"      . ""  ) prettify-symbols-alist)
                             (push '("#+TITLE:"             . ""  ) prettify-symbols-alist)
                             (push '("#+begin_example"      . "↦"  ) prettify-symbols-alist)
                             (push '("#+begin_quote"        . "❝"  ) prettify-symbols-alist)
                             (push '("#+begin_src"          . "↦"  ) prettify-symbols-alist)
                             (push '("#+category:"          . "⛏ "  ) prettify-symbols-alist)
                             (push '("#+end_example"        . "⇤"  ) prettify-symbols-alist)
                             (push '("#+end_quote"          . "❞"  ) prettify-symbols-alist)
                             (push '("#+end_src"            . "⇤"  ) prettify-symbols-alist)
                             (push '("#+filetags:"          . ""  ) prettify-symbols-alist)
                             (push '("#+startup:"           . "⏻"  ) prettify-symbols-alist)
                             (push '("#+title:"             . ""  ) prettify-symbols-alist)
                             (push '("---"                  . "—"  ) prettify-symbols-alist)
                             (push '("->"                   . "→"  ) prettify-symbols-alist)
                             (push '("..."                  . "…"  ) prettify-symbols-alist)
                             (push '("::"                   . "∷"  ) prettify-symbols-alist)
                             (push '(":attr_html"           . "🄗"  ) prettify-symbols-alist)
                             (push '(":attr_latex"          . "🄛"  ) prettify-symbols-alist)
                             (push '(":attr_org"            . "⒪"  ) prettify-symbols-alist)
                             (push '(":author"              . "𝘼"  ) prettify-symbols-alist)
                             (push '(":beamer_header"       . "🅑"  ) prettify-symbols-alist)
                             (push '(":begin_export"        . "⏩"  ) prettify-symbols-alist)
                             (push '(":caption"             . "☰"  ) prettify-symbols-alist)
                             (push '(":date"                . "𝘿"  ) prettify-symbols-alist)
                             (push '(":end"                 . "∎" ) prettify-symbols-alist)
                             (push '(":end_export"          . "⏪"  ) prettify-symbols-alist)
                             (push '(":header"              . "›"  ) prettify-symbols-alist)
                             (push '(":html"                . "🅗"  ) prettify-symbols-alist)
                             (push '(":html_head"           . "🅷" ) prettify-symbols-alist)
                             (push '(":latex"               . "🅛" ) prettify-symbols-alist)
                             (push '(":latex_class"         . "🄻" ) prettify-symbols-alist)
                             (push '(":latex_header"        . "🅻" ) prettify-symbols-alist)
                             (push '(":macro"               . "𝓜" ) prettify-symbols-alist)
                             (push '(":options"             . "⌥" ) prettify-symbols-alist)
                             (push '(":results"             . "🠶" ) prettify-symbols-alist)
                             (push '("<-"                   . "←" ) prettify-symbols-alist)
                             (push '("[ ]"                  . "☐"  ) prettify-symbols-alist)
                             (push '("[#A]"                 . "⚑"  ) prettify-symbols-alist)
                             (push '("[#B]"                 . "⬆"  ) prettify-symbols-alist)
                             (push '("[#C]"                 . "■"  ) prettify-symbols-alist)
                             (push '("[#D]"                 . "⬇"  ) prettify-symbols-alist)
                             (push '("[#E]"                 . "❓"  ) prettify-symbols-alist)
                             (push '("[-]"                  . "◼" ) prettify-symbols-alist)
                             (push '("[X]"                  . "☑" ) prettify-symbols-alist)
                             (push '("lambda"               . "λ"  ) prettify-symbols-alist)
                             (push '("subtitle"             . "𝙩" ) prettify-symbols-alist)
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

;;;;;; FACES

  (custom-set-faces
   '(org-document-title ((t (:inherit outline-1 :height 1.5))))
   '(org-level-1 ((t (:inherit outline-1 :height 1.12))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   )

  (add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-tag nil :foreground nil :inherit '(shadow fixed-pitch) :weight 'bold)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

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

(use-package! org-roam
  :after org
  :config
  (setq org-roam-mode-selections
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section
              #'org-roam-unlinked-references-insert-section))

;;;;; ORG-ROAM POPUP RULES

  (setq +org-roam-open-buffer-on-find-file nil)

  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .12 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .12 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))

;;;;; ORG-ROAM HOOKS

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

;;   (defun my/org-roam-filter-by-tag (tag-name)
;;     (lambda (node)
;;       (member tag-name (org-roam-node-tags node))))

;;   (defun my/org-roam-list-notes-by-tag (tag-name)
;;     (mapcar #'org-roam-node-file
;;             (seq-filter
;;              (my/org-roam-filter-by-tag tag-name)
;;              (org-roam-node-list))))

;;   (defun dw/org-roam-goto-month ()
;;     (interactive)
;;     (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
;;                        :node (org-roam-node-create)
;;                        :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
;;                                      :if-new (file+head "%<%Y-%B>.org"
;;                                                         "#+title: %<%Y-%B>\n#+filetags: Project\n")
;;                                      :unnarrowed t))))

;;   (defun dw/org-roam-goto-year ()
;;     (interactive)
;;     (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
;;                        :node (org-roam-node-create)
;;                        :templates '(("y" "year" plain "\n* Goals\n\n%?* Summary\n\n"
;;                                      :if-new (file+head "%<%Y>.org"
;;                                                         "#+title: %<%Y>\n#+filetags: Project\n")
;;                                      :unnarrowed t))))

;;   (defun my/org-roam-refresh-agenda-list ()
;;     (interactive)
;;     (setq org-agenda-files (my/org-roam-list-notes-by-tag "todo")))


;; ;;;;;; CAPTURE INBOX
;;   (defun marty/org-roam-capture-inbox ()
;;     (interactive)
;;     (org-roam-capture- :node (org-roam-node-create)
;;                        :templates '(("i" "Inbox" plain "** %?"
;;                                      :if-new (file+olp "~/Nextcloud/Notes/org/0mobile.org" ("Inbox"))))))

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

;;;;;; HOTTER BUFFER

  (defun org-roam-buffer-setup ()
    "Function to make org-roam-buffer more pretty."
    (progn
      (setq-local olivetti-body-width 44)
      (variable-pitch-mode 1)
      (olivetti-mode 1)
      ;; (centaur-tabs-local-mode -1)

      (set-face-background 'magit-section-highlight (face-background 'default))))

  (add-hook! 'org-roam-mode-hook #'org-roam-buffer-setup)

;;;;; ORG-ROAM END

  (org-roam-setup))

;;;;; ORG-ROAM-MODULES
;;;;;; ROAM-BIBTEX

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "file" "author-or-editor" "keywords" "pdf" "doi" "author" "tags" "year" "author-bbrev")))

;;;;;; ORG-ROAM-UI

(use-package! org-roam-ui
  :after org-roam)

;;;;;; ORG-ROAM-TIMESTAMPS

(use-package! org-roam-timestamps
  :after org-roam
  :config
  (setq org-roam-timestamps-parent-file t)
  (setq org-roam-timestamps-remember-timestamps t)
  (org-roam-timestamps-mode))

;;;; ORG-MODE MODULES
;;;;; DOCT

(use-package! doct
  :defer t
  :after org
  :commands (doct))

;;;;; ORG-APPEAR

(use-package! org-appear
  :after org
  ;;:hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

;;;;; ORG-EDNA-MODE

(after! org
  (org-edna-mode))

;;;;; ORG-JIRA

(use-package! org-jira
  :defer 10
  :init
  (setq jiralib-url "https://rackspace.atlassian.net")
  (setq org-jira-working-dir "~/Nextcloud/Notes/org-jira")
  (setq org-jira-custom-jqls
        '(
          (:jql " project IN (NSYS) and createdDate < '2020-01-01' order by created DESC "
           :limit 10
           :filename "last-years-work")
          (:jql " project IN (NSYS) and createdDate >= '2021-01-01' order by created DESC "
           :limit 10
           :filename "this-years-work")
          (:jql " project IN (NSYS) and status IN ('To Do', 'In Development') AND (labels = EMPTY or labels NOT IN ('FutureUpdate')) order by priority, created DESC "
           :limit 20
           :filename "nsys-priority-items"))))

;;;;; ORG-NOTER

(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   ;;org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   ;;org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the rclone mega
   org-noter-notes-search-path "~/Nextcloud/Notes/org/Noter"

   ))


(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;;;;; CITATIONS

(use-package! org-ref
  ;;:after org-roam
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   bibtex-completion-bibliography (list "~/Nextcloud/Notes/library.bib")
   bibtex-completion-notes "~/Nextcloud/Notes/org/bibnotes.org"
   org-ref-note-title-format "* %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory "~/Nextcloud/Notes/org/Noter"
   org-ref-notes-function 'orb-edit-notes
   ))

(after! org-ref
  (setq
   bibtex-completion-notes-path "~/Nextcloud/Notes/org/Noter/"
   bibtex-completion-bibliography "~/Nextcloud/Notes/library.bib"
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "* TODO Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n")))

;;;;; ORG-NOTIFICATIONS

(use-package! org-notifications
  :init (org-notifications-start))

;;;;; TSFILE LINKS

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


;;;;; ORG-OL-TREE

      (use-package! org-ol-tree
        :commands org-ol-tree)

(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree)

;;;;; ORG-PANDOC

(use-package! org-pandoc-import
  :after org)

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



;;;; FUNCTIONS
;;;;; LONG-LAT
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

;;;;; FORMAT ORG-BLOCK
(defun format-org-mode-block ()
  "Format org mode code block"
  (interactive "p")
  ;; (execute-kbd-macro (kbd "C-c ' C-x h C-M-\\ C-c '"))
  ;; (execute-kbd-macro (read-kbd-macro "C-c ' C-x h C-M-\\ C-c '"))
  (org-edit-special)
  (format-all-ensure-formatter)
  (format-all-buffer)
  (org-edit-src-exit))

;;;;; PRETTIFY FUNCTIONS FROM TECOSAUR
;; for pretty capture interfaces..
(after! org
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys prompt nil)))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ((equal pressed "ESC") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  (advice-add 'org-mks :override #'org-mks-pretty)

  ;; (((())))

  (setf (alist-get 'height +org-capture-frame-parameters) 15)
  ;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
  (setq +org-capture-fn
        (lambda ()
          (interactive)
          (set-window-parameter nil 'mode-line-format 'none)
          (org-capture)))


  ;; Sprinkle some doct

  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  )

;;;; CAPTURE TEMPLATES Using DOCT
(after! org-capture
  (setq org-capture-templates
        (doct `(("Task" :keys "t"
                 :icon ("tag" :set "octicon" :color "cyan")
                 :file "~/Nextcloud/Notes/org/0mobile.org"
                 :prepend t
                 :headline "Inbox"
                 :template-file "~/.config/doom/templates/org-templates/todo.org")

                ("Contact"
                 :keys "c"
                 :icon ("male" :set "faicon" :color "yellow")
                 :file "~/Nextcloud/Notes/org/contacts.org"
                 :headline "General"
                 :template-file "~/.config/doom/templates/org-templates/contact.org")

                ("Remember-mutt" :keys "R"
                 :icon ("sticky-note" :set "faicon" :color "yellow")
                 :icon ("home" :set "octicon" :color "cyan")
                 :file "~/Nextcloud/Notes/org/0mobile.org"
                 :headline "Mail"
                 :template-file "~/.config/doom/templates/org-templates/mail.org")

                ("Protocol" :keys "P"
                 :file "~/Nextcloud/Notes/org/0mobile.org"
                 :icon ("tag" :set "octicon" :color "cyan")
                 :headline "Inbox"
                 :children (("Read"
                             :keys "r"
                             :headline "Read Later"
                             :immediate-finish t
                             :template-file "~/.config/doom/templates/org-templates/protocol-read-later.org")
                            ("Today"
                             :keys "t"
                             :template-file "~/.config/doom/templates/org-templates/protocol-today.org")
                            ("Important"
                             :keys "i"
                             :template-file "~/.config/doom/templates/org-templates/protocol-important.org")))

                ("Email Workflow"
                 :keys "m"
                 :icon ("mail" :set "octicon" :color "yellow")
                 :file "~/Nextcloud/Notes/org/0mobile.org"
                 :children (("Follow Up"
                             :keys "f"
                             :headline "Follow Up"
                             :template ("* TODO Follow up with %:fromname on %:subject"
                                        "SCHEDULED:%t"
                                        "%a"
                                        "%i"))
                            ("Auto Follow Up"
                             :keys "a"
                             :immediate-finish t
                             :headline "Follow Up"
                             :template ("* TODO Follow up with %:fromname on %:subject"
                                        "%a"

                                        "%i"))
                            ("Follow Up With Deadline"
                             :keys "F"
                             :headline "Follow Up"
                             :template ("* TODO Follow up with %:fromname on %:subject"
                                        "SCHEDULED:%t"
                                        "DEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))"
                                        "%a"
                                        "%i"))
                            ("Read Later"
                             :keys "r"
                             :headline "Read Later"
                             :immediate-finish t
                             :tetmplate ("* TODO Read Later on %:subject"
                                         "SCHEDULED:%t"
                                         "%a"
                                         "%i")
                             ))))))

  (setq org-protocol-default-template-key "t"))

;;;; ORG-ROAM CAPTURE TEMPLATES
(after! org-roam
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
           :immediate-finish t
           :unnarrowed t)
          ("t" "tipjar" plain
           (file "~/.config/doom/templates/roam-templates/tipjar-entry.org")
           :if-new (file+head "TipJar/${slug}.org" "#+TITLE: ${title}\n#+filetags: tipjar\n#+category: tipjar\n")
           :unnarrowed t)
          ("p" "People" plain
           (file "~/.config/doom/templates/roam-templates/people-entry.org")
           :if-new (file+head "People/${slug}.org" "#+TITLE: ${title}\n#+category: people\n#+filetags: :people:\n")
           :unnarrowed t))))
