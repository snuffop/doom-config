;;; org-mode.el --- ORG-MODE -*- lexical-binding: t; -*-

;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2021, marty buchaus, all rights reserved.
;; created:  1 November 2021
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code
;;;; Pre
(setq org-directory "~/Nextcloud/Notes/org/")
(setq org-roam-directory "~/Nextcloud/Notes/org/")
(setq org-roam-dailies/directory "daily/")
(setq org-contacts-files '("~/Nextcloud/Notes/org/contacts.org"))

(setq  marty/org-agenda-files (list
                               (concat org-directory "Tasks.org")
                               (concat org-directory "Habits.org")
                               (concat org-directory "Calendar.org")
                               (concat org-directory "contacts.org")
                               (concat org-directory "Someday.org")
                               (concat org-directory "0mobile.org")
                               "~/Nextcloud/Notes/Calendars/google.org"
                               "~/Nextcloud/Notes/Calendars/tatjana.org"))
;;;; ORG-MODE
;;;;; PACKAGE
(after! org

;;;;; PUBLISH ALIST

  (defun marty/publish (a b c)
    (setq org-export-with-toc t)
    (org-html-publish-to-html a b c))

  ;;(require 'find-lisp)

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

          ("NSI-Documentation" :components ("NSI-Documentation-content"
                                            "NSI-Documentation-images"
                                            "NSI-Documentation-TVA-ScanReports-2020-images"
                                            "NSI-Documentation-TVA-ScanReports-2020-reports"
                                            "NSI-Documentation-TVA-ScanReports-2021-images"
                                            "NSI-Documentation-TVA-ScanReports-2021-reports"
                                            "NSI-Documentation-TVA-ScanReports-files"))))
;;;;; ORG-AGENDA


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
          (org-projectile-todo-files))


;;;;; BASE

  (setq org-default-notes-file (concat org-directory "0mobile.org"))
  (setq org-download-image-dir "~/Nextcloud/Notes/images/")
  (setq org-id-locations-file "~/Nextcloud/Notes/org-id-locations")
  (setq org-persp-startup-org-file "~/Nextcloud/Notes/org/0mobile.org")
  (setq org-projectile-file "todo.org")
  (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴"))
  (setq org-startup-with-inline-images t)  ; Show Inline Images

;;;;; CLOCKING

  ;; (setq org-clock-into-drawer "CLOCKING")
  ;; Where to put the clock in and out for tracked items
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-sound "~/Nextcloud/Music/sounds/shipsBell.wav")

;;;;; LOGGING AND ID

  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-icalendar-store-UID t)
  (setq org-id-track-globally t)

;;;;; REFILE TARGETS

  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 5)))

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)



;;;;; ORG-MODE-HOOK

  (add-hook! 'org-mode-hook (+org-mode-setup) )

  (defun +org-mode-setup ()
    (auto-fill-mode -1)
    (flycheck-mode -1)
    (display-line-numbers-mode -1)
    (hl-line-mode -1)
    (auto-revert-mode 1)
    (visual-line-mode)
    (variable-pitch-mode 1)

    (setq prettify-symbols-unprettify-at-point 'right-edge)

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
    (push '(":CREATION_TIME:"      . ""  ) prettify-symbols-alist)
    (push '(":ID:"                 . ""  ) prettify-symbols-alist)
    (push '(":LAT-LONG:"           . ""  ) prettify-symbols-alist)
    (push '(":MAIL:"               . ""  ) prettify-symbols-alist)
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
    (prettify-symbols-mode))



;;;;; TAG LIST
  (setq org-tag-alist (quote
                       ((:startgroup)
                        ("@ASITS"     . ?A)
                        ("@BillPay"   . ?B)
                        ("@RedEarth"  . ?D)
                        ("@Email"     . ?E)
                        ("@Joyent"    . ?J)
                        ("@Jazney"    . ?j)
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

;;;;; FACES

  (setq org-todo-keyword-faces
        '(("TODO"       . (:foreground "red"     :weight bold))
          ("NEXT"       . (:foreground "#008080" :weight bold))
          ("STARTED"    . (:foreground "#E35DBF" :weight bold))
          ("BLOCKED"    . (:foreground "white"   :weight bold))
          ("TODELEGATE" . (:foreground "white"   :weight bold))
          ("DELEGATED"  . (:foreground "pink"    :weight bold))
          ("CANCELED"   . (:foreground "white"   :weight bold))
          ("TICKLE"     . (:foreground "white"   :weight bold))
          ("DONE"       . (:foreground "green"   :weight bold))))

  (custom-set-faces
   '(org-document-title ((t (:inherit outline-1 :height 1.5))))
   '(org-level-1 ((t (:inherit outline-1 :height 1.12))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   )

  (cond (IS-LINUX (set-face-attribute 'variable-pitch nil
                                       :font "Ubuntu"
                                       :weight 'regular
                                       :height 100)

                  (set-face-attribute 'fixed-pitch nil
                                       :font "Droid Sans Mono"
                                       :weight 'regular
                                       :height 100)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-tag nil
                      :foreground nil
                      :inherit '(shadow fixed-pitch)
                      :weight 'bold)

  (set-face-attribute 'org-block nil
                      :inherit 'fixed-pitch)

  (set-face-attribute 'org-code nil
                      :inherit '(shadow fixed-pitch))

  (set-face-attribute 'org-table nil
                      :foreground "#83a598"
                      :inherit '(shadow fixed-pitch))

  (set-face-attribute 'org-verbatim nil
                      :inherit '(shadow fixed-pitch))

  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))

  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))

  (set-face-attribute 'org-checkbox nil
                      :inherit 'fixed-pitch)

  (set-face-attribute 'org-link nil
                      :foreground "royal blue"
                      :underline t)

  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-property-value nil
                      :inherit 'fixed-pitch)

  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))

  (set-face-attribute 'org-tag nil
                      :inherit '(shadow fixed-pitch)
                      :weight 'bold
                      :height 10)

  (set-face-attribute 'org-verbatim nil
                      :inherit '(shadow fixed-pitch))


;;;;; KEYWORDS

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

;;;;; CAPTURE TEMPLATES Using DOCT

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

  (setq org-protocol-default-template-key "t")

;;;;; MAIL/MUTT
  (org-add-link-type "message" 'mutt-open-message)

;;;;; TSFILE LINKS

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
      (kill-new (concat "[[tsfile:" filename "]]"))))
;;;;; FUNCTIONS
;;;;;; LONG-LAT
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

;;;;;; FORMAT ORG-BLOCK
  (defun format-org-mode-block ()
    "Format org mode code block"
    (interactive "p")
    ;; (execute-kbd-macro (kbd "C-c ' C-x h C-M-\\ C-c '"))
    ;; (execute-kbd-macro (read-kbd-macro "C-c ' C-x h C-M-\\ C-c '"))
    (org-edit-special)
    (format-all-ensure-formatter)
    (format-all-buffer)
    (org-edit-src-exit))

;;;;;; PRETTIFY FUNCTIONS FROM TECOSAUR
  ;; for pretty capture interfaces..
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



;;;;;; Expand org file name
;;;;###autoload (autoload '+org/expand-org-file-name )
  (defun +org/expand-org-file-name (x)
    "Expand file name X with org-directory."
    (if (eq (type-of x) 'cons)
        (-map #'+org/expand-org-file-name x)
      (expand-file-name x org-directory)))

;;;;;; Find in files
;;;;###autoload
  (defun +org/find-in-files (file)
    "Find file in org directory."
    (->> (+org/expand-org-file-name file)
         (find-file)))

;;;;;; Timestamp
  (defun +org/active-timestamp (&optional str)
    (let* ((str (or str ""))
           (default-time (org-current-time))
           (decoded-time (decode-time default-time nil))
           (analyzed-time (org-read-date-analyze str default-time decoded-time))
           (encoded-time (apply #'encode-time analyzed-time)))
      (format-time-string (org-time-stamp-format t) encoded-time)))

  (defun +org/inactive-timestamp (&optional str)
    (let* ((str (or str ""))
           (default-time (org-current-time))
           (decoded-time (decode-time default-time nil))
           (analyzed-time (org-read-date-analyze str default-time decoded-time))
           (encoded-time (apply #'encode-time analyzed-time)))
      (format-time-string (org-time-stamp-format t t) encoded-time)))
;;;;; REFILE

  (setq +org:level-1-refile-targets
        (+org/expand-org-file-name
         '("0mobile.org"
           "Bookmark.org"
           "desktop.org"
           "Media.org"
           "Someday.org"
           "Tasks.org"
           "Rackspace.org"
           "joyent.org")))

  (setq +org:max-level-2-refile-targets
        (+org/expand-org-file-name
         '("read-later.org"
           "red team.org"
           "linode.org")))

  (setq max-level-3-refile-targets
        (+org/expand-org-file-name
         '("TipJar/Emacs/emacs.org")))

  (defun +org:level-1-refile-targets () +org:level-1-refile-targets)
  (defun +org:max-level-2-refile-targets () +org:max-level-2-refile-targets)
  (defun max-level-3-refile-targets () max-level-3-refile-targets)

  (setq org-refile-targets (quote ((nil :maxlevel . 5)
                                   (+org:max-level-2-refile-targets :maxlevel . 2)
                                   (max-level-3-refile-targets :maxlevel . 3)
                                   (+org:level-1-refile-targets :level . 1))))
  (setq org-agenda-refile org-agenda-files)

;;;;; END (progn org)
  )

;;;; DOCT
(use-package! doct
  :after org
  :commands (doct))

;;;; ORG-ROAM
;;;;; PACKAGE
(after! org-roam
  (setq org-roam-mode-selections
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section
              #'org-roam-unlinked-references-insert-section))

;;;;;; ORG-ROAM POPUP RULES

  (setq +org-roam-open-buffer-on-find-file nil)

  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .12 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .12 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))

;;;;;; ORG-ROAM HOOKS

  ;; hook to be run whenever an org-roam capture completes
  (add-hook 'org-roam-capture-new-node-hook #'marty/add-other-auto-props-to-org-roam-properties)

;;;;;; ORG-ROAM CAPTURE TEMPLATES

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Journal"))
           :empty-lines-after 1 )
          ("t" "Tasks" entry "** TODO %? "
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Tasks"))
           :empty-lines-after 1 )
          ("y" "Joyent" entry "** %<%H:%M> %?"
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Joyent"))
           :empty-lines-after 1)
          ("j" "Journal" entry "** %<%H:%M> %?"
           :if-new (file+olp "%<%Y-%m-%d>.org" ("Journal"))
           :empty-lines-after 1)))

  (setq org-roam-capture-templates
        '(("d" "default" plain
           (file "~/.config/doom/templates/roam-templates/default-capture-entry.org")
           :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n#+category: ${title}")
           :immediate-finish t
           :unnarrowed t)
          ("j" "Joyent" plain
           (file "~/.config/doom/templates/roam-templates/joyent-entry.org")
           :if-new (file+head "Joyent/${slug}.org" "#+TITLE: ${title}\n#+filetags: Joyent\n#+category: Joyent\n")
           :unnarrowed t)
          ("t" "tipjar" plain
           (file "~/.config/doom/templates/roam-templates/tipjar-entry.org")
           :if-new (file+head "TipJar/${slug}.org" "#+TITLE: ${title}\n#+filetags: tipjar\n#+category: tipjar\n")
           :unnarrowed t)
          ("p" "People" plain
           (file "~/.config/doom/templates/roam-templates/people-entry.org")
           :if-new (file+head "People/${slug}.org" "#+TITLE: ${title}\n#+category: people\n#+filetags: :people:\n")
           :unnarrowed t)))

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

;;;;;; END Package
  )

;;;; ORG-ROAM-TIMESTAMPS

(use-package! org-roam-timestamps
  :after org-roam
  :config
  (setq org-roam-timestamps-parent-file t)  (org-roam-timestamps-mode))
