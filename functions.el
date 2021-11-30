;;; functions.el --- summary -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2021, marty buchaus, all rights reserved.
;; created:  7 july 2021
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; functions


;;;; CALENDAR OPEN

(defun mb/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")
    (cfw:org-create-file-source "Personal" "~/Nextcloud/Notes/org/Calendar.org" "Blue"))))

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

;;;; OPEN MUTT MESSAGE

(defun mutt-open-message (message-id)
  "In neomutt, open the nmail with the the given Message-ID"
  (let*
      ((message-id
        (replace-regexp-in-string "^/*" "" message-id))
       (mail-file
        ;; notmuch
        (replace-regexp-in-string
         "\n$" "" (shell-command-to-string
                   (format "notmuch search --output=files id:%s" message-id))))
       (mail-box (replace-regexp-in-string "/home/marty/Mail" "" mail-file))
       (mail-dir (replace-regexp-in-string "/\\(cur\\|new\\|tmp\\)/$" ""
                                           (file-name-directory mail-box)))
       (mutt-keystrokes
        (format "macro index - l~i%s; push -\\nb\\n" (shell-quote-argument message-id)))
       (mutt-command (format "neomutt -f '=%s' -e '%s'" mail-dir  mutt-keystrokes)))
    ;; MU
    ;;         (replace-regexp-in-string
    ;;          "\n$" "" (shell-command-to-string
    ;;                    (format "mu find -u i:%s --fields 'm'" message-id ))))
    ;;        (mutt-keystrokes
    ;;         (format "macro index - l~i%s; push -\\nb\\n" (shell-quote-argument message-id)))
    ;;         (mutt-command (format "neomutt -f '=%s' -e '%s'" mail-file  mutt-keystrokes)))

    (message "Launching neomutt for message %s" message-id)
    (message " %s" mutt-command)
    (call-process "setsid" nil nil nil
                  "-f" "termite" "-e"
                  mutt-command)))

;;;; PROT FUNCTIONS

(defvar prot-common-url-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
   "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
         (punct "!?:;.,"))
     (concat
      "\\(?:"
      ;; Match paired parentheses, e.g. in Wikipedia URLs:
      ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com
      "[" chars punct "]+" "(" "[" chars punct "]+" ")"
      "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
      "\\|"
      "[" chars punct "]+" "[" chars "]"
      "\\)"))
   "\\)")
  "Regular expression that matches URLs.
Copy of variable `browse-url-button-regexp'.")


(defun prot-diff-buffer-dwim (&optional arg)
  "Diff buffer with its file's last saved state, or run `vc-diff'.
With optional prefix ARG (\\[universal-argument]) enable
highlighting of word-wise changes (local to the current buffer)."
  (interactive "P")
  (let ((buf))
    (if (buffer-modified-p)
        (progn
          (diff-buffer-with-file (current-buffer))
          (setq buf "*Diff*"))
      (vc-diff)
      (setq buf "*vc-diff*"))
    (when arg
      (with-current-buffer (get-buffer buf)
        (unless diff-refine
          (setq-local diff-refine 'font-lock))))))

(defvar-local prot-diff--refine-diff-state 0
  "Current state of `prot-diff-refine-dwim'.")

;;;###autoload
(defun prot-simple-rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))


;;;###autoload
(defun prot-search-occur-urls ()
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (let ((buf-name (format "*links in <%s>*" (buffer-name))))
    (add-hook 'occur-hook #'goto-address-mode)
    (occur-1 prot-common-url-regexp "\\&" (list (current-buffer)) buf-name)
    (remove-hook 'occur-hook #'goto-address-mode)))

;;; ORG-AUTOLOAD
;;;; Expand org file name
;;;;
;;;###autoload (autoload '+org/expand-org-file-name )
(defun +org/expand-org-file-name (x)
  "Expand file name X with org-directory."
  (if (eq (type-of x) 'cons)
      (-map #'+org/expand-org-file-name x)
    (expand-file-name x org-directory)))

;;;; Find in files
;;;###autoload
(defun +org/find-in-files (file)
  "Find file in org directory."
  (->> (+org/expand-org-file-name file)
       (find-file)))

;;;; Timestamp
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
