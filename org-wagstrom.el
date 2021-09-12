;; Patrick Wagstrom's Custom org-mode Functions
;;
;; Copyright (c) 2021 Patrick Wagstrom <patrick@wagstrom.net>
;; Licensed under terms of the MIT License

(provide 'org-wagstrom)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(defvar org-directory "~/org"
  "Default directory for org mode files")

(defvar org-person-file-name "~/org/Person - %s.org"
  "The default structure for an org file referring to a specific person")

(defvar org-person-regexp "^Person - \\(.*\\).org$"
  "Regular expression glob for finding person files")

;; org mode meeting support
(defvar org-create-meeting-list-file (concat (file-name-as-directory org-directory) "meeting_notes.org")
  "The file that contains the list of all proevious meetings")

;; paste images
(defvar org-image-directory 
  (file-truename
   (file-name-as-directory
    (concat (file-name-as-directory org-directory)
            "images")))
  "The directory that will store all of the images pasted into org-mode notes")

(define-skeleton org-person-skeleton
  "Creates a basic skeleton for a person"
  nil
  "#+TITLE: " str "\n"
  "#+STARTUP: showall\n"
  "#+CATEGORY: person\n"
  "#+TAGS: person\n"
  "#+DATE: <" (format-time-string "%Y-%m-%d %a %H:%M %Z") ">\n"
  "\n"
  "* Background\n"
  (org-id-get-create) "\n"
  "- *Company:* \n"
  "- *Role:* \n"
  "- *Start Date:* \n"
  "- *Namely Profile:* \n"
  "- *LinkedIn Profile:* \n"
  "\n"
  "\n"
  )

(define-skeleton org-meeting-skeleton
  "Creates a basic skeleton for a meeting"
  nil
  "#+TITLE: " str "\n"
  "#+STARTUP: showall\n"
  "#+CATEGORY: meeting\n"
  "#+TAGS: meeting\n"
  "#+DATE: <" (format-time-string "%Y-%m-%d %a %H:%M %Z") ">\n"
  "\n"
  "* Context\n"
  (org-id-get-create) "\n"
  "* Attendees\n"
  "\n"
  "* Take Aways\n"
  "\n"
  "* Notes\n"
  )


;; override spacebar in the mini buffer, I don't use it for completion
;; that often and it breaks the ability to use spaces in people's names.
;;
;; see: https://emacs.stackexchange.com/a/19831/29014
;;      https://stackoverflow.com/a/17476486/57626
;;
;; for IDO mode see: https://github.com/emacs-mirror/emacs/blob/222d033254e1c0c918f3dec523517f3192bc7086/lisp/ido.el#L211-L214

(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)
(define-key ido-common-completion-map " " 'self-insert-command)

(defun org-id-get-id-from-file (file)
  "Gets the ID out of a file"
  (interactive)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (setq outid (org-id-get))
    )
  outid)

;; helper function to get the list of people that have defined files
;; already in org-mode. This is use primarily so we can get completion
;; when using org-insert-person
(defun org-get-person-files ()
  (progn
    (setq rv ())
    (setq df (directory-files org-directory))
    (dolist (elem df rv)
      (if (string-match org-person-regexp elem)
	  (setq rv (cons (match-string 1 elem) rv))))
    (sort rv #'string-collate-lessp)))

(defun org-insert-person (person-name)
  "Inserts a link to a person document.
  PERSON-NAME should be the full ame of the person to create a link to.
  If the document for PERSON-NAME does not exist then it is created."
  (interactive
   (list
    (completing-read "Person Name: " (org-get-person-files) nil nil)))
  (progn
    (setq person-file-name (format org-person-file-name person-name))
    (setq working-buffer (current-buffer))
    (if (not (file-exists-p person-file-name))
	(progn
	  ;; name for new buffer. If start with space, undo is disabled
	  (setq newBufName " org-insert-person-tempbuffer")

	  ;; create a new buffer, save it to a var, so later you can switch to it or kill it
	  (setq newBuf (generate-new-buffer newBufName))
	  
	  ;; make it current (but does not make it visible), so all insert etc operations works on it.
	  (set-buffer newBuf)
	  (org-person-skeleton person-name)
	  ;; like “Save As”. Save current buffer, close it, and open the new saved
	  (write-file person-file-name)	  
	  ;; close it
	  (kill-buffer newBuf)
	  ))
    (set-buffer working-buffer)
    ;; (setq person-link-text (format "[[file:%s][%s]]" person-file-name person-name))
    (setq person-link-text (format "[[id:%s][%s]]" (org-id-get-id-from-file person-file-name) person-name))
    (insert person-link-text)))


(defun org-insert-image ()
  "Pastes an image into a file and then links the image in org-mode"
  (interactive)
  (setq file-name (concat (format-time-string "%Y%m%d %H%M%s") " - " (buffer-name) ".png"))
  (setq file-name-with-path (concat org-image-directory file-name))
  (shell-command (concat "/usr/local/bin/pngpaste \"" file-name-with-path "\"") nil nil)
  (insert "#+CAPTION: Your_Caption_Here")
  (newline)
  (insert "#+ATTR_ORG: :width 500") 
  (newline)
  (insert (concat "[[" file-name-with-path "]]"))
  (newline)
  )


 
;; TODO this should check to see if the meeting already exists and, if so, just open it
(defun org-create-meeting (meeting-name)
  (interactive "sMeeting Name:")
  (setq meeting-name-with-date
	(concat (format-time-string "%Y%m%d")
		" - "
		meeting-name))
  (setq meeting-short-filename
	(concat meeting-name-with-date
		".org"))  
  (setq filename
	(concat
	 (file-name-as-directory org-directory)
	 meeting-short-filename))
  (setq meeting-link-text (format "\n* [[file:%s][%s]]" meeting-short-filename meeting-name-with-date))
  (message "Meeting name: %s" filename)

  (setq meeting-list-buffer (get-buffer (file-name-nondirectory org-create-meeting-list-file)))

  ;; add the entry to the index file
  (if (buffer-live-p meeting-list-buffer)
      (with-current-buffer meeting-list-buffer
	(progn (goto-char (point-max))
               (insert meeting-link-text)
               (save-buffer)
	       ))
    (write-region meeting-link-text nil org-create-meeting-list-file 'append)
    )

  (with-current-buffer (find-file filename)
    (org-meeting-skeleton meeting-name-with-date))
  )
