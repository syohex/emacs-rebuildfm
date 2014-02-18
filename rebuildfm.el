;;; rebuildfm.el --- Emacs rebuildfm client

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-rebuildfm
;; Version: 0.01
;; Package-Requires: ((helm "1.0") (cl-lib "0.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; rebuildfm.el is a emacs client of rebuild.fm(http://rebuild.fm/).
;;
;; rebuildfm.el provides showing podscasts list with helm interface.
;; Its actions are
;;   - Play podcast mp3(requires `avplay' or `ffplay')
;;   - Browse podcast page
;;

;;; Code:

(require 'helm)
(require 'xml)
(require 'url)
(require 'cl-lib)

(defgroup rebuildfm nil
  "rebuildfm client"
  :group 'applications)

(defcustom rebuildfm-mp3-player (or (and (executable-find "avplay") "avplay")
                                    (and (executable-find "ffplay") "ffplay"))
  "MP3 player for playing podcast. The player should support
to open mp3 URL"
  :type 'string
  :group 'rebuildfm)

(defconst rebuildfm--feeds-url "http://feeds.rebuild.fm/rebuildfm")

(defun rebuildfm--move-beginning-of-body ()
  (unless (re-search-forward "^$" nil 't)
    (error "Can't find response body"))
  (forward-line 1)
  (point))

(defun rebuildfm--remove-response-header (buf)
  (with-current-buffer buf
    (goto-char (point-min))
    (rebuildfm--move-beginning-of-body)
    (delete-region (point-min) (point))))

(defsubst rebuildfm--extract-tag-value (tag tree)
  (cadr (assoc-default tag tree)))

(defsubst rebuildfm--extract-tag-attribute (tag attribute tree)
  (assoc-default attribute (car (assoc-default tag tree))))

(defsubst rebuildfm--construct-item (item)
  (let ((tree (cl-remove-if-not (lambda (e)
                                  (and e (listp e))) item)))
    (let ((title (rebuildfm--extract-tag-value 'title tree))
          (link  (rebuildfm--extract-tag-value 'link tree))
          (summary (rebuildfm--extract-tag-value 'summary tree))
          (mp3-url (rebuildfm--extract-tag-attribute 'enclosure 'url tree)))
      (cons title
            (list :link link :summary summary :mp3-url mp3-url)))))

(defun rebuildfm--parse-feed (buf)
  (with-current-buffer buf
    (let ((feed (libxml-parse-xml-region (point-min) (point-max))))
      (let* ((rss (cdr feed))
             (channel (cdr (assoc-default 'channel rss))))
        (cl-loop for elm in channel
                 when (and (listp elm) (eq (car elm) 'item))
                 collect (rebuildfm--construct-item elm))))))

(defun rebuildfm--get-feeds (url)
  (let ((url-request-method "GET"))
    (let ((response-buf (url-retrieve-synchronously url)))
      (unless response-buf
        (error "Can't get '%s'" url))
      (rebuildfm--remove-response-header response-buf)
      (rebuildfm--parse-feed response-buf))))

(defun rebuildfm--collect-podcasts ()
  (rebuildfm--get-feeds rebuildfm--feeds-url))

(defun rebuildfm--persistent-action (item)
  (with-help-window (help-buffer)
    (princ (plist-get item :summary))))

(defun rebuildfm--mp3-player-command (cmd url)
  (cond ((member cmd '("avplay" "ffplay"))
         (format "%s -autoexit -nodisp %s" cmd url))
        (t
         (error "'%s' is not supported!!" cmd))))

(defun rebuildfm--play-podcast (item)
  (let ((mp3-url (plist-get item :mp3-url))
        (buf (get-buffer-create "*rebuildfm mp3*")))
    (start-process-shell-command
     "rebuildfm-mp3" buf
     (rebuildfm--mp3-player-command rebuildfm-mp3-player mp3-url))))

(defun rebuildfm--browse-page (item)
  (let ((link (plist-get item :link)))
    (browse-url link)))

(defvar helm-rebuildfm-source
  '((name . "Rebuildfm Podcasts")
    (candidates . rebuildfm--collect-podcasts)
    (persistent-action . rebuildfm--persistent-action)
    (action . (("Play Podcast" . rebuildfm--play-podcast)
               ("Browse Podcast Page" . rebuildfm--browse-page)))))

;;;###autoload
(defun rebuildfm ()
  (interactive)
  (helm :sources '(helm-rebuildfm-source) :buffer "*rebuildfm*"))

;;;###autoload
(defun rebuildfm-stop ()
  (interactive)
  (let ((buf (get-buffer "*rebuildfm mp3*")))
    (if (not buf)
        (message "already stopped")
      (let ((proc (get-buffer-process buf)))
        (when proc
          (when (yes-or-no-p "Stop MP3 Player? ")
            (kill-process proc)))))))

(provide 'rebuildfm)

;;; rebuildfm.el ends here
