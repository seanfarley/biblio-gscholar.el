;;; biblio-gscholar.el --- Lookup and import bibliographic entries from Google Scholar -*- lexical-binding: t -*-

;; Copyright (C) 2018

;; Author: Nathaniel Chodosh <nchodosh@gmail.com>
;; URL: http://github.com/cpitclaudel/biblio.el
;; Package-requires: ((emacs "25.1") (biblio-core "0.2") (gscholar-bibtex "0.3.1"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Lookup and download bibliographic records from Google Scholar
;; using `gscholar-bibtex'.
;;
;; This package uses `biblio-selection-mode', and plugs into the more general
;; `biblio' package (which see for more documentation).

;;; Code:
(require 'biblio-core)
(require 'gscholar-bibtex)


(defun biblio-gscholar--url (query)
  (let* ((url-request-method "GET")
         (system-time-locale "C")
         ;; Fabricate a cookie with a random ID that expires in an hour.
         (random-id (format "%016x" (random (expt 16 16))))
         (expiration (format-time-string "%a, %d %b %Y %H:%M:%S.00 %Z"
                                         (time-add (current-time)
                                                   (seconds-to-time 3600)) t))
         (my-cookie (mapconcat #'identity
                               (list (format "GSP=ID=%s:CF=4" random-id)
                                     (format "expires=%s" expiration)
                                     "path=/"
                                     "domain=scholar.google.com")
                               "; "))
         (url-current-object
          (url-generic-parse-url "http://scholar.google.com")))
    (url-cookie-handle-set-cookie my-cookie)
    (concat "https://scholar.google.com/scholar?q="
            ;; To prepare the query string, we need to:
            ;; 1. Remove some extraneous puncutation.
            ;; 2. Hex-encode it.
            ;; 3. Convert encoded spaces to +
            ;; If we encode spaces as + first, url-hexify-string
            ;; hex-encodes the + symbols, and they are not interpreted
            ;; properly as spaces on the server.
            (replace-regexp-in-string "%20" "\+"
                                      (url-hexify-string
                                       (replace-regexp-in-string "[:,]" "" query))))))


(defun biblio-gscholar--parse-search-results ()
  (let* ((titles (gscholar-bibtex-google-scholar-titles (buffer-string)))
         (author-lists (mapcar (lambda (sub) (split-string
                                         (car (split-string
                                               sub
                                               "\x3fffa0")) ","))
                               (gscholar-bibtex-google-scholar-subtitles (buffer-string))))
         (bibtex-urls (gscholar-bibtex-google-scholar-bibtex-urls (buffer-string)))
         (bibtex-contents (mapcar 'gscholar-bibtex-google-scholar-bibtex-content bibtex-urls)))

    (cl-loop for title in titles
             for author in author-lists
             for bibtex-content in bibtex-contents
             collecting `((authors . ,author)
                          (title . ,title)
                          (bibtex . ,bibtex-content)))))

(defun biblio-gscholar--forward-bibtext (metadata forward-to)
  (funcall forward-to (biblio-format-bibtex (alist-get 'bibtex metadata))))

;;;###autoload
(defun biblio-gscholar-backend (command &optional arg &rest more)
  "A Google Scholar backend for biblio.el"
  (pcase command
    (`name "Google Scholar")
    (`prompt "Google Scholar query: ")
    (`url (biblio-gscholar--url arg))
    (`parse-buffer (biblio-gscholar--parse-search-results))
    (`forward-bibtex (biblio-gscholar--forward-bibtext arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-gscholar-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-gscholar-backend)

;;;###autoload
(defun biblio-gscholar-lookup (&optional query)
  "Start a Google Scholar search for QUERY, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-gscholar-backend query))

(provide 'biblio-gscholar)
;;; biblio-gscholar.el ends here
