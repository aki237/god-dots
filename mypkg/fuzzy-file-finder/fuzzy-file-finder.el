;;; fuzzy-file-finder.el --- Fuzzy File Finder for emacs

;; Copyright (C) 2016 Akilan Elango

;; Author: Akilan Elango <akilan1997 [at] gmail.com>
;; Keywords: convenience, emulations
;; X-URL: https://github.com/aki237/fuzzy-file-finder
;; URL: https://github.com/aki237/fuzzy-file-finder
;; Package-Requires: ((cl-lib "0.3"))
;; Version: 0.0.1

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
;; Just a beginners package for emacs. Fuzzy File Finder.
;;
;;; Installation:
;;
;;   (require 'fuzzy-file-finder)
;;   (global-set-key (kbd "C-c C-f") 'fff:fuzzy-find) ;; map a key chord for this function
;;
;;; Use:
;; This function is used to search through files in the CWD.
;;
;;; Code:

;; require
(require 'ido)
(require 'ido-better-flex)

;; variables
(defvar fff:filter-regexp "^[^\.].*"
  "The default filter string. This is required as '.','..' will throw errors.")

;; functions
(defun fff:find-in-list(list filename)
  "This is an internal fff function to find the actual path of the filename in the project."
  (setq ret "")
    (loop for i in list do
	  (if (string= (car i) filename)
	      (setq ret (index i 1))
	    ))
    ret)


(defun fff:index-all-files(dir)
  "Index all the files in the 'dir' directory recursively. This will take time if the project is big :(."
  (setq files (directory-files-and-attributes dir :match-regexp fff:filter-regexp))
  (loop for file in files do
	(if (booleanp (index file 1))
	    (if (index file 1)
		(progn
		  (if (and (file-accessible-directory-p (index file 0)) (file-readable-p (index file 0)))
		      (progn
			;;(insert "\n Reading Directory : " (index file 0))
			(setq temp (fff:index-all-files (index file 0)))
			(append indices temp))))
	      (progn
		(setq name (file-name-nondirectory (index file 0)))
		;;(insert "\n Reading File : " name)
		(add-to-list 'indices (list name (concat dir "/" name)))))))
  indices)

(defun fff:fuzzy-find(fff:file)
  "This is the interactive function to Fuzzy find the file. If any this functions has to be key mapped with any 
desired keybinding"
  (interactive
   (list
      (ido-completing-read "sFuzzy Find File: "
			   (progn
			     (setq indices '())
			     (setq fff:completions '())
			     (setq fff:searcher '())
			     (setq fff:searcher (fff:index-all-files (mybkmrk/get-where-to-fuzzy)))
			     (mapcar 'car fff:searcher )))
      ))
  (setq fff:path (fff:find-in-list fff:searcher fff:file))
  (if (booleanp (find-buffer-visiting fff:path))
      (progn
	(find-file fff:path)
	)
    (switch-to-buffer (find-buffer-visiting fff:path))))


(provide 'fuzzy-file-finder)

;; Local Variables:
;; coding: utf-8
;; End:
;;; fuzzy-file-finder.el ends here
