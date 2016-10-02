(defvar mypkg/pkg-directory '())

(defun mypkg/load-el-from (dir)
  "This function is to load all the elisp files in a given directory."
  (setq list (directory-files dir :match-regexp ".*\.el"))
  (loop for i in list do
	(load-file i)))

(defun mypkg/package-directory(dir)
  "This function is used to add the package directories inside this passed \"dir\" to the load-path"
  (add-to-list 'mypkg/pkg-directory dir)
  (setq pkgs (directory-files-and-attributes dir :match-regexp "^[^\.]+.*"))
  (loop for pkg in pkgs do
      (setq name (index pkg 0))
      (if (booleanp (index pkg 1))
	  (progn
	   (add-to-list 'load-path name)
	    (message "Loaded package : " name "\n")))))

(defun mypkg/package-from-repo(url)
  (interactive "sEnter the package's Repo URL : ")
  (setq splitted (delete "" (split-string url "/" )))
  (setq dirname (index splitted (- (length splitted) 1)))
  (loop for i in mypkg/pkg-directory do
	(if (file-exists-p (concat i dirname))
	    (progn
	      (message "Note : that package is probably already installed. Updating...")
	      (async-shell-command (concat "git -C " (concat i "/" dirname) " pull"))
	      )
	  (progn
	    (async-shell-command (concat "git clone --recursive " url (concat " "(car mypkg/pkg-directory) "/" dirname)))
	    (async-shell-command (concat "git -C " (concat i "/" dirname) " submodule init"))
	    (async-shell-command (concat "git -C " (concat i "/" dirname) " submodule update")))
	  )
	(add-to-list 'load-path (concat (car mypkg/pkg-directory) "/" dirname))))
