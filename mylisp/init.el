;; Please load this file after loading all the essentials.
(defun aki237/duplicate-line()
  "This function is to copy the contents of a line where the point is, 
and make make a new line and paste it there"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
  )

(defun aki237/move-line-up ()
  "This interactive function is used to transpose the current line with the previous line."
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun aki237/move-line-down ()
  "This interactive function is used to transpose the current line with the next line."
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))


(defun index(a n)
  "To find the nth index in a list"
       (defvar b)
       (setq b a)
       (if (> n (- (length a) 1))
	   (print "Error : out of bounds" )
	 nil)
       (cl-loop for i from 1 to n do
	     (setq b (cdr b)))
       (car b))


(defun Index(a &rest n)
  (defvar b)
  (setq b a)
  (cl-loop for i in n do
	   (setq b (index b i)))
  b
  )

(defun ddg (search-query)
  (interactive "sSearch For : ")
  (eww (concat "https://duckduckgo.com/html/?q=" search-query))
 )
