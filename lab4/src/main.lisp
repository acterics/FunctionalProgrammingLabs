(defun get-file-content (filename)
  (with-open-file (stream filename)
    (first (loop for line = (read-line stream nil)
          while line
          collect line))))

(defun main() 
    (setq originText (get-file-content "../res/text.txt"))
    (print originText)
)

(main)