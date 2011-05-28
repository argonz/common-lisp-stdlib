
(in-package :ch-util)

(defun read-csv-stream (stream
                       &key
                       (delimiter #\,)
                       (whitespace '(#\Space #\Tab #\Newline)))
  (loop for line = (read-line stream nil nil)
     while line
     collect
     (loop with start = 0 for delimited = (position delimiter line :start start)
        collect (let ((end (or delimited (length line))))
                  (prog1
                      (string-left-trim whitespace (subseq line start end))
                    (when delimited (setf start (1+ delimited)))))
        while delimited)))

(defun read-csv-file (file)
  (with-open-file (stream file)
    (read-csv-stream stream)))
