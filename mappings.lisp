(in-package :glacier)

(defun generate-cw (status-text mappings)
  "returns a content warning based off of our cw-mappings and the STATUS-TEXT"
  (when mappings
    (let ((cw-list (loop :with text := (str:split " " (str:downcase status-text))
                         :for mapping :in mappings
                         :for cw := (first mapping)
                         :for trigger-words := (car (rest mapping))

                         ;; i could probably do something with regexes to simplify this
                         ;;  but also :shrug: it works
                         :collect (loop :for trigger :in trigger-words
                                        :if (member trigger text :test #'str:containsp)
                                          :return cw)
                           :into cws
                         :finally
                            (return (remove-if #'not cws)))))
      (str:join ", " (remove-duplicates cw-list :test #'string=)))))

(defun load-mapping-files (files)
  "loads the content warning mappings from FILES

returns an alist containing all mappings"
  (loop :with mappings
        :for f :in (ensure-list files)
                
        :when (uiop:file-exists-p f)
          :do (setf mappings
                    (append mappings
                            (parse-mapping-file f)))
        :finally (return mappings)))

(defun parse-mapping-file (file)
  "parses mapping FILE, returning an alist of the mappings of the from (CW (words))"
  (let ((lines (str:lines (str:from-file file))))
    (loop :with cw-def
          :with trigger-def
          :with file-def
          
          :for line :in lines

          :when (and (not cw-def) (str:starts-with-p "warning:" line))
            :do (setf cw-def (str:trim (str:replace-first "warning:" "" line)))

          :when (and cw-def (str:starts-with-p "words:" line))
            :do (setf trigger-def (map 'list #'str:trim (str:split "," (str:replace-first "words:" "" line))))

          :when (and cw-def trigger-def)
            :do (setf file-def (append file-def (list (list cw-def trigger-def)))
                      cw-def nil
                      trigger-def nil)

          :finally (return file-def))))

(defun mappings-updated-p (files)
  "checks to see if any mapping FILES has been updated since our last check"
  (let ((mod-times (map 'list #'file-write-date (ensure-list files))))
    (some #'> mod-times *mappings-write-date*)))
