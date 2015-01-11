(defmacro hash-table-keys (hash)
  `(loop for key being the hash-keys of ,hash collect key))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro nvl (object1 object2)
  "If object1 is not nil, eval and return it. Otherwise eval and return object2"
  `(if (eq nil ,object1) ,object2 ,object1))

(defmacro make-hash-table-with-mappings (&optional hash-init &body mappings)
  (with-gensyms (hashtable mappings-setfs)
    `(let ((,hashtable ,(if (eq nil hash-init) `(make-hash-table) `(make-hash-table ,@hash-init))))
       ,(setf mappings-setfs '())
       ,(dolist (mapping mappings)
                (setf mappings-setfs
                      (nconc mappings-setfs (list `(setf (gethash (nth 0 ',mapping) ,hashtable) (nth 1 ',mapping))))))
       ,@mappings-setfs
       ,hashtable
       )
    ))



(defmacro make-prefix-chromosome (decoded-chromosome)
  "Returns a decoded chromosome in prefix form"
  (let ((num-words (length decoded-chromosome)))
    `'(,(second decoded-chromosome)
       ,(first decoded-chromosome)
       ,(third decoded-chromosome)))
  )
