(defclass dictionary ()
   ((size :accessor dictionary-size
          :initform 0)
    (name :reader dictionary-name
          :initarg :name)
	(elements :accessor dictionary-elements
			  :initform '())
   )
)

; Returns the size of the dictionary
(defmethod size ((d dictionary))
  (dictionary-size d))

; Returns true if the dictionary is empty
; Returns false if the dictionary is not empty
(defmethod isEmpty ((d dictionary))
  (if (= (dictionary-size d) 0)
    t
    nil))

; Return a list containing the elements in the dictionary
(defmethod elements ((d dictionary))
  (doElements d (dictionary-elements d))
)

; Return a list containing the head of the tail of each sub list in lst
; e.g. ( (che hungry) (john sleepy) ) would return (hungry sleepy)
(defmethod doElements ((d dictionary) lst)
  (cond ((null lst) nil)
        (t (cons (car (cdr (car lst))) (doElements d (cdr lst))))
  )
)

; Return a list containing the keys in the dictionary
(defmethod keys ((d dictionary))
  (doKeys d (dictionary-elements d))
)

; Return a list containing the head of each sub list in lst.
; e.g. ( (che hungry) (john sleepy) ) would return (che john)
(defmethod doKeys ((d dictionary) lst)
	(cond ((null lst) nil)
		  (t (cons (car (car lst)) (doKeys d (cdr lst))))
    )
)

; Returns the dictionary entry if an entry exists with key k else false
; This method is named find2 because find is a system defined method name
; e.g. (find2 d 'john) with data '((che hungry) (john sleepy)) would return (john sleepy)
(defmethod find2 ((d dictionary) k)
  (doFind d k (dictionary-elements d)) ; use doFind() to use recursion
)

; Returns sublist if the list lst contains a sub list with head k
; e.g. (doFind d 'john '((che hungry) (john sleepy))) would return (john sleepy)
(defmethod doFind ((d dictionary) k lst)
  (cond ((null lst) nil) ; base case 1: the list is empty
        ((equal (car (car lst)) k) (car lst)) ; base case 2: the first entry in the dictionary matches key k
        (t (doFind d k (cdr lst))) ; recursive case: call doFind() without the first entry
  )
)

; Returns list of all found dictionary entries that have key k else false
; e.g. (findAll d 'che) with data '((che hungry) (che sleepy)) would return ((che hungry) (che sleepy))
(defmethod findAll ((d dictionary) k)
  (doFindAll d k (dictionary-elements d)) ; use doFindAll() to use recursion
)

; Returns list of all found sublists that have their head equal to k else false
; e.g. (doFindAll d 'che '((che hungry) (che sleepy))) would return ((che hungry) (che sleepy))
(defmethod doFindAll ((d dictionary) k lst)
  (cond ((null lst) nil) ; base case 1: the list is empty
        ((equal (car (car lst)) k) (cons (car lst) (doFindAll d k (cdr lst)))) ; recursive case 1: first entry matches, keep looking
        (t (doFindAll d k (cdr lst))) ; recursive case 2: first entry does not match, keep looking
  )
)

; Adds an item to the dictionary and returns true
(defmethod insertItem ((d dictionary) k e)
  (setf (dictionary-elements d) (cons (list k e) (dictionary-elements d))) ; add to our dictionary
  (setf (dictionary-size d) (+ 1 (dictionary-size d))) ; increment the dictionary size
  t
 )

; Removes an item from the dictionary
; Returns true if the key exists
; Return false if the key does not exist
(defmethod removeItem ((d dictionary) k)
  (let ((found (find2 d k))) ; see if the key exists
       (cond ((not (null found)) (let ( ; if found then
          (new-items (doRemoveItem d k (dictionary-elements d)))) ; get list with the element removed
		  (setf (dictionary-size d) (- (dictionary-size d) 1)) ; decrement dictionary size
          (setf (dictionary-elements d) new-items) ; set new dictionary items
          t))
       
             (t nil) ; if not found return false
 	   )
   )
)

; Removes a sublist from a list lst if its head equal k
(defmethod doRemoveItem ((d dictionary) k lst)
  (cond ((null lst) nil) ; base case: list is empty
        ((equal (car (car lst)) k) (cdr lst)) ; base case: key is found in first dictionary entry
        (t (cons (car lst) (doRemoveItem d k (cdr lst)))) ; recursive case: call doRemoveItem() with one less dictionary entry
  )
)

; This method will use the previously defined removeItem 
; method to remove elements as long as a key exists
(defmethod removeAllItems ((d dictionary) k)
  (cond ((null (removeItem d k)) nil) ; base case: try removing an item
		(t (removeAllItems d k) t) ; recursive case: restart process of removing the element
  )
)
