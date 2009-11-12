(defclass restricted-dictionary (dictionary)
  (
	(capacity :reader dictionary-capacity
              :initform 20)
  )
)

(defmethod insertItem :around ((d restricted-dictionary) k e)
  (cond ((= (size d) (dictionary-capacity d)) "Cannot insert: Dictionary is full") ; Do not go over capacity
        ((equal k e) "Cannot insert: A key cannot equal its element") ; A key should not equal its element
        ((not (equal nil (find2 d k))) "Cannot insert: A key with this value already exists") ; No duplicate keys
        (t (call-next-method d k e))
  )
)
