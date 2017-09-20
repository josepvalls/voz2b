
;; --------------------------------------------------------------------
;; This file has been automatically generated by Voz
;; Josep Valls-Vargas
;; Jichen Zhu
;; Santiago Ontanon
;; --------------------------------------------------------------------

(setf *story*
  '(STORY-11
     (:discourse
       (:clauses
         (c0 (:s phase1 phase2))
         (phase1 (:s  t1 t2))
         (phase2 (:s  t3 t4 t5))

       )
       (:templates
         ;; Once upon a time, there was a soldier at the orders of certain king.
         ;; The soldier had many abilities and was always loyal.
         ;; The king commanded the soldier to go kidnap a princess for him to marry.
         ;; The soldier accepted the king's orders and prepared to leave.
         ;; He took a small company of soldiers and embarked on their journey.

         (t1 "Once upon a time , there was " (STORY11-m1000 "a soldier") " at the orders of certain king . ")
         (t2 "" (STORY11-m1000 "The soldier") " had many abilities and was always loyal . ")
         (t1001 (STORY11-EXTRA1  (STORY11-m1000 "The soldier") " is the " (Other "Other") ) ".")
         (t3 "The king commanded the " (STORY11-m1000 "soldier") " to go kidnap " (STORY11-m1002 "a princess") " for him to marry . ")
         (t4 "" (STORY11-m1000 "The soldier") " accepted the king 's orders and prepared to leave . ")
         (t5 "" (STORY11-m1000 "He") " took a small company of soldiers and embarked on their journey . ")
         (t1002 (STORY11-EXTRA2  (STORY11-m1000 "He") " is the " (Other "Other") ) ".")
         (t1003 (STORY11-EXTRA3  (STORY11-m1002 "a princess") " is the " (Other "Other") ) ".")

       )
     )
     (:structure
       
       (common
         (:entities
           (human :type animate)
           (MA :type human)
           (FE :type human)
           (anthropomorphized :type animate)
           (AA :type anthropomorphized)
           (AO :type anthropomorphized)
           (othera :type animate)
           (GR :type othera)
           (MB :type anthropomorphized)
           (PA :type entity)
           (AN :type othera)
           (HA :type entity)
           (OB :type inanimate)
           (SC :type inanimate)
           (PO :type inanimate)
           (setting :type entity)
           (SS :type setting)
           (ST :type setting)
           (NC :type entity)
           (NA :type entity)
           (m-1 :type NA)

         )
         (:expressions
         )
       )
      
       (phase1
         (:entities
           (STORY11-m1000 :type animate)
         )
         (:expressions
           ((roleOther STORY11-m1000) :name STORY11-EXTRA0)
         )
       )
       (phase2
         (:entities
           (STORY11-m1000 :type animate)
           (STORY11-m1002 :type animate)
         )
         (:expressions
           ((roleOther STORY11-m1000) :name STORY11-EXTRA1)
           ((roleOther STORY11-m1002) :name STORY11-EXTRA2)
         )
       )

     )
   )
)
    