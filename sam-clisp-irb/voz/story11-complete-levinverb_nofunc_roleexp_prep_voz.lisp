
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
         (phase1 (:s  t1 t2 t3 t4 t5))
         (phase2 (:s  t6 t7 t8 t9))

       )
       (:templates
         ;; Once upon a time, there was a soldier at the orders of certain king.
         ;; The soldier had many abilities and was always loyal.
         ;; The king could not find a bride.
         ;; The king commanded the soldier to go kidnap a princess for him to marry.
         ;; The soldier accepted the king's orders and prepared to leave.
         ;; The soldier stood firm at as the captain's post not matter what.
         ;; The ship was lifted from the sea and the soldier flew to a new kingdom.
         ;; There was a castle by a cliff where a maiden had been locked.
         ;; The soldier rescued the maiden from the castle.

         (t1 "Once upon a time , there was " (STORY11-m1001 "a soldier") " at the orders of certain king . ")
         (t2 "" (STORY11-m1001 "The soldier") " had many abilities and was always loyal . ")
         (t3 "" (STORY11-m1000 "The king") " could not find " (STORY11-m1002 "a bride") " . ")
         (t4 "" (STORY11-m1000 "The king") " commanded the " (STORY11-m1001 "soldier") " to go kidnap " (STORY11-m1002 "a princess") " for him to marry . ")
         (t5 "" (STORY11-m1001 "The soldier") " accepted the king 's orders and prepared to leave . ")
         (t1001 (STORY11-EXTRA1  (STORY11-m1000 "The king") " is the " (NA "NA") ) ".")
         (t1002 (STORY11-EXTRA2  (STORY11-m1001 "The soldier") " is the " (NA "NA") ) ".")
         (t1003 (STORY11-EXTRA3  (STORY11-m1002 "a princess") " is the " (NA "NA") ) ".")
         (t6 "" (STORY11-m1001 "The soldier") " stood firm at as the captain 's post not matter what . ")
         (t7 "The ship was lifted from the sea and " (STORY11-m1001 "the soldier") " flew to a new kingdom . ")
         (t8 "There was " (STORY11-m1005 "a castle") " by a cliff where " (STORY11-m1002 "a maiden") " had been locked . ")
         (t9 "" (STORY11-m1001 "The soldier") " rescued " (STORY11-m1002 "the maiden") " from the castle . ")
         (t1004 (STORY11-EXTRA4  (STORY11-m1005 "a castle") " is the " (NA "NA") ) ".")

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
           (STORY11-m1000 :type NA)
           (STORY11-m1001 :type NA)
           (STORY11-m1002 :type NA)
         )
         (:expressions
           ((levin-29-4 STORY11-m1000 STORY11-m1002) :name STORY11-VERB1)
           ((roleNA STORY11-m1000) :name STORY11-EXTRA0)
           ((roleNA STORY11-m1001) :name STORY11-EXTRA1)
           ((roleNA STORY11-m1002) :name STORY11-EXTRA2)
         )
       )
       (phase2
         (:entities
           (STORY11-m1000 :type NA)
           (STORY11-m1001 :type NA)
           (STORY11-m1002 :type NA)
           (STORY11-m1005 :type NA)
         )
         (:expressions
           ((levin-10-5 STORY11-m1001 STORY11-m1002) :name STORY11-VERB2)
           ((roleNA STORY11-m1005) :name STORY11-EXTRA3)
         )
       )

     )
   )
)
    