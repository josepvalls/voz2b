
;; --------------------------------------------------------------------
;; This file has been automatically generated by Voz
;; Josep Valls-Vargas
;; Jichen Zhu
;; Santiago Ontanon
;; --------------------------------------------------------------------

(setf *story*
  '(STORY-8
     (:discourse
       (:clauses
         (c0 (:s phase1))
         (phase1 (:s  t1 t2 t3 t4 t5))

       )
       (:templates
         ;; Once upon a time there was a happy queen with a son and a daughter.
         ;; In the kingdom lived a witch that was jealous of their happiness.
         ;; The witch devised a plan to ruin their family.
         ;; The witch visited them.
         ;; She gave the prince a ring and told him that he would be happy as long as he married a girl who the ring would fit.

         (t1 "Once upon a time there was " (STORY8-m1000 "a happy queen") " with " (STORY8-m1001 "a son") " and " (STORY8-m1002 "a daughter") " . ")
         (t2 "In the kingdom lived a " (STORY8-m1003 "witch") " that was jealous of their happiness . ")
         (t3 "" (STORY8-m1003 "The witch") " devised a plan to ruin their family . ")
         (t4 "" (STORY8-m1003 "The witch") " visited them . ")
         (t5 "" (STORY8-m1003 "She") " gave " (STORY8-m1004 "the prince") " a ring and told him that he would be happy as long as he married a girl who the ring would fit . ")
         (t1001 (STORY8-EXTRA1  (STORY8-m1001 "a son") " is the " (Other "Other") ) ".")
         (t1002 (STORY8-EXTRA2  (STORY8-m1002 "a daughter") " is the " (Other "Other") ) ".")
         (t1003 (STORY8-EXTRA3  (STORY8-m1003 "She") " is the " (Other "Other") ) ".")

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
           (STORY8-m1000 :type animate)
           (STORY8-m1001 :type animate)
           (STORY8-m1002 :type animate)
           (STORY8-m1003 :type animate)
           (STORY8-m1004 :type animate)
         )
         (:expressions
           ((levin-13-1 STORY8-m1003 STORY8-m1004) :name STORY8-VERB1)
           ((roleOther STORY8-m1001) :name STORY8-EXTRA0)
           ((roleOther STORY8-m1002) :name STORY8-EXTRA1)
           ((roleOther STORY8-m1003) :name STORY8-EXTRA2)
         )
       )

     )
   )
)
    