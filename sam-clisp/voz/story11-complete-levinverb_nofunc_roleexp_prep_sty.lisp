
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
         (phase1 (:s  t1 t2 t3 t4))
         (phase2 (:s  t5))

       )
       (:templates
         ;; Once upon a time there was an old couple, and they had three Sons.
         ;; Two of these had their wits about them, but the third was a simpleton, Ivan by name, surnamed Popyalof.
         ;; For twelve whole years Ivan lay among the ashes from the stove; but then he arose, and shook himself, so that six poods of ashes fell off from him.
         ;; Now in the land in which Ivan lived there was never any day, but always night.
         ;; Well, Ivan undertook to kill that Snake, so he said to his father, "Father make me a mace five poods in weight."

         (t1 "Once upon a time there was an old couple , and they had three Sons . ")
         (t2 "Two of these had their wits about them , but " (STORY11-m4097 "the third") " was " (STORY11-m4097 "a simpleton, Ivan by name, surnamed Popyalof") " . ")
         (t3 "For twelve whole years " (STORY11-m4097 "Ivan") " lay among the ashes from the stove ; but then " (STORY11-m4097 "he") " arose , and shook " (STORY11-m4097 "himself") " , so that six poods of ashes fell off from " (STORY11-m4097 "him") " . ")
         (t4 "Now in the land in which " (STORY11-m4097 "Ivan") " lived there was never any day , but always night . ")
         (t1001 (STORY11-EXTRA1  (STORY11-m4097 "Ivan") " is the " (Hero "Hero") ) ".")
         (t5 "Well , " (STORY11-VERB2 "" (STORY11-m4097 "Ivan") " undertook to kill " (STORY11-m4107 "that Snake") " " ) ", so " (STORY11-VERB3 "" (STORY11-m4097 "he") " said to his " (STORY11-m4108 "his father") " , ' " (STORY11-m4108 "Father") " " ) "make " (STORY11-m4097 "me") " a mace five poods in weight . ' ")
         (t1002 (STORY11-EXTRA2  (STORY11-m4097 "me") " is the " (Hero "Hero") ) ".")
         (t1003 (STORY11-EXTRA3  (STORY11-m4107 "that Snake") " is the " (Villain "Villain") ) ".")
         (t1004 (STORY11-EXTRA4  (STORY11-m4108 "Father") " is the " (Other "Other") ) ".")

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
           (STORY11-m4097 :type MA)
         )
         (:expressions
           ((levin-40-6 STORY11-m4097 STORY11-m4097) :name STORY11-VERB1)
           ((roleHero STORY11-m4097) :name STORY11-EXTRA0)
         )
       )
       (phase2
         (:entities
           (STORY11-m4097 :type MA)
           (STORY11-m4107 :type MB)
           (STORY11-m4108 :type MA)
         )
         (:expressions
           ((levin-42-1 STORY11-m4097 STORY11-m4107) :name STORY11-VERB2)
           ((levin-37-7 STORY11-m4097 STORY11-m4108) :name STORY11-VERB3)
           ((roleHero STORY11-m4097) :name STORY11-EXTRA1)
           ((roleVillain STORY11-m4107) :name STORY11-EXTRA2)
           ((roleOther STORY11-m4108) :name STORY11-EXTRA3)
         )
       )

     )
   )
)
    