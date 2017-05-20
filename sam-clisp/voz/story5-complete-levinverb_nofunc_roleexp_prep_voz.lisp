
;; --------------------------------------------------------------------
;; This file has been automatically generated by Voz
;; Josep Valls-Vargas
;; Jichen Zhu
;; Santiago Ontanon
;; --------------------------------------------------------------------

(setf *story*
  '(STORY-5
     (:discourse
       (:clauses
         (c0 (:s phase1 phase2))
         (phase1 (:s  t1 t2 t3 t4))
         (phase2 (:s  t5))

       )
       (:templates
         ;; Shall I amuse you with a little tale?
         ;; It's a wonderful tale.
         ;; There are marvelous marvels, wondrous wonders, and the laborer Shabarsha, Shabarsha who is a rogue among rogues: oh, well, in for a penny, in for a pound!
         ;; So Shabarsha set off to work as a laborer, and times were bad.
         ;; But one owner thought, a deep thought.

         (t1 "Shall " (STORY5-VERB1 "" (STORY5-m3852 "I") " amuse " (STORY5-m3853 "you") " " ) "with a little tale ? ")
         (t2 "It 's a wonderful tale . ")
         (t3 "There are marvelous marvels , wondrous wonders , and the laborer Shabarsha , Shabarsha who is a rogue among " (STORY5-m3861 "rogues") " : oh , well , in for a penny , in for a pound ! ")
         (t4 "So " (STORY5-m3862 "Shabarsha") " set off to work as a laborer , and times were bad . ")
         (t5 "But " (STORY5-m3867 "one owner") " thought , a deep thought . ")

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
           (STORY5-m3852 :type MA)
           (STORY5-m3853 :type MA)
           (STORY5-m3862 :type MA)
           (STORY5-m3861 :type MA)
         )
         (:expressions
           ((levin-31-1 STORY5-m3852 STORY5-m3853) :name STORY5-VERB1)
         )
       )
       (phase2
         (:entities
           (STORY5-m3867 :type MA)
           (STORY5-m3852 :type MA)
           (STORY5-m3853 :type MA)
           (STORY5-m3862 :type MA)
           (STORY5-m3861 :type MA)
         )
         (:expressions
         )
       )

     )
   )
)
    