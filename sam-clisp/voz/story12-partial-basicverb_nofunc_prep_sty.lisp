
;; --------------------------------------------------------------------
;; This file has been automatically generated by Voz
;; Josep Valls-Vargas
;; Jichen Zhu
;; Santiago Ontanon
;; --------------------------------------------------------------------

(setf *story*
  '(STORY-12
     (:discourse
       (:clauses
         (phase1 (:s  t1 t2 t3 t4 t5 t6 t7 t8 t9))

       )
       (:templates
         ;; In old times there was a village where a serpent frequently flew.
         ;; He had devoured all but one of the villagers.
         ;; At that time a gypsy came to the village.
         ;; He came at night and no matter where he looked, he couldn't find anyone!
         ;; He finally entered the last hut where the last man was sitting and crying.
         ;; "Greetings, good man!"
         ;; "Why are you here, gypsy?
         ;; Surely, you must be tired of life."
         ;; "Why do you say that?"

         (t1 "In old times there was a village where " (4369 "a serpent") "frequently flew . "
         (t2 "" (4369 "He") "had devoured all but one " (4372 "one of the villagers") ". "
         (t3 "At that time " (4374 "a gypsy") "came to the village . "
         (t4 "" (4374 "He") "came at night and no matter where " (4374 "he") "looked , " (4374 "he") "could n't find " (4377 "anyone") "! "
         (t5 "" (4374 "He") "finally entered the last hut where " (4372 "the last man") "was sitting and crying . "
         (t6 "' Greetings , good man ! ' "
         (t7 "' Why are " (4374 "you") "here , gypsy ? "
         (t8 "Surely , " (4374 "you") "must be tired of life . ' "
         (t9 "' Why do " (4372 "you") "say that ? ' "

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
           (m4369 :type MB)
           (m4372 :type MA)
           (m4374 :type MA)
           (m4377 :type GR)
         )
         (:expressions
           ((verb-was m-1 m4369) :name E1)
           ((verb-flew m-1 m4369) :name E2)
           ((verb-devoured m4369 m4372) :name E3)
           ((verb-came m-1 m4374) :name E4)
           ((verb-came m-1 m4374) :name E5)
           ((verb-looked m4374 m-1) :name E6)
           ((verb-find m4374 m4377) :name E7)
           ((verb-entered m4374 m4372) :name E8)
           ((verb-sitting m-1 m4372) :name E9)
           ((verb-crying m4372 m-1) :name E10)
           ((verb-are m-1 m4374) :name E11)
           ((verb-be m-1 m4374) :name E12)
           ((verb-say m4372 m-1) :name E13)
         )
       )

     )
   )
)
    