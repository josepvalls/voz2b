#|

Note: the names used for clauses, templates, etc. have to be unique only inside each one of the story blocks or memories

For clauses, there are the following constructs:
:s -> sequence
:a -> alternative (one will be picked at random)
:gt -> go to (:gt <branch-name)
:c -> choice: user input (:c (<choice1> <branch1-clause> <phase1-name> (<additional-entities>) (<additional-expressions>))
							 (<choice1> <branch1-clause> <phase1-name> (<additional-entities>) (<additional-expressions>))
:m -> memory retrieval trigger (the argument refers to the "phase" with which analogy will be made)
:r -> repetition


The first phase of each memory is augmented with force dynamics since is the one used for analogy

|#


(defvar *riu-STORY-DAG* nil)
(setf *riu-STORY-START* 'riu-STORY-0-CAT)

(setf *riu-STORY-DAG* 
	'(
		(riu-STORY-0-CAT
			(:discourse
				(:clauses
					(phase1 (:s (:a t1a t1b t1c) t2a (:m phase1) t3a t4a
					        (:c (ignore (:gt riu-story-1a-ignore-cat) phase1 () ()) 
								(play (:s t5a (:gt riu-story-1b-play-cat)) phase1 () (((play ales cat) :name s0-p1-play)
																	   ((fd-move-tendency s0-p1-agonist s0-p1-play phase1) :name s0-p1-move-agonist)))
								(feed (:s t5b (:gt riu-story-1c-feed-cat)) phase1 () (((feed ales cat) :name s0-p1-feed)
																	   ((fd-move-tendency s0-p1-agonist s0-p1-feed phase1) :name s0-p1-move-agonist))))))
				)
				(:templates
					(t1a "One day, " (s0-p1-walks (ales "Ales") " was walking in the " (street "street")))
					(t1b "One day, " (s0-p1-walks (ales "Ales") " was walking in an alley."))
					(t1c "One day, " (s0-p1-walks (ales "Ales") " was walking outside."))
					(t2a "when " (s0-p1-see (ales "he") " saw a " (cat "cat") " in front of him."))
					(t3a (ales "Ales") " hesitated for a second about what to do with the " (cat "cat") ".")
					(t4a (ales "He") " might be late for " (work "work") " if he lost too much time with the cat...")
					(t5a (s0-p1-play (ales "Ales") " played with the " (cat  "cat")))
					(t5b (s0-p1-feed (ales "Ales") " fed the " (cat  "cat")))
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (cat :type animate) (street :type inanimate) (work :type inanimate))
					(:expressions  
						((walks ales street) :name s0-p1-walks)
						((in cat street) :name s0-p1-in)
						((see ales cat) :name s0-p1-see)
						((animal cat) :name s0-p1-animal)
						((cute cat) :name s0-p1-cute)

						((fd-agonist ales phase1) :name s0-p1-agonist)
						((fd-antagonist work phase1) :name s0-p1-antagonist)
						((fd-stronger s0-p1-agonist phase1) :name s0-p1-strong-agonist)
					)
				)			
			)
		)

		(riu-STORY-1A-IGNORE-CAT
			(:discourse
				(:clauses
					(phase1 (:s t1a t2a t3a (:gt riu-story-2-SHOP)))
				)
				(:templates
					(t1a (ales "Ales") " just kept walking.")
					(t2a (s1a-p1-cat-walks "The " (cat "cat") " walked away."))
					(t3a (s1a-p1-ales-walks (ales "Ales") " then decided to continue his way to " (work "work") "."))
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (cat :type animate) (work :type inanimate) (street :type inanimate) (phase :type inanimate) (phase1 :type phase))
					(:expressions
						((walks ales work) :name s1a-p1-ales-walks)
						((walks cat street) :name s1a-p1-cat-walks)
						((animal cat) :name s1a-p1-animal)

						((fd-agonist ales phase1) :name s1a-p1-agonist)
					)
				)			
			)
		)

		(riu-STORY-1B-PLAY-CAT
			(:discourse
				(:clauses
					(phase1 (:s t1a t2a t3a t4a (:gt riu-story-2-SHOP)))
				)
				(:templates
					(t1a (s1b-p1-play (ales "Ales") " stopped and " (play "played") " with the " (cat "cat")) " in the " (street "street"))
					(t2a (s1b-p1-fun (ales "Ales") " had a lot of fun."))
					(t3a (s1b-p1-walks (ales "Ales") " then decided to continue his way to " (work "work") "."))
					(t4a "But " (s1b-p1-lost-time (ales "he") " lost a lot of " (time "time")) " and " (s1b-p1-late-work "was late to " (work "work")))
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (cat :type animate) (work :type inanimate) (street :type inanimate) (time :type inanimate))
					(:expressions
						((walks ales work) :name s1b-p1-walks)
						((play ales cat) :name s1b-p1-play)
						((fun ales) :name s1b-p1-fun)
						((animal cat) :name s1b-p1-animal)
						((lost ales time) :name s1b-p1-lost-time)
						((late-for ales work) :name s1b-p1-late-work)

						((fd-agonist ales phase1) :name s1b-p1-agonist)
						((fd-move-tendency s1b-p1-agonist s1b-p1-play phase1) :name s1b-p1-move-agonist)
						((fd-antagonist work phase1) :name s1b-p1-antagonist)
						((fd-stronger s1b-p1-agonist phase1) :name s1b-p1-strong-agonist)
					)
				)
			)
		)
		
		(riu-STORY-1C-FEED-CAT
			(:discourse
				(:clauses
					(phase1 (:s t1a t2a t3a t4a (:gt riu-story-2-SHOP)))
				)
				(:templates
					(t1a (s1c-p1-store (ales "Ales") " decided to go into a " (store "convenience store")) " and " (s1c-p1-buy "buy cat " (food "food")) ".")
					(t2a (ales "He") " gave the " (food "food") " to the " (cat "cat") ", and " (s1c-p1-happy-cat "the " (cat "cat") " looked very happy."))
					(t3a (s1c-p2-happy-ales (ales "Ales") " was very happy."))
					(t4a "But " (s1c-p2-lost-time (ales "he") " lost a lot of " (time "time")) " and " (s1c-p2-late-work "was late to " (work "work")))
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (cat :type animate) (work :type inanimate) 
						   (street :type inanimate) (time :type inanimate) (store :type location)
						   (food :type inanimate))
					(:expressions
						((animal cat) :name s1b-animal)
						((walks ales store) :name s1c-p1-store)
						((buy ales food) :name s1c-p1-buy)

						((fd-agonist ales phase1) :name s1c-p1-agonist)
						((fd-antagonist work phase1) :name s1c-p1-antagonist)
						((fd-move-tendency s1c-p1-agonist s1c-p1-buy phase1) :name s1c-p1-move-agonist)
						((fd-antagonist work phase1) :name s1c-p1-antagonist)
						((fd-stronger s1c-p1-agonist phase1) :name s1c-p1-strong-agonist)
					)
				)
				(phase2
					(:entities)
					(:expressions
						((feed ales cat) :name s1c-p2-feed)
						((happy ales) :name s1c-p2-happy-ales)
						((happy cat) :name s1c-p2-happy-cat)
						((lost ales time) :name s1b-p2-lost-time)
						((late ales work) :name s1b-p2-late-work)

						((fd-agonist ales phase2) :name s1c-p2-agonist)
						((fd-antagonist work phase2) :name s1c-p2-antagonist)
						((fd-move-tendency s1c-p2-agonist s1c-p2-feed phase2) :name s1c-p2-move-agonist)
						((fd-antagonist work phase2) :name s1c-p2-antagonist)
						((fd-stronger s1c-p2-agonist phase2) :name s1c-p2-strong-agonist)
					)
				)
			)
		)
		
		(riu-STORY-2-SHOP
			(:discourse
				(:clauses
					(phase1 (:s t1 t2 (:m phase1) (:gt riu-STORY-3-CROWD)))
				)
				(:templates
					(t1 (s2-p1-store (store "Stores") " are about to be open."))
					(t2 (s2-p1-in "Behind the store display windows, " (s2-p1-silverware "shinny " (silverware "sterling silverware"))) " reflects lights and shadows of the city.")
				)
			)
			
			(:structure
				(phase1
					(:entities (store :type location) (silverware :type inanimate) (silver :type inanimate))
					(:expressions
						((is-open store) :name s2-p1-store)
						((shine silverware) :name s2-p1-silverware)
						((has-color silverware silver) :name s2-p1-silver)
						((color silver) :name s2-p1-color)
						((in silverware store) :name s2-p1-in)
					)
				)
			)	
		)
		
		(riu-STORY-3-CROWD
			(:discourse
				(:clauses
					(phase1 (:s t1 t2 
						(:c (go-back (:s t3a (:gt riu-STORY-3a-BACK)) phase1 () (((walks ales crowd) :name s3-p1-walk)
															   	  ((fd-move-tendency s3-p1-agonist s3-p1-walk phase1) :name s3-p1-move-agonist)
																  ((fd-stronger s3-p1-antagonist phase1) :name s3-p1-strong))) 
							(go-forward (:s t3b (:gt riu-STORY-3b-FORWARD)) phase1 () (((walks ales crowd) :name s3-p1-walk)
																   	    ((fd-move-tendency s3-p1-agonist s3-p1-walk phase1) :name s3-p1-move-agonist)
																	    ((fd-stronger s3-p1-agonist phase1) :name s3-p1-strong))))))
				)
				(:templates
					(t1 "Ahead, some " (incident "incident") " occurred at the " (intersection "intersection."))
					(t2 "A big " (crowd "crowd") " of people blocks the way.")
				    (t3a (ales "Ales") " backs up")
				    (t3b (s3-p1-walk (ales "Ales") " walks towards " (crowd "the crowd")))
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (crowd :type inanimate) (intersection :type inanimate) (intersection :type inanimate) (incident :type inanimate))
					(:expressions						
						((in incident intersection) :name s3-in1)
						((in crowd intersection) :name s3-in2)
						((in ales intersection) :name s3-in3)
						
						((fd-agonist ales phase1) :name s3-p1-agonist)
						((fd-antagonist crowd phase1) :name s3-p1-antagonist)
					)
				)
			)	
		)
		
		
		(riu-STORY-3a-BACK
			(:discourse
				(:clauses
					(phase1 (:s t1 (:m phase1)))
				)
				(:templates
					(t1 "Go back. The close contact with " (crowd "others") " always freaks " (ales "Ales") " out.")
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (crowd :type animate))
					(:expressions
						((walks ales crowd) :name s3a-p1-walk)
						
						((fd-agonist ales phase1) :name s3a-p1-agonist)
						((fd-antagonist crowd phase1) :name s3a-p1-antagonist)
						((fd-move-tendency s3a-p1-agonist s3a-p1-walk phase1) :name s3a-p1-move)
						((fd-stronger s3a-p1-antagonist phase1) :name s3a-p1-strong)
					)
				)
			)	
		)
		
		
		(riu-STORY-3b-FORWARD
			(:discourse
				(:clauses
					(phase1 (:s t1 (:m phase1)))
				)
				(:templates
					(t1 "Go forward. " (ales "Ales") " collides with the " (crowd "crowd") " and " (s3b-p1-walk "makes his way through."))
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (crowd :type animate))
					(:expressions
						((walks ales crowd) :name s3b-p1-walk)

						((fd-agonist ales phase1) :name s3b-p1-agonist)
						((fd-antagonist crowd phase1) :name s3b-p1-antagonist)
						((fd-move-tendency s3b-p1-agonist s3b-p1-walk phase1) :name s3b-p1-move)
						((fd-stronger s3b-p1-agonist phase1) :name s3b-p1-strong)
					)
				)
			)	
		)
		
	)
)