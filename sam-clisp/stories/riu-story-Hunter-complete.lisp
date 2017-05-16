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
					(:entities (ales :type robot) (robot :type animate) (cat :type animal) (animal :type animate) (street :type inanimate) (work :type inanimate))
					(:expressions  
						((walks ales street) :name s0-p1-walks)
						((in cat street) :name s0-p1-in)
						((see ales cat) :name s0-p1-see)
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
					(:entities (ales :type robot) (robot :type animate) (cat :type animal) (animal :type animate) (work :type inanimate) (street :type inanimate) (phase :type inanimate) (phase1 :type phase))
					(:expressions
						((walks ales work) :name s1a-p1-ales-walks)
						((walks cat street) :name s1a-p1-cat-walks)

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
					(:entities (ales :type robot) (robot :type animate) (cat :type animal) (animal :type animate) (work :type inanimate) (street :type inanimate) (time :type inanimate))
					(:expressions
						((walks ales work) :name s1b-p1-walks)
						((play ales cat) :name s1b-p1-play)
						((fun ales) :name s1b-p1-fun)
						((lose ales time) :name s1b-p1-lost-time)
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
					(:entities (ales :type robot) (robot :type animate) (cat :type animal) (animal :type animate) (work :type inanimate) 
						   (street :type inanimate) (time :type inanimate) (store :type location)
						   (food :type inanimate))
					(:expressions
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
						((lose ales time) :name s1b-p2-lost-time)
						((late-for ales work) :name s1b-p2-late-work)

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
					(:entities (store :type location) (silverware :type inanimate) (silver :type color) (color :type inanimate))
					(:expressions
						((is-open store) :name s2-p1-store)
						((shine silverware) :name s2-p1-silverware)
						((has-color silverware silver) :name s2-p1-silver)
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
					(phase1 (:s t1 (:m phase1) (:gt STORY-4-ARISTO-REWARD)))
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
					(phase1 (:s t1 (:m phase1) (:gt STORY-4-ARISTO-REWARD)))
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
		
		(STORY-4-ARISTO-REWARD
			(:discourse
				(:clauses
					(phase1 (:s t1a t2a t3a t4a (:m phase1)
					        (:c (take-reward (:s t5a (:gt STORY-4A-ACCEPT)) phase1 () (((accept ales s4-offer) :name s0-p2-accept)
																	   ((fd-stronger s0-p2-antagonist phase1) :name s0-p2-strong-antagonist)))
								(reject-reward (:s t5b (:gt STORY-4B-REJECT)) phase1 () (((reject ales s4-offer) :name s0-p2-reject)
																	   ((fd-stronger s0-p2-antagonist phase1) :name s0-p2-strong-antagonist)))
								;;;The following choice is only available if a memory with quality "confidence" is in pool.
								(ask-for-job (:s t5c (:gt STORY-4C-ASK-FOR-JOB)) phase1 () (((tell ales richbot job) :name s0-p2-ask)
																	   ((fd-stronger s0-p2-agonist phase1) :name s0-p2-strong-agonist))))))
				)
                
				(:templates
					(t1a (s4-greet "The " (richbot "Aristobot") " gives " (ales "Ales") " a firm, two handed shake") " and asks " (ales "Ales") " what he's doing in the area.") 
					(t2a "As " (s4-tell (ales "Ales") " tells the " (richbot "Aristobot") " about " (topic "life in the sparsely populated areas and how the city offers more " (job "opportunity") ","))) 
					(t3a (ales "he") " notices that the " (richbot "Aristobot") " has pulled out a handkerchief and is vigorously " (clean "scrubbing") " his hands,") 
					(t4a (s4-offer "The " (richbot "rich robot") " pulls a " (trinket "trinket") " from his pocket, a small trifle to him but of decent value on the street, and offers " (trinket "it") " to " (ales "Ales") "."))

					(t5a (s0-p2-accept (ales "Ales") " extends his hand, gratefully accepting the Aristobots offer of a reward."))
					(t5b (s0-p2-reject (ales "Ales") " declines the award."))
					;;;The following choice is only available if a memory with quality "confidence" is in pulled pool.
					(t5c (s0-p2-ask "Feeling emboldened by his recent recollection, " (ales "Ales") " decides to ask for what he really wants: a job."))
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (richbot :type robot) (topic :type inanimate) (trinket :type inanimate) (job :type inanimate))
					(:expressions
						((rich richbot) :name s4-rich)
						((greets richbot ales) :name s4-greet)
						((tell ales richbot topic) :name s4-tell)
						((offer richbot ales trinket) :name s4-offer)

						((fd-agonist ales phase1) :name s0-p2-agonist)
						((fd-antagonist richbot phase1) :name s0-p2-antagonist)
					    ((fd-move-tendency s0-p2-agonist job phase1) :name s0-p2-move-agonist)
					)
				)			
			)
		)
		
		(STORY-4A-ACCEPT
		;;; Memory Qualities: Gratefulness, Submissive, Indebtedness, Inferiority, Class-affirming
			(:discourse
				(:clauses
					(c1 (:s t1a t2a t3a (:gt story-5-THIEF)))
				)
				(:templates
					(t1a (s3a-p1-give "The " (richbot "Aristobot") " hands over the " (trinket "trinket") " and takes a step back, admiring his own generosity."))
					(t2a "The " (richbot "Aristobot") " then gives a curt nod, rattles off an impersonal parting phrase and goes his own way.")
					(t3a (ales "Ales") " walks of, wondering where the City streets will take him.")
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (richbot :type robot) (trinket :type inanimate))
					(:expressions
						((rich richbot) :name s4-rich)
						((give richbot ales trinket) :name s3a-p1-give)
					)
				)
			)
		)
		
		(STORY-4B-REJECT
		;;; Memory Qualities: Chivalry, Disgust, Right/Wrong, Unworthy, Amoral
			(:discourse
				(:clauses
					(c1 (:s t1a t2a t3a (:gt story-5-THIEF)))
				)
				(:templates
					(t1a "An awkward moment hangs in the air and it seems seems that the conversation is over.")
					(t2a "The " (richbot "Aristobot") " utters a barely perceptible 'oh' and studies the mansory on the roof of some building stories above.")
					(t3a (ales "Ales") " walks of, wondering where the City streets will take him.")
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (richbot :type robot))
					(:expressions
						((rich richbot) :name s4-rich)
					)
				)
			)
		)
		
		(STORY-4C-ASK-FOR-JOB
		;;; Memory Qualities: Chivalry, Disgust, Right/Wrong, Unworthy, Amoral
			(:discourse
				(:clauses
					(c1 (:s t1a t2a t3a t4a (:gt Story-5-THIEF)))
				)
				(:templates
					(t1a "Feeling emboldened by a recent recollection, " (ales "Ales") " tells the " (richbot "Aristobot") " that he can keep his " (trinket "trinket,"))
					(t2a "The " (richbot "Aristobot") " thinks for a moment and with an 'ah ha!' the robot begins to explain that he has a friend with a factory and that they may be looking for extra workers.")
					(t3a "With a recommendation from himself, the Aristobot said, " (ales "Ales") " would almost certainly find some kind of employment at the factory. ")
					(t4a "After quickly jotting down directions and a brief endorsement the " (richbot "Aristobot") " sent " (ales "Ales") " on his way with a gracious nod and a 'good day.'")
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate) (richbot :type robot) (trinket :type inanimate))
					(:expressions
						((rich richbot) :name s4-rich)
					)
				)
			)
		)
		
		
        (Story-5-THIEF   
            (:discourse
                (:clauses
                    (phase1 (:s (:a t1a t1b) (:a t2a t2b t2c) (:a t3a t3b) (:m phase1) t4a
			        	(:c (ignore (:s (:gt STORY-6-FACTORY-JOB)) phase1 () ()) 
							(help (:s t5 (:gt STORY-6-FACTORY-JOB)) phase1 () (((help ales victim) :name s5-p1-help))))))
                )
                (:templates
						(t1a (s5-walk (ales "Ales") " is " (s5-walks "walking") " down an unremarkable " (s5-in-street (s5-street "street") " in a " (s5-sad-city "grungy " (city "metropolis.")))))
						(t1b (s5-walk (ales "Ales") " " (s5-walks "strolls") " along a dystopian " (s5-in-street (s5-street "avenue") " in a " (s5-sad-city "decaying" (city "city.")))))
						(t2a (s5-ahead "Ahead, " (ales "He") " " (s5-see "sees a wandering " (s5-victim "robot")) " who appears to be of the " (rich "leisure class.")))
						(t2b (s5-ahead "Ahead, " (ales "He") " " (s5-sees "see a " (s5-lost (s5-rich "well-appointed " (s5-victim "robot")) " peers up from a map, confused."))))
						(t2c (s5-ahead "Up the street, " (ales "Ales") " " (s5-see "sees a " (s5-lost (s5-rich "wealthy") " " (s5-victim "robot") " studies street sign names while scratching his chin."))))
						(t3a (ales "Ales") " " (s5-notice "notices") " that a " (s5-thief "small robot") " is " (s5-approach "stealthy moving towards the " (s5-victim "wealthy robot.")))
						(t3b (ales "Ales") " " (s5-notice "notices") " a " (s5-thief "small robot") " in the shadows, " (s5-approach "making his way toward the distracted, " (victim "wealthy one") "."))
						(t4a (s5-realize "By the time " (ales "Ales") " realizes what is happening, the " (s5-thief "small robot") " is within an arms length of his aloof " (s5-victim "victim.")))
						(t5 (s5-p1-help (ales "Ales")" helped the " (s5-victim "victim") " by alerting him."))
				)
			)
			
			(:structure
				(phase1
					(:entities (ales :type robot) (robot :type animate)  (street :type inanimate) (victim :type robot) (thief :type robot) (city :type inanimate))
					(:expressions
						((sad city) :name s5-sad-city)
						((in street city) :name s5-in-street)
						((lost victim) :name s5-lost)
						((walks ales street) :name s5-walk)
						((see ales victim) :name s5-see)
						((rich victim) :name s5-rich)
						((notice ales thief) :name s5-notice)
						((approaches thief victim) :name s5-approach)

        				((fd-agonist ales phase1) :name s5-p1-agonist)
        				((fd-antagonist thief phase1) :name s5-p1-antagonist)
						((fd-move-tendency s5-p1-antagonist s5-approach phase1) :name s5-p1-move-antagonist)
						((fd-stronger s5-p1-antagonist phase1) :name s5-p1-strong-antagonist)
					)
				)
			)
		)
		
		(STORY-6-FACTORY-JOB
			(:discourse
				(:clauses
					(c0 (:s phase1 phase2))
					(phase1 (:s t1a t2a t3a t4a (:m phase1)))
					(phase2 (:s t5a t6a t7a t8a t9a t10a t11a
					        (:c (take-job (:s t12a) phase2 () (((accept ales job) :name s6-p2-accept)
																	   ((fd-move-tendency s6-p2-agonist s6-p2-accept phase1) :name s6-p2-move-agonist)))
								(reject-job (:s t12b) phase2 () (((reject ales job) :name s6-p2-reject)
																	   ((fd-move-tendency s6-p2-agonist s6-p2-reject phase1) :name s6-p2-move-agonist)))
								;;;The following choice is only available if a memory with quality "confidence" is in pool.
								(ask-better-job (:s t12c) phase2 ((better-job :type inanimate)) (((tell ales foreman better-job) :name s6-p2-ask)
																	   ((fd-move-tendency s6-p2-agonist s6-p2-ask phase1) :name s6-p2-move-agonist))))))
				)
			
				(:templates
					(t1a "After counting street numbers and puzzling over the scrawl of a 4, which turned out to be a 9,")
					(t2a (s6-notice (ales "Ales") " found a " (s6-in-door (s6-small-door "small " (door "door")) " in the side of an " (s6-big-factory "immense " (factory "factory"))) " that matched the Aristobots description."))
					(t3a "Soon after knocking, " (s6-open "a solid, " (workbot "grubby workbot") " opened up") " and greeted " (ales "Ales") " with a scowl of pure skepticism.")
					(t4a (s6-explain (ales "Ales") " stammered out his situation, describing in too great of detail the recent " (theft "theft incident,")))
				
					(t5a "to which the grubby " (workbot "workbot") " said nothing but stood aside and made way for " (s6-enter (ales "Ales") " to enter") ".")
					(t6a "With a grunt and a tilt of the head, the " (workbot "workbot") " motioned " (ales "Ales") " to follow.")
					(t7a "They walked past giant greasy machines made of tubbing and spinning apparati.")
					(t8a "Here and there robots hung precariously, sometimes 20 feet in the air, and fed, beat or massaged parts of the machine,")
					(t9a "not doing the work of production themselves, but insuring that the monsters would continue to live and be fruitful.")
					(t10a "Ales was delivered a workbot who, identifying himself as " (s6-give "the " (foreman "foreman") ", handed Ales a " (tools "harness and a wrench")) ", indicated a spot near the top of one of the machines and " (s6-explin-job "explained the " (job "job.")))
					(t11a "The " (foreman "foreman") " said nothing more, but provided a stare that could mean nothing more than '" (s6-work "get to work") "'")
				
					(t12a (s6-p2-accept "Ales takes the gear and heads for the ladder."))
					(t12b (s6-p2-reject "Ales shrugs and walks back the way he came. This isn't his kind of work."))
					;;;The following choice is only available if a memory with quality "confidence" is in pulled pool.
					(t12c (s6-p2-ask "Ales wants work, but this seems to grueling. He asks the foreman if there are alternatives."))
				)
			)
				
			(:structure
				(common
					(:entities (ales :type robot) (robot :type animate) (workbot :type robot) (factory :type inanimate))
					(:expressions
						((big factory) :name s6-big-factory)
					)
				)
				(phase1
					(:entities (door :type inanimate) (theft :type inanimate))
					(:expressions
						((small door) :name s6-small-door)
						((big factory) :name s6-big-factory)
						((notice ales door) :name s6-notice)
						((tell ales workbot theft) :name s6-explain)
						((in door factory) :name s6-in-door)
						((open workbot door) :name s6-open)
					
						((fd-agonist ales phase1) :name s6-p1-agonist)
						((fd-antagonist workbot phase1) :name s6-p1-antagonist)
						((fd-stronger s6-p1-agonist phase1) :name s6-p1-strong-agonist)
						((fd-move-tendency s6-p1-agonist s6-explain phase1) :name s6-p1-move-agonist)
					)
				)			
				(phase2
					(:entities (door :type inanimate) (theft :type inanimate) (foreman :type robot) (job :type inanimate) (tools :type inanimate))
					(:expressions
						((enter ales factory) :name s6-enter)
						((give foreman ales tools) :name s6-give)
						((tell foreman ales job) :name s6-explain-job)
						((work ales factory) :name s6-work)
					
						((fd-agonist ales phase2) :name s6-p2-agonist)
						((fd-antagonist workbot phase2) :name s6-p2-antagonist)
						((fd-move-tendency s6-p2-antagonist s6-work phase2) :name s6-p2-move-antagonist)
					)
				)			
			)
		)
		
		
	)
)