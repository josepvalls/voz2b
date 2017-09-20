#|

Note: the names used for clauses, templates, etc. have to be unique only inside each one of the story blocks or memories

For clauses, there are the following constructs:
:s -> sequence
:a -> alternative (one will be picked at random)
:gt -> go to (:gt <branch-name)
:c -> choice: user input (:c (<choice1> <branch1-name> <phase1-name> (<additional-entities>) (<additional-expressions>))
							 (<choice1> <branch1-name> <phase1-name> (<additional-entities>) (<additional-expressions>))
:m -> memory retrieval trigger (the argument refers to the "phase" with which analogy will be made)
:r -> repetition


The first phase of each memory is augmented with force dynamics since is the one used for analogy

|#


(defvar *riu-GOALS* nil)
(defvar *riu-MEMORY* nil)

(setf *riu-MEMORY*
	'(
	#|
		(MEMORY-1-LONG-SERVO
			(:discourse
				(:clauses
					(c0 (:s phase1 phase2 phase3))
					(phase1 (:s t1 t2 t3 t4 t5 t6 t7 t8 t9))
					(phase2 (:a (:s t10a t11a) (:s t10b t11b) t10c t10d (:s t10e t11e t12e)))
					(phase3 (:s t13 t14 t15 t16))
				)				
				(:templates
					(t1 "Minutes earlier, Ales was drilling into one of the last servo motor casings of the evening")
					(t2 "when he felt the sharp, familiar pull of an electromagnet grappling his elbow")
					(t3 "It was the final stretch of a double shift at the Turing Servo Co. Production Facility and memories had begun to distract Ales, dangerous on a job this difficult.")
					(t4 "The trick, when reaching into each servo motor casing to bore the final screw, was to avoid any of the prematurely activated electromagnets, possibly tripped as they jostled along on the conveyor belt deeper inside the factory.")
					(t5 "Reach inside the casing, bore the screw and remove his arm in time for the next motor to lurch into place.")
					(t6 "Do that a couple thousand times and the shift is over.")
					(t7 "A couple thousand more and the day is finished.")
					(t8 "Getting stuck to a magnet happens several times a shift, even to the best screw borers.")
					(t9 "Ales knows from experience that, when stuck, the best procedure is to walk with the lurch while working his arm loose, do the next motor doubly fast and get back on track.")
					
					(t10a "Ales moved with the servo motor down the assembly line, while trying to work his arm free.")
					(t11a "Ales used his free hand to reach in and speed up the process.")

					(t10b "Ales continued to pull, push and work leverage, but he slowly came to realize that he was only worsening his situation.")
					(t11b "With the timing of each lurch of the assembly line engrained on his circuitry, Ales knew that he would have to shift again soon.")

					(t10c "Ales moves again and continues, in a seemingly futile effort, to try freeing his arm.")

					(t10d "Ales moves again, but this time tries reaching his free arm to help escape from the casing.")
					
					(t10e "With frustration building, Ales alternates between flailing about, hoping that a wild move will free him, and resting against the casing, going over all of his options.")
					(t11e "The next lurch is coming and he knows that there is really only one option.")
					(t12e "Reluctantly, Ales reached inside the casing with his free arm.")
					
					(t13 "The golden rule of terminal screw drilling, plastered on a banner above Ales's work station, is 'never stick put arms into the servo motor casing, especially when freeing a limb.'")
					(t14 "But the more powerful, unspoken rule of the factory is that defective products mean less revenue, and more layoffs.")
					(t15 "Hoping that he could get back to work more quickly, Ales reached into the casing with his free arm.")
					(t16 "And then he really got himself into trouble.")
				)
			)
			
			(:structure
				(common
					(:entities)
					(:expressions  
					)
				)
				(phase1
					(:entities)
					(:expressions  
					)
				)
				(phase2
					(:entities)
					(:expressions
					)
				)
				(phase3
					(:entities)
					(:expressions
					)
				)
			)
		)
	|#
		
		(MEMORY-2-LONG-THEFT
			(:discourse
				(:clauses
					(c0 (:s phase1 phase2 phase3 (:a phase4a phase4b phase4c)))
					(phase1 (:s t1 t2 t3))
					(phase2 (:s t4 t5))
					(phase3 (:s t6 t61 t7 t8))
					(phase4a (:s t9a t10a t11a))
					(phase4b (:s t9b t10b t11b t12b t13b))
					(phase4c (:s t9c t10c t11c t12c 13c))
				)
				(:templates
					(t1 "Around " (ales "Ales") ", " (c-glimy "the " (street "street") " was grimy with " (rain "morning rain") " and " (soot "industrial soot")) ".")
					(t2 (c-first-time (ales "He") "'d never seen anything like " (polis "the city") " before, it was his first day in " (polis "Polis")) ".")
					(t3 (p1-help "'Look out, a " (thief "thief") "!', " (ales "Ales") " heard himself calling."))
					
					(t4 "Ahead, " (p2-see (aristobot "the aristobot") ", whirled and spotted the would-be " (thief "pick-pocketer") " " (aristobot "he") "'d been warned of."))
					(t5 (p2-angry (aristobot "He") " tossed a quick glower in the " (p2-leave "retreating " (thief "thief")) "'s direction"))
					
					(t6 (p3-greet (aristobot "The aristobot") " then cast his arms in affectionate greeting towards " (ales "Ales") "."))
					(t61 (p3-call "'Come here, "  (ales "my boy") "!'"))
					(t7 (p3-reluctant "With a slight sense of reluctance, " (p3-approach (ales "Ales")) " approached " (aristobot "the aristobot")) ", whose steel-brushed top hat alone represented more wealth than " (ales "Ales") " had ever seen.")
					(t8 "'" (aristobot "Octavicon Alcorn") ", in your debtedness "  (ales "son") ",' " (p3-offer "thrusting his " (hand "hand") " forward in a gesture of greeting."))
					
					(t9a  (p4a-shake (ales "Ales") " gripped " (aristobot "the aristobot") "'s " (hand "hand") " confidently") " and smiled.")
					(t10a (p4a-happy-aristobot (aristobot "Octavion") " beamed and invigorated his handshake, explaining to " (ales "Ales") " his immense gratitude."))
					(t11a "After exchanging pleasantries, with "  (p4a-tell (ales "Ales") " relating his " (travels "recent travels from " (country-side "the country-side") " into " (polis "the city"))) ", " 
						  (p4a-offer (aristobot "Octavion") " offered a token of his appreciation: a " (p4a-in-job (job "job") " in " (factory "one of his factories"))) ".")
					
					(t9b  (p4a-shake (ales "Ales") " passively shook " (aristobot "the aristobot") "'s " (hand "hand")) ", self-conscious of the difference between his limp squeeze and " (aristobot "the aristobot") "'s vice-like grip.")
					(t10b (p4a-tell (aristobot "Octavion") " politely listened as " (ales "Ales") " explained his recent arrival to " (polis "the city")) ", all the while " (p4b-use "using an " (handkerchief "exquisite microfiber handkerchief") " to buff the jewel-encrusted " (hand "palm") " he'd shaken with") ".")
					(t11b "'" (ales "You") "'ve done " (aristobot "me") " a service, sir, now let " (aristobot "me") " provide one in turn,' the " (aristobot "aristobot") " said, when " (ales "Ales") " seemed finished.")
					(t12b (p4b-give (aristobot "He") " scribbled a " (coordinates "coordinate") " and handed it to " (ales "Ales")) ".")
					(t13b (p4a-offer "That's "  (factory "his factory") ", " (aristobot "he") " explained, go there and " (ales "Ales") " can find a " (job "job")) ".")
					
					(t9c  (p4c-ashamed (ales "Ales") " cast his eyes downward and avoided " (hand (aristobot "the aristobot") "'s hand") ", unsure of how to interact") ".")
					(t10c (p4a-tell "As " (ales "Ales") " began to mumble how " (ales "he") " just arrived in " (polis "Polis")) ", " (p4c-shut-up (aristobot "Alcron") " cut him off") ".")
					(t11c (p4b-give "'Thank you " (ales "boy") ", here is a token of my appreciation,' " (aristobot "he") " said, handing " (ales "Ales")" a swiftly jotted " (coordinates "coordinate")) ".")
					(t12c (p4a-offer "That's "  (factory "his factory") ", " (aristobot "he") " explained, go there and " (ales "Ales") " can find a " (job "job")) ".")
					(t13c (p4c-leave "Before " (ales "Ales") " could muster a reply, " (aristobot "the aristobot") " had spun on his heel and headed the other direction, calling 'good day' over his shoulder") ".")
				)
			)
			
			(:structure
				(common
					(:entities (robot :type animate) (location :type inanimate)
							   (ales :type robot) (aristobot :type robot) 
							   (street :type location) (polis :type location) ;  
							   (hand :type inanimate) (rain :type inanimate) (soot :type inanimate))
					(:expressions  
						((in ales street) :name c-in-ales)
						((in street polis) :name c-in-street)
						((in aristobot street) :name c-in-aristobot)
						((glimy street) :name c-glimy)
						((rich aristobot) :name c-rich)
						((poor ales) :name c-poor)
						((first-time ales polis) :name c-first-time)
					)
				)
				
				(phase1
					(:entities (thief :type robot))
					(:expressions
						((help ales aristobot) :name p1-help)
						((in thief street) :name p1-in-thief)
						((see ales thief) :name p1-see)
						
						((fd-agonist ales phase1) :name p1-agonist)
						((fd-antagonist thief phase1) :name p1-antagonist)
						((fd-move-tendency p1-agonist p1-help phase1) :name p1-move-agonist)
						((fd-rest-tendency p1-antagonist p1-help phase1) :name p1-rest-antagonist)
						((fd-stronger p1-move-agonist phase1) :name p1-strong-agonist)
					)
				)
				(phase2
					(:entities (thief :type robot))
					(:expressions  
						((in thief street) :name p1-in-thief)	;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((see aristobot thief) :name p2-see)
						((angry aristobot thief) :name p2-angry)
						((leave thief) :name p2-leave)

						((fd-agonist aristobot phase2) :name p2-agonist)
						((fd-move-tendency p2-agonist p2-see phase2) :name p2-move-agonist)
					)
				)
				(phase3
					(:entities (thief :type robot))
					(:expressions
						((greets aristobot ales) :name p3-greet)
						((call aristobot ales) :name p3-call)
						((approaches ales aristobot) :name p3-approach)
						((reluctant ales p3-approach) :name p3-reluctant)
						((offer aristobot ales hand) :name p3-offer)

						((fd-agonist ales phase3) :name p3-agonist)
						((fd-antagonist aristobot phase3) :name p3-antagonist)
						((fd-rest-tendency p3-agonist p3-approach phase3) :name p3-rest-agonist)
						((fd-move-tendency p3-antagonist p3-approach phase3) :name p3-move-antagonist)
						((fd-stronger p3-move-antagonist phase3) :name p3-strong-antagonist)
					)
				)
				(phase4a
					(:entities (country-side :type location) (factory :type location) (travels :type inanimate) (job :type inanimate))
					(:expressions
						((shake ales aristobot hand) :name p4a-shake)
						((happy aristobot) :name p4a-happy-aristobot)
						((tell ales aristobot travels) :name p4a-tell)
						((offer aristobot ales job) :name p4a-offer)
						((in job factory) :name p4a-in-job)

						((fd-agonist ales phase4a) :name p4a-agonist)
						((fd-antagonist aristobot phase4a) :name p4a-antagonist)
						((fd-move-tendency p4a-agonist p4a-shake phase4a) :name p4a-move-agonist)
						((fd-move-tendency p4a-antagonist p4a-shake phase4a) :name p4a-move-antagonist)
						((fd-stronger p4a-move-antagonist phase4a) :name p4a-strong-antagonist)
					)
				)
				(phase4b
					(:entities (country-side :type location) (factory :type location) (travels :type inanimate) (handkerchief :type inanimate) (coordinates :type location) (job :type inanimate))
					(:expressions
						((shake ales aristobot hand) :name p4a-shake)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((tell ales aristobot travels) :name p4a-tell)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((use handkerchief hand) :name p4b-use)
						((give aristobot ales coordinates) :name p4b-give)
						((offer aristobot ales job) :name p4a-offer)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((in job factory) :name p4a-in-job)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains

						((fd-agonist ales phase4b) :name p4b-agonist)
						((fd-antagonist aristobot phase4b) :name p4b-antagonist)
						((fd-rest-tendency p4a-agonist p4a-shake phase4b) :name p4b-rest-agonist)
						((fd-move-tendency p4a-antagonist p4a-shake phase4b) :name p4b-move-antagonist)
						((fd-stronger p4b-move-antagonist phase4b) :name p4b-strong-antagonist)
					)
				)
				(phase4c
					(:entities (country-side :type location) (factory :type location) (travels :type inanimate) (coordinates :type location) (job :type inanimate))
					(:expressions
						((shake ales aristobot hand) :name p4a-shake)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((ashamed ales) :name p4c-ashamed)  
						((tell ales aristobot travels) :name p4a-tell)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((give aristobot ales coordinates) :name p4b-give)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((offer aristobot ales job) :name p4a-offer)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((in job factory) :name p4a-in-job)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((leave aristobot) :name p4c-leave)

						((fd-agonist ales phase4c) :name p4c-agonist)
						((fd-antagonist aristobot phase4c) :name p4c-antagonist)
						((fd-rest-tendency p4c-agonist p4a-shake phase4c) :name p4c-rest-agonist)
						((fd-move-tendency p4c-antagonist p4a-shake phase4c) :name p4c-move-antagonist)
						((fd-stronger p4c-rest-agonist phase4c) :name p4c-strong-agonist)
					)
				)
			)
		)		
	)
)
