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

;; Goals contains the lists of entities and expressions that advance the goal and the ones that hamper the goal
(setf *riu-GOALS*
	'((happy 
		(happy fun play cute)
		(sad dead bored work force rusty awkward))
	  (sad 
		(sad dead bored work force rusty awkward)
		(happy fun play cute))
	)
)

(setf *riu-MEMORY*
	'(
		(MEMORY-1-BIRD-DEATH
			(:discourse
				(:clauses
					(c0 (:s phase1 phase2))
					(phase1 (:s (:a t1a t1b) (:a t2a t2b)))
					(phase2 (:s t3))
				)				
				(:templates
					(t1a (m1-have (ales "Ales") " used to have a " (bird "bird")) " when he was " (m1-young "young") ".")
					(t1b (m1-have (ales "Ales") " had a " (bird "bird")) " many years ago.")
					(t2a (m1-play (ales "Ales") " played a lot with the " (bird "bird")) " and " (m1-happy "was very happy") ".")
					(t2b (m1-play (ales "Ales") " used to play with the " (bird "bird")) " and " (m1-happy "was very fond of it") ".")
					(t3  "But " (m1-dead "the " (bird "bird") " died") ", " (m1-sad "leaving " (ales "him") " really sad") ".")
					
					(bk1 (c-has-beak "birds have a " (beak "beak")))
					(bk2 (c-beak-color "flaming " (red "red") "-coloured " (beak "beak")))
					(bk3 (c-has-wings (bird "bird") " has " (wings "wings")))
					(bk4 (c-has-feathers (wings "wings") " have " (feathers "feathers")))
					(bk5 (c-wing-color (gold "golden") "-coloured " (wings "wings")))
					(bk5 (c-has-cage (bird "bird") " has a " (cage "cage")))
				)
			)
			
			(:structure
				(common
					(:entities (ales :type robot) (robot :type animate) (bird :type animal) (animal :type bird) 
							   (wings :type inanimate) (beak :type inanimate) (feathers :type inanimate) (cage :type location) (location :type inanimate)
							   (gold :type color) (red :type color) (color :type inanimate))
					(:expressions  
						((young ales) :name m1-young)
						((have ales bird) :name m1-have)
						((cute bird) :name m1-cute)
						((play ales bird) :name m1-play)
						((have bird wings) :name c-has-wings)
						((has-color wings gold) :name c-wing-color)
						((has-color beak red) :name c-beak-color)
						((have wings feathers) :name c-has-feathers)
						((have bird cage) :name c-has-cage)
					)
				)
				(phase1
					(:entities)
					(:expressions  
						((happy ales) :name m1-happy)
						
						((fd-agonist ales phase1) :name m1-p1-agonist)
						((fd-move-tendency m1-p1-agonist m1-play phase1) :name m1-p1-move-agonist)
						((fd-stronger m1-p1-agonist phase1) :name m1-p1-strong-agonist)
					)
				)
				(phase2
					(:entities)
					(:expressions
						((dead bird) :name m1-dead)
						((sad ales) :name m1-sad)

						((fd-agonist ales phase2) :name m1-p2-agonist)
						((fd-antagonist m1-dead phase2) :name m1-p2-antagonist)
						((fd-move-tendency m1-p2-agonist m1-play phase2) :name m1-p2-move-agonist)
						((fd-stronger m1-p2-antagonist phase2) :name m1-p2-strong-antagonist)
					)
				)
			)
		)
		
		(MEMORY-2-LONG-THEFT
			(:discourse
				(:clauses
					(c0 (:s phase1 phase2 phase3 (:a phase4a phase4b phase4c)))
					(phase1 (:s t1 t2 t2a))
					(phase2 (:s t3 t4 t5))
					(phase3 (:s t6 t61 t7 t8))
					(phase4a (:s t9a t10a t11a))
					(phase4b (:s t9b t10b t11b t12b t13b))
					(phase4c (:s t9c t10c t11c t12c 13c))
				)
				(:templates
					(t1 "Around " (ales "Ales") ", " (c-glimy "the " (street "street") " was grimy with " (rain "morning rain") " and " (soot "industrial soot")) ".")
					(t2 (c-first-time (ales "He") "'d never seen anything like " (polis "the city") " before, it was his first day in " (polis "Polis")) ".")
					(t2a "There had to be a " (job "job") " somewhere around here for him!")

					(t3 (p1-help "'Look out, a " (thief "thief") "!', " (ales "Ales") " heard himself calling."))					
					(t4 "Ahead, " (p2-see (aristobot "the aristobot") ", whirled and spotted the would-be " (thief "pick-pocketer") " " (aristobot "he") "'d been warned of."))
					(t5 (p2-angry (aristobot "He") " tossed a quick glower in the " (p2-leave "retreating " (thief "thief")) "'s direction"))
					
					(t6 (p3-greet (aristobot "The aristobot") " then cast his " (arms "arms") " in affectionate greeting towards " (ales "Ales") "."))
					(t61 (p3-call "'Come here, "  (ales "my boy") "!'"))
					(t7 (p3-reluctant "With a slight sense of reluctance, " (p3-approach (ales "Ales")) " approached " (aristobot "the aristobot")) ", whose " (hat-color "steel-brushed " (hat "top hat")) " alone represented more wealth than " (ales "Ales") " had ever seen.")
					(t8 "'" (aristobot "Octavicon Alcorn") ", in your debtedness "  (ales "son") ",' " (p3-offer "thrusting his " (hand "hand") " forward in a gesture of greeting."))
					
					(t9a  (p4a-shake (ales "Ales") " gripped " (aristobot "the aristobot") "'s " (hand "hand") " confidently") " and smiled.")
					(t10a (p4a-happy-aristobot (aristobot "Octavion") " beamed and invigorated his handshake, explaining to " (ales "Ales") " his immense gratitude."))
					(t11a "After exchanging pleasantries, with "  (p4a-tell (ales "Ales") " relating his " (travels "recent travels from " (country-side "the country-side") " into " (polis "the city"))) ", " 
						  (p4a-offer (aristobot "Octavion") " offered a token of his appreciation: " (c-have "a " (p4a-in-job (job "job") " in " (factory "one of his factories")))) ".")
					
					(t9b  (p4a-shake (ales "Ales") " passively shook " (aristobot "the aristobot") "'s " (hand "hand")) ", self-conscious of the difference between his limp squeeze and " (aristobot "the aristobot") "'s vice-like grip.")
					(t10b (p4a-tell (aristobot "Octavion") " politely listened as " (ales "Ales") " explained his recent arrival to " (polis "the city")) ", all the while " (p4b-use "using an " (handkerchief "exquisite microfiber handkerchief") " to buff the jewel-encrusted " (hand "palm") " he'd shaken with") ".")
					(t11b "'" (ales "You") "'ve done " (aristobot "me") " a service, sir, now let " (aristobot "me") " provide one in turn,' the " (aristobot "aristobot") " said, when " (ales "Ales") " seemed finished.")
					(t12b (p4b-give (aristobot "He") " scribbled a " (coordinates "coordinate") " and handed it to " (ales "Ales")) ".")
					(t13b (p4a-offer "That's his "  (factory "factory") ", " (aristobot "he") " explained, go there and " (c-have (ales "Ales") " can find a " (job "job"))) ".")
					
					(t9c  (p4c-ashamed (ales "Ales") " cast his eyes downward and avoided " (hand (aristobot "the aristobot") "'s hand") ", unsure of how to interact") ".")
					(t10c (p4a-tell "As " (ales "Ales") " began to mumble how " (ales "he") " just arrived in " (polis "Polis")) ", " (p4c-shut-up (aristobot "Alcron") " cut him off") ".")
					(t11c (p4b-give "'Thank you " (ales "boy") ", here is a token of my appreciation,' " (aristobot "he") " said, handing " (ales "Ales")" a swiftly jotted " (coordinates "coordinate")) ".")
					(t12c (p4a-offer "That's "  (factory "his factory") ", " (aristobot "he") " explained, go there and " (c-have (ales "Ales") " can find a " (job "job"))) ".")
					(t13c (p4c-leave "Before " (ales "Ales") " could muster a reply, " (aristobot "the aristobot") " had spun on his heel and headed the other direction, calling 'good day' over his shoulder") ".")
				)
			)
			
			(:structure
				(common
					(:entities (robot :type animate) (location :type inanimate)
							   (ales :type robot) (aristobot :type robot) 
							   (street :type location) (polis :type location)
							   (hand :type inanimate) (hat :type inanimate) (rain :type inanimate) (soot :type inanimate)
							   (job :type inanimate) (steel :type color) (color :type inanimate) (arms :type inanimate))
					(:expressions  
						((in ales street) :name c-in-ales)
						((in street polis) :name c-in-street)
						((in aristobot street) :name c-in-aristobot)
						((glimy street) :name c-glimy)
						((rich aristobot) :name c-rich)
						((poor ales) :name c-poor)
						((first-time ales polis) :name c-first-time)
						((have ales job) :name c-have)
						((wants ales c-have) :name c-wants)
						((have aristobot hand) :name c-have-hand)
						((have aristobot arms) :name c-have-arms)
						((have arms hand) :name c-arms-have-hands)
						((have aristobot hat) :name c-have-hat)
						((rich hat) :name c-rich-hat)
						((has-color hat steel) :name hat-color)
					)
				)
				
				(phase1
					(:entities (thief :type robot))
					(:expressions

						((fd-agonist ales phase1) :name p1-agonist)
						((fd-antagonist polis phase1) :name p1-antagonist)
						((fd-move-tendency p1-agonist c-have phase1) :name p1-move-agonist)
						((fd-rest-tendency p1-antagonist c-have phase1) :name p1-rest-antagonist)
						((fd-stronger p1-rest-antagonist phase1) :name p1-strong-antagonist)
					)
				)
				(phase2
					(:entities (thief :type robot))
					(:expressions  
						((help ales aristobot) :name p2-help)
						((see ales thief) :name p2-see)
						((in thief street) :name p2-in-thief)
						((see aristobot thief) :name p2-see)
						((angry aristobot thief) :name p2-angry)
						((leave thief) :name p2-leave)

						((fd-agonist ales phase2) :name p2-agonist)
						((fd-antagonist thief phase2) :name p2-antagonist)
						((fd-move-tendency p2-agonist p2-help phase2) :name p2-move-agonist)
						((fd-rest-tendency p2-antagonist p2-help phase2) :name p2-rest-antagonist)
						((fd-stronger p2-move-agonist phase2) :name p2-strong-agonist)
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
					(:entities (country-side :type location) (factory :type location) (travels :type inanimate))
					(:expressions
						((shake ales aristobot hand) :name p4a-shake)
						((happy aristobot) :name p4a-happy-aristobot)
						((tell ales aristobot travels) :name p4a-tell)
						((offer aristobot ales c-have) :name p4a-offer)
						((have aristobot factory) :name p4a-have-factory)
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
						((offer aristobot ales c-have) :name p4a-offer)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((have aristobot factory) :name p4a-have-factory)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((in job factory) :name p4a-in-job)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains

						((fd-agonist ales phase4b) :name p4b-agonist)
						((fd-antagonist aristobot phase4b) :name p4b-antagonist)
						((fd-rest-tendency p4b-agonist p4a-shake phase4b) :name p4b-rest-agonist)
						((fd-move-tendency p4b-antagonist p4a-shake phase4b) :name p4b-move-antagonist)
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
						((offer aristobot ales c-have) :name p4a-offer)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
						((have aristobot factory) :name p4a-have-factory)  ;; since it's the same as in another phase, it has to have the same name, otherwise SME complains
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
