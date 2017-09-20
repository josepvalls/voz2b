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


(defvar *riu-MEMORY* nil)

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
				)
			)
			
			(:structure
				(common
					(:entities (ales :type robot) (robot :type animate) (bird :type animate))
					(:expressions  
						((young ales) :name m1-young)
						((have ales bird) :name m1-have)
						((animal bird) :name m1-animal)
						((cute bird) :name m1-cute)
						((play ales bird) :name m1-play)
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

		(MEMORY-2-OIL-CHANGE
			(:discourse
				(:clauses
					(c0 (:s phase1 phase2))
					(phase1 (:s t1 t2))
					(phase2 (:s t3 (:a t4a t4b)))
				)				
				(:templates
					(t1 (ales "Ales") " remembered the " (garage "garage") " in which " (ales "he") " had his first " (m2-p1-oil (oil "oil") " change") ", " (m2-p1-red "it was all " (red "red") "."))
					(t2 (owner "His owners") " said " (m2-p1-rusty "he was rusty") ", and " (m2-p1-force "forced " (ales "him") " to " (m2-p1-oil "change his " (oil "oil"))) ", he was a fool to accept.")
					(t3 (m2-p2-awkward (ales "Ales") " felt very awkward afterwards") ", and decided that " (ales "he") " would have to be really rusty before his next " (m2-p1-oil (oil "oil") " change") ".")
					(t4a (ales "He") " wondered why no one ever complained about that.")
					(t4b "How come no one ever complained about " (m2-p1-oil (oil "oil") " changes") "?")
				)
			)
			
			(:structure
				(common
					(:entities (ales :type robot) (robot :type animate) (oil :type inanimate) (garage :type inanimate) (owner :type inanimate) (red :type inanimate))
					(:expressions  
						((in ales garage) :name m2-p1-in)
						((has-color garage red) :name m2-p1-red)
						((color red) :name m2-red)

						((change ales oil) :name m2-p1-oil)
					)
				)
				(phase1
					(:entities)
					(:expressions  
						((rusty ales) :name m2-p1-rusty)
						((force owner ales m2-p1-oil) :name m2-p1-force)
						((accept ales m2-p1-oil) :name m2-p1-accept)

						((fd-agonist ales phase1) :name m2-p1-agonist)
						((fd-antagonist owner phase1) :name m2-p1-antagonist)
						((fd-rest-tendency m2-p1-agonist m2-p1-oil phase1) :name m2-p1-move-agonist)
						((fd-stronger m2-p1-antagonist phase1) :name m2-p1-strong-agonist)
						((fd-move-tendency m2-p1-antagonist m2-p1-oil phase1) :name m2-p1-move-antagonist)
					)
				)
				(phase2
					(:entities)
					(:expressions
						((awkward ales) :name m2-p2-awkward)

						((fd-agonist ales phase2) :name m2-p2-agonist)
						((fd-antagonist owner phase2) :name m2-p2-antagonist)
					)
				)
			)
		)

        (MEMORY-3-TREE
			(:discourse
				(:clauses
					(c0 (:s phase1 phase2))
					(phase1 (:s (:a t1a t1b t1c) (:a t2a t2b)))
					(phase2 (:s t3))
				)				
				(:templates
					(t1a (m3-tree-small "The little " (tree "willow tree")))
					(t1b (m3-tree-small "The young " (tree "pine tree")))
					(t1c (m3-tree-small "The newly-planted " (tree "tree")))
					
					(t2a (m3-p1-in "behind the " (playground "playground")))
					(t2b (m3-p1-in "by the " (pond "pond") " in the " (playground "playground")))
					
					(t3 (m3-p2-growth "grew into a big," (silver-tree " silver " (tree "tree."))))
				)
			)
			
			(:structure
				(common
					(:entities (tree :type inanimate) (playground :type location) (pond :type location) (gravity :type inanimate))
					(:expressions
						((in tree playground) :name m3-p1-in)
						((by playground pond) :name m3-p1-by)
						((grow tree) :name m3-p2-growth)
					)
				)
				(phase1
					(:entities)
					(:expressions  
						((young tree) :name m3-tree-young)
						((small tree) :name m3-tree-small)
						
						((fd-agonist tree phase1) :name m3-p1-agonist)
						((fd-antagonist gravity phase1) :name m3-p1-antagonist)
						((fd-stronger m3-p1-agonist phase1) :name m3-p1-strong-agonist)
						((fd-move-tendency m3-p1-agonist m3-p2-growth phase1) :name m3-p1-move-agonist)
					)
				)
				(phase2
					(:entities (tree :type inanimate) (silver :type inanimate) (phase :type inanimate) (phase2 :type phase))
					(:expressions
						((has-color tree silver) :name silver-tree)
						((color silver))
						
						((fd-agonist tree phase2) :name m3-p2-agonist)
						((fd-antagonist gravity phase2) :name m3-p2-antagonist)
						((fd-stronger m3-p2-agonist phase2) :name m3-p2-strong-agonist)
						((fd-move-tendency m3-p2-agonist m3-p2-growth phase2) :name m3-p2-move-agonist)						
					)
				)
			)
		)
		
		(MEMORY-4-PAINTER-FAIL
			(:discourse
				(:clauses
					(c0 (:s phase1 phase2))
					(phase1 (:s t1 t2))
					(phase2 (:s t3 (:a t4a t4b) t5))
				)				
				(:templates
					(t1 (ales "Ales") " has always wanted to " (m4-learns "be a " (painter "painter")) ".")
					(t2 (ales "He") " was even " (m4-learns "learning to be a " (painter "painter")) ".")
					(t3 "But the long hours of his " (work "day job") " leave him very little time to " (m4-learns "practice,") (m4-gives-up " eventually giving up."))
					(t4a "He stores different " (things "odd-looking objects") " and hopes ")
					(t4b "He collects all kinds of " (things "strange things") " and dreams that")
					(t5 (m4-learns "one day he can draw them."))
				)
			)
			(:structure
				(common 
					(:entities (ales :type robot) (robot :type animate) (work :type inanimate) (painter :type inanimate) (things :type inanimate))
					(:expressions
						((learns ales painter) :name m4-learns)
						((have ales work) :name m4-have-work)
					)
				)
				(phase1
					(:entities)
					(:expressions
						((fd-agonist ales phase1) :name m4-p1-agonist)
						((fd-antagonist work phase1) :name m4-p1-antagonist)
						((fd-move-tendency m4-p1-agonist m4-learns phase1) :name m4-p1-move)
						((fd-stronger m4-p1-antagonist phase1) :name m4-p1-strong)
					)
				)
				(phase2
					(:entities)
					(:expressions
						((gives-up ales painter) :name m4-gives-up)
						((have ales things) :name m4-have-things)

						((fd-agonist ales phase2) :name m4-p2-agonist)
						((fd-antagonist work phase2) :name m4-p2-antagonist)
						((fd-move-tendency m4-p2-agonist m4-learns phase2) :name m4-p2-move)
						((fd-stronger m4-p2-antagonist phase2) :name m4-p2-strong)
					)
				)
			)
		)
		
		(MEMORY-5-PAINTER
			(:discourse
				(:clauses
					(c0 (:s phase1 phase2))
					(phase1 (:s t1))
					(phase2 (:s (:a t2a t2b) t3))
				)				
				(:templates
					(t1 (ales "Ales") " has always wanted to " (m5-learns "be a " (painter "painter")) ".")
					(t2a "The " (work "long hours of his day job") " leaves only a little time to practice.")
					(t2b (m5-rusty-p2-hands "His " (joint "rusty joints ") "do not allow " (m5-have-hands "his hands") " enough flexibility to maneuver the brush."))
					(t3 "But he examines every " (things "interesting object") " he lays his eyes on and " (m5-p2-paints "tries to draw " (things "them")) " at night by memory.")
				)
			)
			(:structure
				(common 
					(:entities (ales :type robot) (robot :type animate) (work :type inanimate) (painter :type inanimate) (things :type inanimate) (joint :type inanimate) (hands :type inanimate))
					(:expressions
						((learns ales painter) :name m5-learns)
						((have ales work) :name m5-have-work)
						((have ales hands) :name m5-have-hands)
					)
				)
				(phase1
					(:entities)
					(:expressions

						((fd-agonist ales phase1) :name m5-p1-agonist)
						((fd-move-tendency m5-p1-agonist m5-learns phase1) :name m5-p1-move)
						((fd-stronger m5-p1-agonist phase1) :name m5-p1-strong)
					)
				)
				(phase2
					(:entities)
					(:expressions
						((painter ales things) :name m5-p2-paints)
						((rusty hands) :name m5-rusty-p2-hands)
						
						((fd-agonist ales phase2) :name m5-p2-agonist)
						((fd-antagonist work phase2) :name m5-p2-antagonist-work)
						((fd-antagonist m5-rusty-p2-hands phase2) :name m5-p2-antagonist-rusty)
						((fd-move-tendency m5-p2-agonist m5-learns phase2) :name m5-p2-move)
						((fd-stronger m5-p2-agonist phase2) :name m5-p2-strong)
					)
				)
			)
		)
	)
)
