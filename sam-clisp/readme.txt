Riu v1.0
Jichen Zhu and Santiago Ontañón

A.- About Riu:

Riu is an interactive narrative system which focuses on narrating the inner world about the main character, Ales. 
Ales is a robot, who has initially lost all of its memories. As the story progresses, Ales might recall some of those
memories if he finds himself in a situation similar to one of his past memories.

In order to achieve the desired narrative effect, Riu uses computational analogy to implement the memory recall of Ales,
as well as his imagination. When Ales is faced with a decision point (where the user can provide input), he will imagine
the outcome of each of the options, and refuse to execute some of them if he does not like the outcome he imagines.

For more details about Riu, we refer the reader to the following research papers:

1.- Santiago Ontañón and Jichen Zhu (2010) Story and Text Generation through Computational Analogy in the Riu System. 
	In Artificial Intelligence and Interactive Digital Entertainment (AIIDE 2010), pp. 51-56.
2.- Jichen Zhu and Santiago Ontañón (2010) Story Representation in Analogy-Based Story Generation in Riu. 
	In proceedings of IEEE Computational Intelligence in Games (IEEE-CIG 2010). pp. 435-442.



B.- Installing and Running Riu:

Riu is coded in Lisp, and is designed to work in CLISP. To "install" it, follow these steps:
1.- Create a folder XXX in your computer where you want Riu to be installed 
2.- Copy all of the Riu files to XXX
4.- To run Riu, simply open CLISP from folder XXX, then type:
	(load "riu.lisp")
	(riu)
	
The files in the "stories" folders contain the main story line of Riu, as well as the memories of Ales. By editing those files,
it is possible to alter the story told by Riu. 

The SME folder contains the source code of the Structure Mapping Engine (SME) algorithm (by Falkenhaimer, Forbus and Gentner), which 
is the algorithm internally used by Riu.

To change the behavior of Ales, you can alter the default values of its personality in the file "Riu-bdi-lisp". By default,
Ales is programmed with the goal to be happy, but this can be changed by setting the goal to "sad" in the method "init-Ales-BDI"
in such file. Also, by changing the last value of the BDI model of Ales (it's level of intentionality) we can achieve the following
effects: setting the value to 0.0 will result in Ales being a pure avatar (no memory recalls, and always following user commands),
setting the value to 1.0 will always recall memories, and always do what it wants (almost ignoring the user), intermediate values
produce more interesting mixed behaviors.

For questions or suggestions, contact:

Jichen Zhu
Santiago Ontanon
