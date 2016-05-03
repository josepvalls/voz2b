from nltk import CFG
import nltk
import pprint
import collections
import math
import util
from operator import itemgetter,attrgetter
import settings
import logging
import itertools
import random

attribute_selection_rules = ''' 0.1599    2 Func. Position
 0.0877    4 Ratio Villain
 0.0872   15 Possession
 0.0855    3 Ratio Hero
 0.0769    8 Ratio Other
 0.0755   17 Becoming_aware
 0.0754   13 Motion
 0.0691   24 Manipulation
 0.0678   23 Communication
 0.0655   16 Self_motion
 0.0647   21 Giving
 0.0642    5 Ratio Tester
 0.0641   19 Ingestion
 0.0622   26 Forming_relationships
 0.0608   14 Cause_motion'''.splitlines()

logger = logging.getLogger(__name__)

function_list = [None] + 'alpha beta gamma delta epsilon zeta eta theta lambda A a B C depart D E F G H J I K return Pr Rs o L M N Q Ex T U W'.split()

def main():
    logging.root.setLevel(logging.ERROR)
    #do_print_markov(0.5)
    #do_recognize_to_table(0.5)
    #return
    #do_recognize()

    fp = SequentialFunctionPredictor(laplacian_beta=0.5,num_attributes_to_include=10)
    #dataset = fp.predict_knn()
    fp.predict_mcts(epsilon_greedy=0.5)
    accuracy = fp.eval_dataset_accuracy(fp.narratives)
    ranks = fp.eval_dataset_rank(fp.narratives)
    print accuracy,util.describe_distribution(ranks)




class NarrativeFunctionData(object):
    def __init__(self,attributes,label):
        self.attributes,self.label=attributes,label

class NarrativeData(object):
    def __init__(self,story):
        self.story = story #type:int
        self.functions = [] #type: list[NarrativeFunctionData]
        self.predictions = [] #type: list[string]
        # used to guide MCTS
        self.distributions = [] #type: list[list[float]]
        # used for MCTS
        self.parent = None #type: NarrativeData
        self.children = {} #type: dict(string,NarrativeData)
        self.best_child_cache = None #type: string
        self.visits = 1
        self.payout = 0.0
    def new_child(self,prediction):
        nc = NarrativeData(self.story)
        nc.functions = self.functions
        nc.predictions = self.predictions
        nc.predictions.append(prediction)
        nc.parent = self
        return nc



class SequentialFunctionPredictor(object):
    def select_attributes(self,attributes,rules):
        new_attributes = []
        indices = []
        for rule in rules:
            index = int(rule.strip().split()[1])-1 # Weka is not 0-based
            indices.append(index)
            new_attributes.append(self.attributes[index])
        self.attributes = new_attributes

        new_attributes = []
        for vector in attributes:
            new_vector = []
            for index in indices:
                new_vector.append(vector[index])
            new_attributes.append(new_vector)
        return new_attributes

    def __init__(self,laplacian_beta=1.0,num_attributes_to_include=0):
        # init
        self.n = 5
        self.laplacian_beta = laplacian_beta

        self.markov_table = None

        # Load dataset
        self.stories = range(1,16)+[1001,1002,1003]
        filtered = '_filtered'
        story_indices = [int(i.strip()) for i in open('/Users/josepvalls/voz2/tool_corpus_functions_summary/story_indices%s.txt' % filtered).readlines()]
        dataset = [i.strip().split('\t') for i in open('/Users/josepvalls/voz2/tool_corpus_functions_summary/tool_corpus_functions_summary_5_dist%s.tsv'%filtered).readlines()]
        self.attributes = dataset[0][0:-1]
        self.weights = [1.0 for _ in self.attributes]
        dataset = dataset[1:]
        labels = [i[-1] for i in dataset]
        attributes = [[float(j) for j in i[0:-1]] for i in dataset]
        if num_attributes_to_include:
            attributes = self.select_attributes(attributes,attribute_selection_rules[0:num_attributes_to_include])
        self.narratives = [NarrativeData(i) for i in self.stories] #type: list[NarrativeData]
        for story,attributes,label in zip(story_indices,attributes,labels):
            self.narratives[self.stories.index(story)].functions.append(NarrativeFunctionData(attributes,label))

    def get_training_dataset(self,current_story):
        training = []
        for narrative in self.narratives:
            if narrative.story==current_story:
                pass
            else:
                training += narrative.functions
        return training

    def predict_knn(self):
        for test in self.narratives:
            training = self.get_training_dataset(test.story)
            logger.info('cross validation on story %d training %d test %d' % (test.story,len(training),len(test.functions)))
            for function in test.functions:
                distribution = self.probabilistic_distribution_knn(training,function,self.n,self.laplacian_beta)
                test.distributions.append(distribution)
                test.predictions.append(self.probabilistic_assignment(distribution))
        return self.narratives
    def init_distributions(self,test,training):
        for function in test.functions:
            distribution = self.probabilistic_distribution_knn(training,function,self.n,self.laplacian_beta)
            test.distributions.append(distribution)
    def predict_mcts(self,epsilon_greedy=0.1):
        for test in self.narratives:
            training = self.get_training_dataset(test.story)
            logger.info('cross validation on story %d training %d test %d' % (test.story,len(training),len(test.functions)))
            self.init_distributions(test,training)
            self.learn_markov(test.story)
            test.predictions = self.mcts(test,training).predictions
            del test.children

    def mcts(self,test,training):
        for _ in xrange(1000):
            self.mcts_get_child(test,epsilon_greedy=0.5)
        return self.get_most_visited(test)
    def get_most_visited(self,node):
        if len(node.predictions)<len(node.functions):
            child = sorted([i for i in node.children.values()],key=attrgetter("visits"))[-1]
            return self.get_most_visited(child)
        else:
            return node

    def mcts_get_child(self,node,epsilon_greedy):
        assert isinstance(node,NarrativeData)
        # selection
        if random.random < epsilon_greedy:
            choice = random.choice(function_list[1:])
        else:
            if not node.best_child_cache:
                prev_pred = None if not node.parent else node.parent.predictions[-1]
                choice = max([(self.get_transition_probability(prev_pred,i),i) for i in function_list[1:]])[1]
                node.best_child_cache = choice
            choice = node.best_child_cache

        if choice in node.children.keys():
            self.mcts_get_child(node.children[choice],epsilon_greedy)
        else:
            # expansion
            if len(node.predictions)<len(node.functions):
                current = node.new_child(choice)
                node.children[choice] = current
            else:
                # cannot expand, we are at the bottom
                current = node
            # playthrough
            self.mcts_play_propagate(current)
    def mcts_play_propagate(self,node):
        predictions = node.predictions
        while len(predictions)<len(node.functions):
            predictions.append(random.choice(function_list[1:]))
        payout = self.eval_narrative_accuracy_predictions(node,predictions)
        while node:
            node.visits+=1
            node.payout+=payout
            node = node.parent



    def distance_euclidean(self,c1,c2):
        return math.sqrt(
                         sum([1.0*(a-b)**2 for a,b in zip(c1.attributes,c2.attributes)])
                         /
                         len(self.attributes)
                         )
    def distance_euclidean_weighted(self,c1,c2):
        return math.sqrt(
                         sum([1.0*w*(a-b)**2 for a,b,w in zip(c1.attributes,c2.attributes,self.weights)])
                         /
                         sum(self.weights)
                         )

    def probabilistic_assignment(self,distribution):
        return function_list[sorted(enumerate(distribution), key=itemgetter(1), reverse=True)[0][0]]

    def probabilistic_distribution_knn(self,training,target,n,laplacian_beta):
        instances = self.get_knn(training,target,n)
        # TODO return some sort of sparse object instead of the full list
        if laplacian_beta:
            total = 1.0*len(instances)+laplacian_beta*(len(function_list)-1)
        else:
            total = 1.0*len(instances)
        distribution = [laplacian_beta for _ in function_list]
        for i in instances:
            distribution[function_list.index(i.label)]+=1
        if total:
            distribution = [1.0*i/total for i in distribution]
        return distribution
    def eval_narrative_accuracy_predictions(self,narrative,predictions):
        total = 0
        eq = 0
        for example,prediction in zip(narrative.functions,predictions):
            total +=1
            eq +=1 if example.label==prediction else 0
        return 1.0*eq/total


    def eval_dataset_accuracy(self,dataset):
        total = 0
        eq = 0
        for narrative in dataset:
            for example,prediction in zip(narrative.functions,narrative.predictions):
                total +=1
                eq +=1 if example.label==prediction else 0
        return 1.0*eq/total

    def eval_dataset_rank(self,dataset):
        ranks = []
        for narrative in dataset:
            for example,distribution in zip(narrative.functions,narrative.predictions):
                evals = sorted(zip(distribution,function_list),reverse=True)
                rank = 0
                for k,group in itertools.groupby(evals,key=itemgetter(0)):
                    if example.label in list(group):
                        break
                    rank +=1
                ranks.append(rank)
        return ranks
    def get_knn(self,training,target,n):
        return [i[1] for i in sorted([(self.distance_euclidean(target,c),c) for c in training])[0:(min(n,len(training)))]]
    def get_1nn(self,training,target,n):
        best = None
        best_d = 0.0
        for c in training:
            d = self.distance_euclidean(target,c)
            if best==None or d<best_d:
                best = c
                best_d = d
        return best

    def learn_markov(self,exclude):
        table = collections.defaultdict(lambda:collections.defaultdict(lambda:0))
        for narrative in self.narratives:
            if narrative.story==exclude: continue
            for a,b in zip([None]+narrative.functions[0:-1],narrative.functions):
                table[a][b]+=1
        self.markov_table = table
    def get_transition_probability(self,f0,f1):
        if self.laplacian_beta:
            total = sum(self.markov_table[f0].values())+self.laplacian_beta*(len(function_list)-1)
        else:
            total = sum(self.markov_table[f0].values())
        if self.markov_table[f0][f1]:
            return ((1.0*self.markov_table[f0][f1]+self.laplacian_beta)/total)
        else:
            return self.laplacian_beta/total
    def select_policy(self):
        return self.select_policy_ucb()
    def select_policy_ucb(self):
        pass
    def select_policy_egreedy(self):
        pass




if __name__=="__main__":
    main()



# current features

