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

function_list = 'alpha beta gamma delta epsilon zeta eta theta lambda A a B C depart D E F G H J I K return Pr Rs o L M N Q Ex T U W'.split()

MCTS_NODE_START_VALUE = 0.5**(len(function_list)*2) # this is 2 since its the joint probability of the ml and markov predictions
MCTS_ROUNDS_PER_NARRATIVE = 100000
LAPLACIAN_BETA_KNN = 0.5
LAPLACIAN_BETA_MARKOV = 0.5

def main():
    #logging.root.setLevel(logging.INFO)
    logging.root.setLevel(logging.ERROR)
    fp = SequentialFunctionPredictor(laplacian_beta_knn=LAPLACIAN_BETA_KNN,laplacian_beta_markov=LAPLACIAN_BETA_MARKOV,num_attributes_to_include=10)
    fp.predict_mcts(epsilon_greedy=0.1)
    accuracy = fp.eval_dataset_accuracy(fp.narratives)
    if True:
        ranks = fp.eval_dataset_rank(fp.narratives)
        print accuracy,util.describe_distribution(ranks)
    else:
        print accuracy



class NarrativeData(object):
    def __init__(self,story):
        self.story=story
        self.data = []
        self.predictions = []
        self.distributions = []

class NarrativeFunctionData(object):
    def __init__(self,attributes,label):
        self.attributes,self.label=attributes,label
        self.distribution_knn = []

class NarrativeFunctionPrediction(object):
    def __init__(self,prediction,parent):
        self.prediction = prediction
        self.parent = parent #type: NarrativeFunctionPrediction
        self.children = [] #type: list[NarrativeFunctionPrediction]
        self.visits = 0
        self.payout = 0.0
        self.value = MCTS_NODE_START_VALUE

class LearnedMarkovTable(object):
    def __init__(self,laplacian_beta,narratives,exclude):
        self.laplacian_beta = laplacian_beta
        self.markov_table = None
        self.learn_markov(narratives,exclude)
    def get_transition_probability(self,f0,f1):
        if self.laplacian_beta:
            total = sum(self.markov_table[f0].values())+self.laplacian_beta*(len(function_list)-1)
        else:
            total = sum(self.markov_table[f0].values())
        if self.markov_table[f0][f1]:
            return ((1.0*self.markov_table[f0][f1]+self.laplacian_beta)/total)
        else:
            return self.laplacian_beta/total
    def learn_markov(self,narratives,exclude):
        table = collections.defaultdict(lambda:collections.defaultdict(lambda:0))
        for narrative in narratives:
            if narrative.story==exclude.story: continue
            table[None][narrative.data[0].label]+=1
            for a,b in zip(narrative.data[0:-1],narrative.data[1:]):
                table[a.label][b.label]+=1
        self.markov_table = table

class MCTS(object):
    def __init__(self):
        self.max_depth = 0
        self.root = None #type: NarrativeFunctionPrediction
        self.narrative = None #type: NarrativeData
        self.markov_table = None #type: LearnedMarkovTable
    def search(self,narrative,markov_table,epsilon_greedy):
        self.narrative = narrative
        self.markov_table = markov_table
        self.max_depth = len(narrative.data)
        self.root = NarrativeFunctionPrediction(None,None)
        for i in xrange(MCTS_ROUNDS_PER_NARRATIVE):
            node = self.selection(self.root,0,epsilon_greedy)
            self.eval_node(node)
        return self.selection(self.root,0,0.0)
    def best_child(self,node):
        return max(node.children, key=lambda i:i.value)
    def selection(self,node,depth,epsilon_greedy):
        if depth==self.max_depth:
            #logger.info("Reached the end")
            return node
        if node.children:
            if random.random() < epsilon_greedy:
                choice = random.choice(node.children)
            else:
                choice = self.best_child(node)
            return self.selection(choice,depth+1,epsilon_greedy)
        else:
            # expand
            node.children = [NarrativeFunctionPrediction(i,node) for i in function_list[1:]]
            return node
    def get_predictions(self,node):
        predictions = []
        node_ = node
        while node_.parent:
            predictions.append(node_.prediction)
            node_ = node_.parent
        predictions.reverse()
        return predictions # the root node has a None prediction
    def get_distributions(self,node):
        distributions = []
        node_ = node
        while node_:
            distribution = [i.visits for i in node_.children]
            total = sum(distribution)
            if total:
                distribution = [1.0*i.visits/total for i in node_.children]
            distributions.append(distribution)
            node_ = node_.parent
        distributions.reverse()
        return distributions

    def eval_node(self,node):
        predictions = self.get_predictions(node)
        predictions2 = list(predictions)
        # random playout
        while len(predictions)<self.max_depth:
            predictions.append(random.choice(function_list))
        payout = self.eval_narrative_predictions(node,predictions)
        #print predictions2,payout,
        # backpropagate
        while node:
            node.visits+=1
            node.payout+=payout
            node.value = node.payout/node.visits
            #print node.value,
            node = node.parent
    def eval_narrative_predictions(self,node,predictions):
        evaluation = 1.0
        prev = None
        for prediction,actual in zip(predictions,self.narrative.data):
            prob_knn = actual.distribution_knn[function_list.index(prediction)]
            prob_markov = self.markov_table.get_transition_probability(prev,prediction)
            prev = prediction
            evaluation *= prob_knn*prob_markov
        return evaluation

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

    def __init__(self,laplacian_beta_knn,laplacian_beta_markov,num_attributes_to_include=0):
        # init
        self.n = 5
        self.laplacian_beta_knn = laplacian_beta_knn
        self.laplacian_beta_markov = laplacian_beta_markov

        # load dataset
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
            self.narratives[self.stories.index(story)].data.append(NarrativeFunctionData(attributes,label))

    def get_training_dataset(self,current_story):
        training = []
        for narrative in self.narratives:
            if narrative.story==current_story:
                pass
            else:
                training += narrative.data
        return training

    def init_distributions(self,test,training):
        for function in test.data:
            function.distribution_knn = self.probabilistic_distribution_knn(training,function,self.n,self.laplacian_beta_knn)
    def predict_mcts(self,epsilon_greedy):
        #for test in self.narratives[0:1]:
        for test in self.narratives:
            training = self.get_training_dataset(test.story)
            logger.info('cross validation on story %d training %d test %d' % (test.story,len(training),len(test.data)))
            self.init_distributions(test,training)
            markov_table = LearnedMarkovTable(self.laplacian_beta_markov,self.narratives,test)
            mcts = MCTS()
            node = mcts.search(test,markov_table,epsilon_greedy)
            test.predictions = mcts.get_predictions(node)
            test.distributions = mcts.get_distributions(node)

    def distance_euclidean(self,c1,c2):
        return math.sqrt(
                         sum([1.0*(a-b)**2 for a,b in zip(c1.attributes,c2.attributes)])
                         /
                         len(self.attributes)
                         )

    def probabilistic_assignment(self,distribution):
        return function_list[sorted(enumerate(distribution), key=itemgetter(1), reverse=True)[0][0]]

    def probabilistic_distribution_knn(self,training,target,n,laplacian_beta):
        instances = self.get_knn(training,target,n)
        # TODO return some sort of sparse object instead of the full list?
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
            for example,prediction in zip(narrative.data,narrative.predictions):
                total +=1
                eq +=1 if example.label==prediction else 0
        return 1.0*eq/total if total else 0.0
    def eval_dataset_rank(self,dataset):
        ranks = []
        for narrative in dataset:
            assert isinstance(narrative,NarrativeData)
            for example,distribution in zip(narrative.data,narrative.distributions):
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


if __name__=="__main__":
    main()



# current features

