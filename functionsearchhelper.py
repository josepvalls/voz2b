import collections
import math
import util
from operator import itemgetter,attrgetter
import settings
import jsonpickle
import logging
import itertools
import random
from grammarhelper import ProppNFSA

import sys
from os.path import expanduser
home = expanduser("~")

logger = logging.getLogger(__name__)

function_list = 'alpha beta gamma delta epsilon zeta eta theta lambda A a B C depart D E F G H J I K return Pr Rs o L M N Q Ex T U W'.split()

DO_CHECK_KNN = 1
DO_CHECK_MARKOV = 2
DO_CHECK_CARDINALITY = 4
DO_CHECK_NFSA = 8
DO_CHECK_NFSA_AT_THE_END = 16
DO_CHECK = DO_CHECK_KNN | DO_CHECK_MARKOV | DO_CHECK_CARDINALITY | DO_CHECK_NFSA | DO_CHECK_NFSA_AT_THE_END

USE_FILTERED_DATASET = True # 230 vs 208 instances
K_IN_KNN = 5 # test 5 to 11

#LAPLACIAN_BETA_KNN = 0.5
#LAPLACIAN_BETA_MARKOV = 0.5
#LAPLACIAN_BETA_NFSA = 0.5
#LAPLACIAN_BETA_CARDINALITY = 0.1

LAPLACIAN_BETA_KNN = LAPLACIAN_BETA_MARKOV = LAPLACIAN_BETA_NFSA = LAPLACIAN_BETA_CARDINALITY = 0.1
USE_GT_FOR_PREDICTIONS_WHEN_STEPPING = True

def main():
    do_systematic()

def do_systematic():
    for i in range(DO_CHECK_KNN | DO_CHECK_MARKOV | DO_CHECK_CARDINALITY | DO_CHECK_NFSA | DO_CHECK_NFSA_AT_THE_END):
        global DO_CHECK
        DO_CHECK = i+1
        print "START USING SETUP %d" % DO_CHECK
        fp = SequentialFunctionPredictor(k_in_knn=K_IN_KNN,laplacian_beta_knn=LAPLACIAN_BETA_KNN,laplacian_beta_markov=LAPLACIAN_BETA_MARKOV,num_attributes_to_include=10)
        fp.predict_systematic(best_first_branches_num=-1,beam_search_open_size=1000,beam_search_open_size_multiplier=1)


def do_dump_all_predictions():
    fp = SequentialFunctionPredictor(k_in_knn=K_IN_KNN,laplacian_beta_knn=LAPLACIAN_BETA_KNN,laplacian_beta_markov=LAPLACIAN_BETA_MARKOV,num_attributes_to_include=10)
    fp.predict_knn()
    for narrative in fp.narratives:
        for function in narrative.data:
            print '\t'.join([str(i) for i in [narrative.story]+util.flatten([function.distribution_knn,function.distribution_markov,function.distribution_cardinal,function.distribution_nfsa])])


class NarrativeData(object):
    def __init__(self,story):
        self.story=story
        self.data = []

class NarrativeFunctionData(object):
    def __init__(self,attributes,label):
        self.attributes,self.label=attributes,label
        self.distribution_knn = []
        self.prediction_knn = None
        self.prediction = None

class NarrativeFunctionPrediction(object):
    def __init__(self,prediction,parent,value):
        self.prediction = prediction # the option for this node
        self.parent = parent #type: NarrativeFunctionPrediction
        self.value = value #type: float
    def __str__(self):
        return "%s %f" % (self.prediction,self.value)

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

class LearnedCardinalityTable2(object):
    def __init__(self,laplacian_beta,narratives,exclude):
        self.laplacian_beta = laplacian_beta
        self.table = {}
        self.total = 0
        self.learn_table(narratives,exclude)
    def get_probability(self,f,n):
        total = self.total+self.total*self.laplacian_beta
        return 1.0* (self.table[function_list.index(f)][n]+self.laplacian_beta)/total
    def learn_table(self,narratives,exclude):
        table = [[0]*6 for _ in function_list]
        for narrative in narratives:
            if narrative.story==exclude.story: continue
            self.total+=1
            for f,i in collections.Counter([i.label for i in narrative.data]).items():
                table[function_list.index(f)][i]+=1
        for i in range(len(function_list)):
            table[i][0]=self.total-sum(table[i])
        self.table = table

class SystematicSearchEngine(object):
    def search(self,narrative,markov_table,cardinality,nfsa,best_first_branches_num=-1,beam_search_open_size=10,beam_search_open_size_multiplier=1):
        def get_predictions(node):
            predictions = []
            node_ = node
            while node_.parent:
                predictions.append(node_.prediction)
                node_ = node_.parent
            predictions.reverse()
            return predictions # the root node has a None prediction, not returned here
        def get_predictions_accuracy(narrative,predictions):
            c = 0
            t = 0
            for function,prediction in zip(narrative.data,predictions):
                if function.label == prediction:
                    c +=1
                t +=1
            return 1.0*c/t

        max_depth = len(narrative.data)
        root = NarrativeFunctionPrediction(None,None,0.0)
        open = [root]
        for depth in xrange(max_depth):
            logger.info("%d depth, %d open" % (depth,len(open)))
            new_open = []
            for node in open:
                # get successors
                for function in function_list:
                    value = 1.0
                    if DO_CHECK & DO_CHECK_KNN:
                        value *= narrative.data[depth].distribution_knn[function_list.index(function)]
                    if DO_CHECK & DO_CHECK_MARKOV:
                        prev = node.parent.prediction if node.parent else None
                        value *= markov_table.get_transition_probability(prev,function)
                    if DO_CHECK & DO_CHECK_NFSA:
                        nfsa.reset()
                        for prediction in get_predictions(node):
                            nfsa.step(prediction)
                        value *= nfsa.current_probability(function)
                    new_open.append(NarrativeFunctionPrediction(function,node,value))
            open_size = beam_search_open_size*beam_search_open_size_multiplier*(depth+1)
            logger.info(" %d successors" % len(new_open))
            if len(new_open)>open_size:
                open = sorted(new_open,key=attrgetter('value'),reverse=True)[0:open_size]
            else:
                open = new_open
        # we are at the bottom, let's see what's here
        results = []
        for node in open:
            predictions = get_predictions(node)
            probability = node.value
            if DO_CHECK & DO_CHECK_CARDINALITY:
                counter = collections.Counter(predictions)
                for function in function_list:
                    probability *= cardinality.get_probability(function,counter.get(function,0))

            if DO_CHECK & DO_CHECK_NFSA_AT_THE_END:
                nfsa.reset()
                for prediction in predictions:
                    probability *= nfsa.current_probability(prediction)
                    nfsa.step(prediction)
            accuracy = get_predictions_accuracy(narrative,predictions)
            results.append((math.log(probability),math.log(node.value),accuracy,','.join(predictions)))
        print "sorted by final probability"
        results.sort(reverse=True)
        for result in results[0:100]:
            print "%f\t%f\t%f\t%s" % result
        return results[0]

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

    def __init__(self,k_in_knn=5,laplacian_beta_knn=1.0,laplacian_beta_markov=1.0,num_attributes_to_include=0):
        # init
        self.n = k_in_knn
        self.laplacian_beta_knn = laplacian_beta_knn
        self.laplacian_beta_markov = laplacian_beta_markov

        # load dataset
        self.stories = range(1,16)+[1001,1002,1003]
        filtered = '_filtered' if USE_FILTERED_DATASET else ''
        story_indices = [int(i.strip()) for i in open(home+'/voz2/tool_corpus_functions_summary/story_indices%s.txt' % filtered).readlines()]
        dataset = [i.strip().split('\t') for i in open(home+'/voz2/tool_corpus_functions_summary/tool_corpus_functions_summary_5_dist%s.tsv'%filtered).readlines()]
        self.attributes = dataset[0][0:-1]
        self.weights = [1.0 for _ in self.attributes]
        dataset = dataset[1:]
        labels = [i[-1] for i in dataset]
        attributes = [[float(j) for j in i[0:-1]] for i in dataset]
        if num_attributes_to_include:
            attributes = self.select_attributes(attributes,attribute_selection_rules[0:num_attributes_to_include])
        self.narratives = [NarrativeData(i) for i in self.stories]
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

    def init_distributions(self,test,training,use_gt_for_predictions=True,markov_table=None,cardinality=None,nfsa=None):
        prev = None
        for function in test.data:
            function.distribution_knn = self.probabilistic_distribution_knn(training,function,self.n,self.laplacian_beta_knn)
            function.prediction_knn = self.probabilistic_assignment(function.distribution_knn)


    def predict_systematic(self,best_first_branches_num=-1,beam_search_open_size=10,beam_search_open_size_multiplier=1):
        #for test in self.narratives[0:1]:
        results = []
        for test in self.narratives:
            training = self.get_training_dataset(test.story)
            logger.info('cross validation on story %d training %d test %d' % (test.story,len(training),len(test.data)))
            markov_table = LearnedMarkovTable(self.laplacian_beta_markov,self.narratives,test)
            cardinality = LearnedCardinalityTable2(self.laplacian_beta_markov, self.narratives, test)
            nfsa = ProppNFSA('data/nfsa-propp3.txt',function_list,LAPLACIAN_BETA_NFSA,allow_only_one=True)
            self.init_distributions(test, training, use_gt_for_predictions=USE_GT_FOR_PREDICTIONS_WHEN_STEPPING, markov_table=markov_table, cardinality=cardinality, nfsa=nfsa)
            sse = SystematicSearchEngine()
            result = sse.search(test,markov_table,cardinality,nfsa,best_first_branches_num,beam_search_open_size,beam_search_open_size_multiplier)
            results.append(result)
        total_functions = sum(len(i.data) for i in self.narratives)
        "overall results for the first result",sum(i[2]*len(i[3].split(','))/total_functions for i in results)





    def predict_mcts(self,epsilon_greedy,sampling_accumulator=None):
        #for test in self.narratives[0:1]:
        for test in self.narratives:
            training = self.get_training_dataset(test.story)
            logger.info('cross validation on story %d training %d test %d' % (test.story,len(training),len(test.data)))
            markov_table = LearnedMarkovTable(self.laplacian_beta_markov,self.narratives,test)
            cardinality = LearnedCardinalityTable2(self.laplacian_beta_markov, self.narratives, test)
            nfsa = ProppNFSA('data/nfsa-propp3.txt',function_list,LAPLACIAN_BETA_NFSA,allow_only_one=True)
            self.init_distributions(test, training, use_gt_for_predictions=USE_GT_FOR_PREDICTIONS_WHEN_STEPPING, markov_table=markov_table, cardinality=cardinality, nfsa=nfsa)
            mcts = MCTS()
            mcts.search(test,markov_table,cardinality,epsilon_greedy)
            if sampling_accumulator is None:
                pass
            else:
                sampling_accumulator += mcts.sample_tree()

    def predict_knn(self):
        for test in self.narratives:
            training = self.get_training_dataset(test.story)
            logger.info('cross validation on story %d training %d test %d' % (test.story,len(training),len(test.data)))
            self.init_distributions(test, training, use_gt_for_predictions=USE_GT_FOR_PREDICTIONS_WHEN_STEPPING, markov_table=markov_table, cardinality=cardinality, nfsa=nfsa)
            for function in test.data:
                function.distribution = function.distribution_knn
                function.prediction = function.prediction_knn

    def dump_probabilities(self):
        for test in self.narratives:
            markov_table = LearnedMarkovTable(self.laplacian_beta_markov,self.narratives,test)
            cardinality = LearnedCardinalityTable2(self.laplacian_beta_markov, self.narratives, test)
            nfsa = ProppNFSA('data/nfsa-propp3.txt',function_list,LAPLACIAN_BETA_NFSA,allow_only_one=True)
            if False:
                # do printouts of the tables
                for i in [None]+function_list:
                    print i,
                    for j in function_list:
                    #for k in range(6):
                        print markov_table.markov_table[i][j],
                        #observations = cardinality.table[function_list.index(i)]
                        #print collections.Counter(observations).get(k,0),
                    print
                sys.exit()

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
    def eval_dataset_accuracy(self,dataset,field_to_check='prediction',field_gt='label'):
        total = 0
        eq = 0
        for narrative in dataset:
            for function in narrative.data:
                total +=1
                eq +=1 if getattr(function,field_gt)==getattr(function,field_to_check) else 0
        return 1.0*eq/total if total else 0.0
    def eval_dataset_rank(self,dataset,distr_field = 'distribution', field_gt='label'):
        ranks = []
        for narrative in dataset:
            assert isinstance(narrative,NarrativeData)
            for function in narrative.data:
                evals = sorted(zip(getattr(function,distr_field),function_list),reverse=True)
                rank = 0
                for k,group in itertools.groupby(evals,key=itemgetter(0)):
                    if function.label in list(group):
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