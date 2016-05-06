import pprint
import collections
import math
import util
from operator import itemgetter,attrgetter
import settings
import logging
import itertools
import random
from grammarhelper import ProppNFSA
import jsonpickle
import sys
from os.path import expanduser
home = expanduser("~")



'''
plot chart with x = probability computed y = accuracy
compute the probability of the ground truth to have a target
try decreasing epsilon?



'''


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

USE_FILTERED_DATASET = True # 230 vs 208 instances
EVAL_DO_USE_MARKOV = True
if EVAL_DO_USE_MARKOV:
    MCTS_NODE_START_VALUE = 0.5**(len(function_list)*3) # this is 2 since its the joint probability of the ml and markov predictions, cardinality
else:
    MCTS_NODE_START_VALUE = 0.5**(len(function_list))
MCTS_ROUNDS_PER_FUNCTION = 1000000
LAPLACIAN_BETA_KNN = 0.5
LAPLACIAN_BETA_MARKOV = 0.5
LAPLACIAN_BETA_NFSA = 0.5
K_IN_KNN = 5 # test 5 to 11
DO_LEAVE_ONE_OUT_MARKOV = True
NUM_SAMPLES_FROM_TREE_TO_PLOT = 100

def main():


    do_mcts()
    return
    logging.root.setLevel(logging.INFO)
    #logging.root.setLevel(logging.ERROR)
    #do_explore_knn()
    #do_compute_probabilities_for_charting_mcts()
    do_charting()


def do_analize_mcts_results():

    d = jsonpickle.decode(open("mcts_knn_narratives.json").read())
    for n in d:
        for f in n.data:
            print n.story,f.label,f.prediction_knn,f.prediction_mcts


    return


def do_mcts():
    fp = SequentialFunctionPredictor(k_in_knn=K_IN_KNN,laplacian_beta_knn=LAPLACIAN_BETA_KNN,laplacian_beta_markov=LAPLACIAN_BETA_MARKOV,num_attributes_to_include=10)
    fp.predict_mcts(epsilon_greedy=0.1)

    accuracy = fp.eval_dataset_accuracy(fp.narratives,'label','prediction_mcts')
    print 'accuracy gt vs mcts',fp.eval_dataset_accuracy(fp.narratives,'label','prediction_mcts')
    ranks_mcts = fp.eval_dataset_rank(fp.narratives,'distribution_mcts')
    print util.describe_distribution(ranks_mcts)
    print 'accuracy gt vs knn',fp.eval_dataset_accuracy(fp.narratives,'label','prediction_knn')
    ranks_knn = fp.eval_dataset_rank(fp.narratives,'prediction_knn')
    print util.describe_distribution(ranks_knn)
    print 'accuracy knn vs mcts',fp.eval_dataset_accuracy(fp.narratives,'prediction_knn','prediction_mcts')
    stories = util.flatten([[i.story for j in i.data] for i in fp.narratives])
    for i in zip(stories,ranks_knn,ranks_mcts):
        print i

    open("mcts_knn_markov_ranks.json",'w').write(jsonpickle.dumps((ranks_knn,ranks_mcts)))
    open("mcts_knn_markov_narratives.json",'w').write(jsonpickle.dumps(fp.narratives))

    if False:
        ranks = fp.eval_dataset_rank(fp.narratives,'distribution_mcts')
        print accuracy,util.describe_distribution(ranks)
    else:
        print accuracy

def do_explore_knn():
    n = 10
    for k in range(1,12):
        for n in range(1,15):
            print "checking k=%d, attributes=%d\t" % (k,n),
            fp = SequentialFunctionPredictor(k_in_knn=k,laplacian_beta_knn=LAPLACIAN_BETA_KNN,laplacian_beta_markov=LAPLACIAN_BETA_MARKOV,num_attributes_to_include=n)
            fp.predict_knn()
            accuracy = fp.eval_dataset_accuracy(fp.narratives)
            ranks = fp.eval_dataset_rank(fp.narratives)
            print accuracy,util.describe_distribution(ranks)

def do_compute_probabilities_for_charting_mcts():
    fp = SequentialFunctionPredictor(k_in_knn=K_IN_KNN,laplacian_beta_knn=LAPLACIAN_BETA_KNN,laplacian_beta_markov=LAPLACIAN_BETA_MARKOV,num_attributes_to_include=10)
    samples = []
    fp.predict_mcts(epsilon_greedy=0.1,sampling_accumulator = samples)
    try:
        import jsonpickle
        open("samples.json",'w').write(jsonpickle.dumps(samples))
    except:
        import pickle
        pickle.dump(samples,open('samples.pickle','wB'))




def do_charting():
    narratives_probabilities_knn = [] # list of lists of probabilities, one list per narrative
    narratives_probabilities_markov_knn = [] # list of lists of probabilities, one list per narrative
    narratives_probabilities_markov_gt = [] # list of lists of probabilities, one list per narrative
    narratives_probabilities_joint = [] # list of lists of probabilities, one list per narrative
    narratives_probabilities_knn_random = []
    narratives_probabilities_markov_random = []
    narratives_probabilities_markov_knn_gt = []
    narratives_probabilities_nfsa = []
    narratives_probabilities_nfsa_random = []

    import numpy as np


    # first compute the ones from the
    fp = SequentialFunctionPredictor(k_in_knn=5)
    fp.predict_knn()
    f=ProppNFSA('data/nfsa-propp3.txt',function_list,LAPLACIAN_BETA_NFSA)
    for narrative in fp.narratives:
        probabilities = [i.distribution[function_list.index(i.prediction)] for i in narrative.data]
        narratives_probabilities_knn.append(probabilities)
        for i in range(500):
            p = 1.0
            c = 0
            t = 0
            for function in narrative.data:
                if t == 6:
                    break

                distr = np.array(function.distribution_knn)
                distr = distr / sum(distr)

                pred = np.random.choice(function_list, 1, p=distr)[0]
                p *= function.distribution_knn[function_list.index(pred)]
                if pred==function.label:
                    c +=1
                t +=1

            #samples.append((accuracy,payout))
            narratives_probabilities_knn_random.append((1.0*c/t,p))

        if DO_LEAVE_ONE_OUT_MARKOV:
            exclude = narrative
        else:
            exclude = NarrativeData("fake")
        markov_table = LearnedMarkovTable(LAPLACIAN_BETA_MARKOV,fp.narratives,exclude)

        for i in range(500):
            p = 1.0
            p2 = 1.0
            c = 0
            t = 0
            prev = None
            #print 'new random'
            for function in narrative.data:
                if t == 6:
                    break

                distr = [markov_table.get_transition_probability(prev,i) for i in function_list]
                distr = np.array(distr)
                distr = distr / sum(distr)

                pred = np.random.choice(function_list, 1, p=distr)[0]

                p_markov = markov_table.get_transition_probability(prev,pred)
                prev = pred
                p_knn = function.distribution_knn[function_list.index(pred)]
                p *= p_knn
                p2 *= p_knn*p_markov
                #print p_knn,p_markov
                if pred==function.label:
                    c +=1
                t +=1

            #samples.append((accuracy,payout))
            narratives_probabilities_markov_random.append((1.0*c/t,p))
            narratives_probabilities_joint.append((1.0*c/t,p2))



        probabilities = []
        probabilities2 = []
        prev = None
        for function in narrative.data:
            prob_markov = markov_table.get_transition_probability(prev,function.prediction)
            prob_markov_knn = markov_table.get_transition_probability(prev,function.prediction) * function.distribution_knn[function_list.index(function.label)]
            probabilities.append(prob_markov)
            probabilities2.append(prob_markov_knn)


        narratives_probabilities_markov_knn.append(probabilities)
        narratives_probabilities_markov_knn_gt.append(probabilities2)

        probabilities = []
        prev = None
        for function in narrative.data:
            prob_markov = markov_table.get_transition_probability(prev,function.label)
            probabilities.append(prob_markov)
        narratives_probabilities_markov_gt.append(probabilities)


        probabilities = []
        f.reset()
        for function in narrative.data:
            prob = f.current_distribution()[function_list.index(function.label)]
            f.step(function.label)
            probabilities.append(prob)
        narratives_probabilities_nfsa.append(probabilities)
        for i in range(500):
            p = 1.0
            c = 0
            t = 0
            f.reset()
            for function in narrative.data:
                if t == 6:
                    break

                distr = np.array(f.current_distribution())
                distr = distr / sum(distr)

                pred = np.random.choice(function_list, 1, p=distr)[0]
                p *= f.current_distribution()[function_list.index(pred)]
                f.step(pred)
                if pred==function.label:
                    c +=1
                t +=1

            #samples.append((accuracy,payout))
            narratives_probabilities_nfsa_random.append((1.0*c/t,p))





    # plot stuff, let's start with knn
    #util.sliding_window
    import operator
    print 'nfsa',[reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_nfsa]
    print 'knn_gt',[reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_markov_knn_gt]
    print 'markov_knn',[reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_markov_knn]
    print 'markov_gt',[reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_markov_gt]
    #return
    if False:
        # chart nfsa
        X1 = [reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_nfsa]
        X2 = filter(None,X1)
        print "missing GT",(len(X1),len(X2))
        Y = [1.0 for _ in X2]
        #return
        from matplotlib import pyplot as plt
        ax = plt.gca()
        ax.set_yscale('log')
        #ax.set_xscale('log')
        plt.scatter(Y,X2,color='red')
        #plt.scatter(Y,X3,color='orange')
        import pickle
        plt.scatter(*zip(*narratives_probabilities_nfsa_random),color='cyan')
        plt.show()

    if True:
        # chart both
        X2 = [reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_markov_knn_gt]
        Y = [1.0 for _ in X2]
        #return
        from matplotlib import pyplot as plt
        ax = plt.gca()
        ax.set_yscale('log')
        #ax.set_xscale('log')
        plt.scatter(Y,X2,color='red')
        #plt.scatter(Y,X3,color='orange')
        import pickle
        samples = pickle.load(open('samples_both.pickle','rB'))
        plt.scatter(*zip(*narratives_probabilities_joint),color='cyan')
        plt.scatter(*zip(*samples),color='blue')
        plt.scatter(*zip(*samples[0:1]),color='green')
        plt.show()
    if False:
        # chart markov
        X2 = [reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_markov_knn]
        X3 = [reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_markov_gt]
        Y = [1.0 for _ in X2]
        #return
        from matplotlib import pyplot as plt
        ax = plt.gca()
        ax.set_yscale('log')
        #ax.set_xscale('log')
        plt.scatter(Y,X2,color='red')
        #plt.scatter(Y,X3,color='orange')
        import pickle
        samples = pickle.load(open('samples_markov.pickle','rB'))
        plt.scatter(*zip(*narratives_probabilities_markov_random),color='cyan')
        plt.scatter(*zip(*samples),color='blue')
        plt.show()
    if False:
        # chart knn

        X1 = [reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_knn]
        Y = [1.0 for _ in X1]
        X2 = [reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_markov_knn]
        X3 = [reduce(operator.mul,i[0:6],1.0) for i in narratives_probabilities_markov_gt]
        #return
        from matplotlib import pyplot as plt
        ax = plt.gca()
        ax.set_yscale('log')
        #ax.set_xscale('log')
        plt.scatter(Y,X1,color='red')
        import pickle
        samples = pickle.load(open('samples.pickle','rB'))
        plt.scatter(*zip(*narratives_probabilities_knn_random),color='cyan')
        plt.scatter(*zip(*samples),color='blue')
        plt.show()








class NarrativeData(object):
    def __init__(self,story):
        self.story=story
        self.data = []

class NarrativeFunctionData(object):
    def __init__(self,attributes,label):
        self.attributes,self.label=attributes,label
        self.distribution_knn = []
        self.distribution_mcts = []
        self.prediction_knn = None
        self.prediction_mcts = None

class NarrativeFunctionPrediction(object):
    def __init__(self,prediction,parent):
        self.prediction = prediction # the option for this node
        self.parent = parent #type: NarrativeFunctionPrediction
        self.children = [] #type: list[NarrativeFunctionPrediction]
        self.visits = 0
        self.payout = 0.0
        self.value = MCTS_NODE_START_VALUE
    def __str__(self):
        return "%s %d %f %f" % (self.prediction,self.visits,self.payout,self.value)

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

class LearnedCardinalityTable(object):
    def __init__(self,laplacian_beta,narratives,exclude):
        self.laplacian_beta = laplacian_beta
        self.table = None
        self.learn_table(narratives,exclude)
        self.observations = {}
        self.total = 0
    def reset(self):
        self.observations = {}
    def step(self,f):
        self.observations[f]=self.observations.get(f,0)+1
    def get_probability(self,f):
        observations = self.table[function_list.index(f)]
        total = self.total+self.total*self.laplacian_beta # this total accounts for the observations of NOT observing the function
        already = self.observations.get(f,0)
        return 1.0*collections.Counter(observations).get(already+1,self.laplacian_beta)/total
    def learn_table(self,narratives,exclude):
        table = [[] for i in function_list]
        for narrative in narratives:
            if narrative.story==exclude.story: continue
            self.total+=1
            for f,i in collections.Counter([i.label for i in narrative.data]):
                table[function_list.index(f)].append(i)
        self.table = table


class MCTS(object):
    def __init__(self):
        self.max_depth = 0
        self.narrative = None #type: NarrativeData
        self.markov_table = None #type: LearnedMarkovTable
        self.cardinality = None #type: LearnedCardinalityTable
        self.random = random
        self.random.seed(1)
        self.root = None #type: NarrativeFunctionPrediction
    def search(self,narrative,markov_table,cardinality,epsilon_greedy):
        self.narrative = narrative
        self.markov_table = markov_table
        self.cardinality = cardinality
        self.max_depth = len(narrative.data)
        root = NarrativeFunctionPrediction(None,None)
        for i in xrange(MCTS_ROUNDS_PER_FUNCTION):
            node = self.selection(root,0,epsilon_greedy)
            self.eval_node(node)
        final_node = self.selection(root,0,0.0,True)
        preds,distrs = self.get_predictions(final_node),self.get_distributions(final_node)
        assert len(preds)==len(distrs)
        if  not len(preds)==len(narrative.data):
            logger.error("MCTS for %s did not reach the end of the tree" % narrative.story)
        else:
            logger.info("MCTS for %s best node stats" % final_node)
            for prediction,distribution,function in zip(preds,distrs,narrative.data):
                function.prediction_mcts = prediction
                function.distribution_mcts = distribution
        self.root = root
        return root

    def selection(self,node,depth,epsilon_greedy,final_search=False):
        if depth==self.max_depth:
            #logger.info("Reached the end")
            return node
        if node.children:
            if self.random.random() < epsilon_greedy:
                choice = self.random.choice(node.children)
            else:
                if final_search:
                    # choice based on node visits for the final selection
                    choice =max(node.children, key=lambda i:i.visits)
                else:
                    # choice based on node value
                    choice =max(node.children, key=lambda i:i.value)
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
        node_ = node.parent
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
            predictions.append(self.random.choice(function_list))
        payout = self.eval_narrative_predictions(node,predictions)
        #print predictions2,payout,
        # backpropagate
        while node:
            node.visits+=1
            node.payout+=payout
            node.value = 1.0*node.payout/node.visits
            #print node.value,
            node = node.parent
    def eval_narrative_predictions(self,node,predictions):
        evaluation = 1.0
        prev = None
        self.cardinality.reset()
        for prediction,actual in zip(predictions,self.narrative.data):
            prob_knn = actual.distribution_knn[function_list.index(prediction)]
            prob_markov = self.markov_table.get_transition_probability(prev,prediction)
            prob_cardinal = self.cardinality.get_probability(prediction)
            self.cardinality.step(prediction)
            prev = prediction
            if EVAL_DO_USE_MARKOV:
                evaluation *= prob_knn*prob_markov*prob_cardinal
            else:
                evaluation *= prob_knn
        return evaluation
    def eval_predictions_accuracy(self,predictions):
        c = 0
        t = 0
        for function,prediction in zip(self.narrative.data,predictions):
            if function.label == prediction:
                c +=1
            t +=1
        return 1.0*c/t

    def sample_tree(self):
        samples = []
        max_depth = 6
        max_samples = NUM_SAMPLES_FROM_TREE_TO_PLOT

        def collect_samples(node,depth):
            if len(samples)>=max_samples: return
            if depth>=max_depth:
                # process this node
                predictions = self.get_predictions(node)
                assert len(predictions)==max_depth
                accuracy = self.eval_predictions_accuracy(predictions)
                payout = self.eval_narrative_predictions(node,predictions)
                samples.append((accuracy,payout,node.visits))
            else:
                children = list(node.children)
                #random.shuffle(children)
                children = sorted(node.children,key=attrgetter('visits'),reverse=True)
                for child in children:
                    collect_samples(child,depth+1)
            pass

        collect_samples(self.root,0)
        return samples

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
            function.prediction_knn = self.probabilistic_assignment(function.distribution_knn)
    def predict_mcts(self,epsilon_greedy,sampling_accumulator=None):
        #for test in self.narratives[0:1]:
        for test in self.narratives:
            training = self.get_training_dataset(test.story)
            logger.info('cross validation on story %d training %d test %d' % (test.story,len(training),len(test.data)))
            self.init_distributions(test,training)
            markov_table = LearnedMarkovTable(self.laplacian_beta_markov,self.narratives,test)
            cardinality = LearnedCardinalityTable(self.laplacian_beta_markov,self.narratives,test)
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
            self.init_distributions(test,training)
            for function in test.data:
                function.distribution = function.distribution_knn
                function.prediction = sorted(zip(function.distribution,function_list))[-1][1]



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
    def eval_dataset_accuracy(self,dataset,field_gt='label',field_to_check='prediction'):
        total = 0
        eq = 0
        for narrative in dataset:
            for function in narrative.data:
                total +=1
                eq +=1 if getattr(function,field_gt)==getattr(function,field_to_check) else 0
        return 1.0*eq/total if total else 0.0
    def eval_dataset_rank(self,dataset,distr_field = 'distribution'):
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



# current features

