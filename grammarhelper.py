import pprint
import collections
import math
import util
from operator import itemgetter,attrgetter
import settings
import logging
import itertools

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

def main():
    logging.root.setLevel(logging.ERROR)
    #do_print_markov(0.5)
    #do_recognize_to_table(0.5)
    #return
    #do_recognize()

    fp = SequentialFunctionPredictor(num_attributes_to_include=10,laplacian_beta=0.5)
    dataset = fp.predict_knn()
    accuracy = fp.eval_dataset_accuracy(dataset)
    ranks = fp.eval_dataset_rank(dataset)
    print accuracy,util.describe_distribution(ranks)


def do_find_feature_selection():
    for i in range(16):
        fp = SequentialFunctionPredictor(num_attributes_to_include=i)
        accuracy = fp.eval_dataset_accuracy(fp.predict_knn())
        print "prediction accuracy\t%f\tusing\t%s" % (accuracy,', '.join(fp.attributes))
    # no feature selection: 0.164502164502
    # weka auto feature selection: 0.206731
    # random feature selection: 0.138528138528

function_list = [None] + 'alpha beta gamma delta epsilon zeta eta theta lambda A a B C depart D E F G H J I K return Pr Rs o L M N Q Ex T U W'.split()

class Example(object):
    def __init__(self,id,story,attributes,label):
        self.id,self.story,self.attributes,self.label=id,story,attributes,label
        self.distribution = None
        self.prediction = None

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

    def __init__(self,num_attributes_to_include=0,laplacian_beta=1.0):
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
        self.dataset = [Example(a,b,c,d) for a,(b,c,d) in enumerate(zip(story_indices,attributes,labels))]
        self.n = 5
        self.laplacian_beta = laplacian_beta
        self.markov_table = self.learn_markov(self.dataset,self.laplacian_beta) # TODO move this to the leave-one-out loop
    def predict_knn(self):
        #for story in self.stories[0:1]:
        for story in self.stories:
            training,test = util.partition_dataset(self.dataset,[story],attrgetter('story'))
            logger.info('cross validation on story %d training %d test %d' % (story,len(training),len(test)))
            for instance in test:
                instance.distribution = self.probabilistic_distribution_knn(training,instance,self.n,self.laplacian_beta)
                instance.prediction = self.probabilistic_assignment(instance.distribution)
        return self.dataset

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
    def eval_dataset_accuracy(self,dataset):
        total = 0
        eq = 0
        for i in dataset:
            total +=1
            eq +=1 if i.label==i.prediction else 0
        return 1.0*eq/total

    def eval_dataset_rank(self,dataset):
        ranks = []
        for example in dataset:
            evals = sorted(zip(example.distribution,function_list),reverse=True)
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

    def learn_markov(self,dataset,laplacian_beta):
        table = collections.defaultdict(lambda:collections.defaultdict(lambda:0))
        stories_as_functions = []
        story = [None]
        last_story_id = None
        for example in dataset:
            if example.story==last_story_id or not last_story_id:
                story.append(example.label)
            else:
                stories_as_functions.append(story)
                story = [None,example.label]
            last_story_id = example.story
        else:
            stories_as_functions.append(story)
        for story in stories_as_functions:
            for a,b in zip(story[0:-1],story[1:]):
                table[a][b]+=1
    def get_transition_probability(self,table,laplacian_beta,f0,f1):
        if laplacian_beta:
            total = sum(table[f0].values())+laplacian_beta*(len(function_list)-1)
        else:
            total = sum(table[f0].values())
        if table[f0][f1]:
            return ((1.0*table[f0][f1]+laplacian_beta)/total)
        else:
            return laplacian_beta/total
    def select_policy(self):
        return self.select_policy_ucb()
    def select_policy_ucb(self):
        pass
    def select_policy_egreedy(self):
        pass


def do_print_markov(laplacian_beta=1.0):
    table = collections.defaultdict(lambda:collections.defaultdict(lambda:0))
    for file_name in ['data/grammar-test.txt']:
    #for file_name in ['data/grammar-test.txt','data/grammar-test-filtered.txt']:
    #for file_name in ['data/grammar-test-filtered.txt']:
        grammar_test = [i.split() for i in open(file_name).readlines()]
        for i,story in enumerate(grammar_test):
            story = [None]+story
            for a,b in zip(story[0:-1],story[1:]):
                table[a][b]+=1
    print '\t'.join([str(i) for i in function_list])
    for f0 in function_list:
        if laplacian_beta:
            total = sum(table[f0].values())+laplacian_beta*(len(function_list)-1)
        else:
            total = sum(table[f0].values())
        if True: # print as table
            print f0,
            for f1 in function_list:
                if table[f0][f1]:
                    print ((1.0*table[f0][f1]+laplacian_beta)/total),
                else:
                    print laplacian_beta/total,
            print

        else:
            if not total or not sum(table[f0].values()):
                print f0,'N/A',laplacian_beta/total
            else:
                for f1 in table[f0].keys():
                    print f0,f1,(1.0*table[f0][f1]+laplacian_beta)/total,table[f0][f1]





def do_recognize():
    #for file_name in ['data/grammar-test.txt']:
    for file_name in ['data/grammar-test-filtered.txt']:
    #for file_name in ['data/grammar-test.txt','data/grammar-test-filtered.txt']:
        grammar_test = [i.split() for i in open(file_name).readlines()]
        for i,story in enumerate(grammar_test):
            f=ProppNFSA('data/nfsa-propp3.txt')
            print 1 if order_test(story) else 0
            continue
            if not f.recognize(story):
                print i,story
                f.recognize(story,True)
def do_recognize_to_table(laplacian_beta=1.0):
    f=ProppNFSA('data/nfsa-propp3.txt')
    table = collections.defaultdict(lambda:collections.defaultdict(lambda:0))
    prev = None
    for token in function_list[1:]:
        for i in f.currently_allowed():
            table[prev][i]+=1
        prev = token
        f.step(token)

    print '\t'.join([str(i) for i in function_list])
    for f0 in function_list:
        if laplacian_beta:
            total = sum(table[f0].values())+laplacian_beta*(len(function_list)-1)
        else:
            total = sum(table[f0].values())
        if True: # print as table
            print f0,
            for f1 in function_list:
                if table[f0][f1]:
                    print '%f\t'%((1.0*table[f0][f1]+laplacian_beta)/total),
                else:
                    print '%f\t' % (laplacian_beta/total),
            print

        else:
            if not total or not sum(table[f0].values()):
                print f0,'N/A',laplacian_beta/total
            else:
                for f1 in table[f0].keys():
                    print f0,f1,(1.0*table[f0][f1]+laplacian_beta)/total,table[f0][f1]




def do_order_test(tokens):
    reference = 'A a B C depart D E F G H J I K return Pr Rs o L M N Q Ex T U W'.split()
    #reference = 'A a B C D E F G H J I K L M N Q T U W'.split()
    tokens = [i for i in tokens if i in reference]
    tokens = [reference.index(i) for i in tokens]
    tokens2 = sorted(tokens)
    return tokens == tokens2

def do_grammar_tests():
    from nltk import CFG
    grammar_files = ['grammar-mpropp.txt','grammar-mpropp2.txt','grammar-lakoff.txt','grammar-gervas.txt','grammar-finlayson.txt']
    grammar_test = [i.split() for i in open('data/grammar-test-filtered.txt').readlines()]
    for i in grammar_files:
        grammar_file = 'data/'+i
        print grammar_file,'\t',
        g = CFG.fromstring(open(grammar_file).read())
        #pprint.pprint(g.productions())
        coverage = True
        for i,tokens in enumerate(grammar_test):
            try:
                g.check_coverage(tokens)
                print 1,
            except Exception as e:
                print 0,#,e`
                coverage = False
        print
        #rdp = nltk.RecursiveDescentParser(g)
        #srp = nltk.ShiftReduceParser(g)
        #bulccp = nltk.BottomUpLeftCornerChartParser(g)
        if coverage:
            for i,tokens in enumerate(grammar_test):
                pass
                #print srp.parse_one(tokens)
                #print bulccp.parse_one(tokens)
        # CONVERT GRAMMAR TO NONDETERMINISTIC FSM, PARSE USING THIS? DEBUG?


class ProppNFSA(object):
    special_tokens = {'depart':'return','Pr':'Rs'}
    state_transition_dict = None
    function_list = None
    def __init__(self,filename,function_list,laplacian_beta):
        self.state = 'START'
        self.extra_tokens = set()
        self.laplacian_beta = laplacian_beta
        self.reset()
        if not ProppNFSA.state_transition_dict: ProppNFSA.state_transition_dict = ProppNFSA.read_nfsa_file(filename)
        if not ProppNFSA.function_list: ProppNFSA.function_list = function_list

    @classmethod
    def read_nfsa_file(cls,filename):
        state_transition_dict = {}
        for line in open(filename).readlines():
            if not line.strip() or line.strip().startswith('#'): continue
            s_origin,transition,s_final = line.strip().split()
            for i in transition.split(','):
                state_transition_dict[(s_origin,i)]=s_final
        return dict(state_transition_dict)

    def reset(self):
        self.state = 'START'
        self.extra_tokens = set()
    def recognize(self,tokens,verbose=False):
        self.reset()
        for token in tokens:
            if not self.step(token,verbose):
                return False
        else:
            return True

    def currently_allowed(self):
        return set([i[1] for i in ProppNFSA.state_transition_dict.keys() if i[0]==self.state])-set(ProppNFSA.special_tokens.values())|self.extra_tokens
    def current_distribution(self):
        current = self.currently_allowed()
        #total = len(current) + self.laplacian_beta*len(ProppNFSA.function_list)
        #default = 1.0 * self.laplacian_beta / total
        #included = 1.0*( 1.0+self.laplacian_beta) / total
        #return [included if i in current else default for i in ProppNFSA.function_list]
        included = 1.0 / len (current)
        return [included if i in current else 0.0 for i in ProppNFSA.function_list]
    def step(self,token,verbose=False):
        # account for special pairs
        if token in ProppNFSA.special_tokens.keys(): self.extra_tokens.add(ProppNFSA.special_tokens[token])
        if token in self.extra_tokens:
            if verbose:
                print self.state,token,'EXTRA'
            self.extra_tokens.remove(token)
            return True
        if token in ProppNFSA.special_tokens.values():
            if verbose:
                print self.state,token,'ERROR SPECIAL'
            return False

        # fsa transition lookup
        key = (self.state,token)
        if key in ProppNFSA.state_transition_dict:
            old_state = self.state
            self.state = ProppNFSA.state_transition_dict[key]
            if verbose:
                print old_state,token,self.state
            return True
        else:
            if verbose:
                print self.state,token,'ERROR'
            return False


if __name__=="__main__":
    main()



# current features

