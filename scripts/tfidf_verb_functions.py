import logging
import settings
import pickle
import util
import collections,itertools
import pprint
import random,numpy.random as npr
import json
import vozbase
import verbmanager


import math
from operator import itemgetter


pp = pprint.pprint

def main():
    do_tf_idf()


class TfIdf(object):
    def __init__(self, matrix, DEFAULT_IDF=1.5):
        self.num_docs = 0
        self.term_num_docs = {}  # term : num_docs_containing_term
        self.idf_default = DEFAULT_IDF

        for k,v in matrix.items():
            self.add_input_document(v)

        terms = {}
        for k,v in matrix.items():
            tfidf = self.get_doc_keywords(v)
            #print k,'\t\t',tfidf
            for i in tfidf:
                if i[0] not in terms or i[0] in terms and terms[i[0]][0]<i[1]:
                    terms[i[0]] = (i[1],k)
        tfidf_sorted = sorted(terms.items(), key=itemgetter(1), reverse=True)
        tfidf_set = set()
        for i in tfidf_sorted[0:10]:
            print i
            tfidf_set.add(i[0])
        terms = [(self.get_idf(k),k,v) for k,v in self.term_num_docs.items()]
        print "sorted IDF"
        for i in sorted(terms):
            print i[1] in tfidf_set, i






    def add_input_document(self, words):
        self.num_docs += 1
        for word in words:
            if word in self.term_num_docs:
                self.term_num_docs[word] += 1
            else:
                self.term_num_docs[word] = 1

    def get_idf(self, term):
        if not term in self.term_num_docs:
            return self.idf_default

        return math.log(float(1 + self.num_docs) / (1 + self.term_num_docs[term]))

    def get_doc_keywords(self, tokens):
        tfidf = {}
        tokens_set = set(tokens)
        for word in tokens_set:
            mytf = float(tokens.count(word)) / len(tokens_set)
            myidf = self.get_idf(word)
            tfidf[word] = mytf * myidf

        return sorted(tfidf.items(), key=itemgetter(1), reverse=True)

def do_tf_idf():
    vm = verbmanager.VerbMapper(verbmanager.VerbMapper.MODE_LEVIN_TEXT)
    function_dict = vozbase.unserialize_file('/Users/josepvalls/temp/voz2/verbs.json')[2]
    for f in function_dict.keys():
        if False:
            from nltk.corpus import wordnet as wn
            function_dict[f] = util.flatten(util.flatten([[i.root_hypernyms() for i in wn.synsets(verb, 'v')] for verb in function_dict[f]]))
        elif False:
            function_dict[f] = filter(None,[vm.map(i,fallback=False) for i in function_dict[f]])

    TfIdf(function_dict)




if __name__=="__main__":
    main()


