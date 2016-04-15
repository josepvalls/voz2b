import vozbase
import util
from string import ascii_letters

DO_COMPUTE_ROLE_DISTRIBUTION = False

class Narrative(vozbase.VozContainer):
    """
    Models narrative
    """
    def __init__(self,document):
        self.id = document.get_new_id(Narrative)
        self.document = document
        self.functions = []
    def compute_features(self):
        for function in self.functions:
            assert isinstance(function,NarrativeFunction)
            function._compute_features(self)
    def add_function(self,id,offset,length,function,locations):
        self.functions.append(NarrativeFunction(id,offset,length,function,locations))
    def format_summary(self):
        return '|'.join([i.function for i in self.functions])
    def format(self):
        return util.format_list(self.functions,'\n')
    def __str__(self):
        return self.format()
    def format_arff(self,include_header=True):
        arff = ""
        if include_header:
            arff += '@RELATION narrative\n'
            for i in zip(NarrativeFunction.get_feature_vector_labels(), NarrativeFunction.get_feature_vector_types()):
                arff += "@ATTRIBUTE \"%s\" %s\n" % i
            arff += "@DATA\n"
        # data
        for function in self.functions:
            arff += ','.join([str(i) for i in function.get_feature_vector()]) + "\n"
        return arff
    def format_tsv(self,include_header=True):
        tsv = ""
        if include_header:
            tsv += '\t'.join(NarrativeFunction.get_feature_vector_labels())+"\n"
        for function in self.functions:
            tsv += '\t'.join([str(i) for i in function.get_feature_vector()]) + "\n"
        return tsv


class NarrativeFunctionLocation(object):
    def __init__(self,kind,token_ids):
        self.kind = kind
        self.token_ids = token_ids
    def format(self,options={}):
        return "%s %s" % (self.kind, ','.join([str(i) for i in self.token_ids]))


class NarrativeFunction(vozbase.VozTextContainer):
    def __init__(self,id,offset,length,function_,locations):
        super(NarrativeFunction, self).__init__(id,offset,length)
        self.function = function_
        self.locations = locations
        self.function_group = function_
    def format(self,options={}):
        return "%s: %s" % (self.function, util.format_list(self.locations, '; '))
    @classmethod
    def translate_function(cls,function,mode='finlayson'):
        if mode=='finlayson':
            functions = ['alpha','beta','gamma','delta','epsilon','zeta','eta','theta','lambda','depart','return','Pr','Rs','Ex','up','down','A','a','B','C','D','d','E','F','f','G','H','J','I','K','o','L','M','N','Q','T','U','W','w']
            translate = {'up':'depart','down':'return','lambda':'theta','d':'D','f':'F','w':'W'}
            function = ''.join([i for i in function if i in ascii_letters])
            for i in functions:
                if function.startswith(i):
                    function = i
                    break
            if function in translate:
                function = translate[function]
            return function
        elif mode=='malec':
            return function[0]
    def _compute_features(self,narrative):
        assert isinstance(narrative,Narrative)
        document = narrative.document
        import voz
        assert isinstance(document,voz.Document)
        self.function_group =NarrativeFunction.translate_function(self.function)
        midpoint = 1.0*(self.offset+self.offset+self.len)/2.0
        self.position = midpoint / len(document.get_text())
        self.actual = 1.0 if 'ACTUAL' in [i.kind for i in self.locations] else 0.0
        self._compute_features_mentions(narrative)
    def _compute_features_mentions(self,narrative):
        import entitymanager
        taxonomy_labels = entitymanager.taxonomy_dict[entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES].labels
        self.role_counts = [0 for _ in taxonomy_labels]
        self._mentions_counted = set()
        self._compute_features_mentions_explicit(narrative,taxonomy_labels)
        self._compute_features_mentions_implicit(narrative,taxonomy_labels)
        if sum(self.role_counts):
            if DO_COMPUTE_ROLE_DISTRIBUTION:
                total = sum(self.role_counts)
                self.role_counts = [1.0*i/total for i in self.role_counts]
            else:
                self.role_counts = [1.0 if i else 0.0 for i in self.role_counts]
    def _compute_features_mentions_explicit(self,narrative,taxonomy_labels):
        import entitymanager
        mentions = filter(None,util.flatten([[narrative.document.get_mention_by_token_id(i) for i in location.token_ids] for location in self.locations]))
        for mention in mentions:
            if mention.id in self._mentions_counted: continue
            self._mentions_counted.add(mention.id)
            #print mention.format_all_taxonomies()
            labels = mention.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES)
            for label in labels:
                if label and label in taxonomy_labels:
                    self.role_counts[taxonomy_labels.index(label)]+=1
    def _compute_features_mentions_implicit(self,narrative,taxonomy_labels):
        import entitymanager
        import verbmanager
        verbs = filter(None,util.flatten([[narrative.document.get_verb_by_token_id(i) for i in location.token_ids] for location in self.locations]))
        for verb in verbs:
            mentions = list(verb.get_subjects())+list(verb.get_objects())
            for mention in mentions:
                if mention.id in self._mentions_counted: continue
                self._mentions_counted.add(mention.id)
                labels = mention.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES)
                for label in labels:
                    if label and label in taxonomy_labels:
                        self.role_counts[taxonomy_labels.index(label)]+=1
    def get_feature_vector(self):
        return [
            self.actual,
            self.position]+\
            self.role_counts+[
            self.function_group,
            "'"+self.function+"'"
        ]
    @classmethod
    def get_feature_vector_labels(cls):
        return [
            "Is Actual",
            "Func. Position",
            "Ratio Hero",
            "Ratio Villain",
            "Ratio Tester",
            "Ratio Prize",
            "Ratio FalseHero",
            "Ratio Other",
            "Ratio NA",
            "Function Group Label",
            "Function Label",
        ]
    @classmethod
    def get_feature_vector_types(cls):
        return [
            "NUMERIC",
            "NUMERIC",
            "NUMERIC",
            "NUMERIC",
            "NUMERIC",
            "NUMERIC",
            "NUMERIC",
            "NUMERIC",
            "NUMERIC",
            "{alpha,beta,gamma,delta,epsilon,zeta,eta,theta,A,a,B,C,depart,D,E,F,G,H,J,I,K,return,Pr,Rs,o,L,M,N,Q,Ex,T,U,W}",
            "STRING",
        ]
    @classmethod
    def format_feature_vector(cls,vector):
        return '\n'.join(["%s: %.2f" % i for i in zip(cls.get_feature_vector_labels(),vector)])

