import vozbase
import util
from string import ascii_letters
import verbmanager

DO_COMPUTE_ROLE_DISTRIBUTION = True
DO_EXPORT_TEXT_ANNOTATION_IN_ARFF = False

VERB_FEATURES = 1 # levin base
#VERB_FEATURES = 2 # levin tfidf
#VERB_FEATURES = 3 # wordnet root
#VERB_FEATURES = 4 # similarity
VERB_FEATURES = 5 # framenet

DO_USE_OLD_AUTO_DATA_INSTEAD_OF_STY_GT = False

class Narrative(vozbase.VozContainer):
    """
    Models narrative
    """
    def __init__(self,document):
        self.id = document.get_new_id(Narrative)
        self.document = document
        self.function_list = []
        self.filter_non_actual_default = True
    def compute_features(self):
        #for function in self.functions():
        for function in self.function_list:
            assert isinstance(function,NarrativeFunction)
            function._compute_features(self)
    def add_function(self,id,offset,length,function,locations):
        self.function_list.append(NarrativeFunction(id,offset,length,function,locations))
    def format_summary(self):
        return '|'.join([i.function for i in self.functions()])
    def format(self,options={}):
        if(options.get('one_liner',False)):
            return util.format_list(self.functions(),' ',options=options)
        else:
            return util.format_list(self.functions(),'\n',options=options)
    def functions(self,filter_non_actual=None):
        if filter_non_actual is None: filter_non_actual = self.filter_non_actual_default
        return self.function_list if not filter_non_actual else [i for i in self.function_list if i.actual and (i.tokens_count is None or i.tokens_count>0)]



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
        for function in self.functions():
            arff += ','.join([str(i) for i in function.get_feature_vector()]) + "\n"
        return arff
    def format_tsv(self,include_header=True):
        tsv = ""
        if include_header:
            tsv += '\t'.join(NarrativeFunction.get_feature_vector_labels())+"\n"
        for function in self.functions():
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
        self.function_group = NarrativeFunction.translate_function(function_)
        self._mentions_explicit = []
        self._mentions_implicit = []
        self._verbs = []
        self.role_counts = None
        self.actual = 1.0 if 'ACTUAL' in [i.kind for i in self.locations] else 0.0
        self.position = 0.0
        self.tokens_count = None
    def format(self,options={}):
        if(options.get('use_function_group',False)):
            repr = self.function_group
        else:
            repr = self.function

        if(options.get('one_liner',False)):
            return repr
        else:
            return "%s: %s" % (repr, util.format_list(self.locations, '; '))
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
        midpoint = 1.0*(self.offset+self.offset+self.len)/2.0
        self.position = midpoint / len(document.get_text())

        if self.actual:
            # this accounts for removed dialogue
            tokens = set([i.id for i in narrative.document.get_all_tokens()]) & set(util.flatten([[i for i in location.token_ids] for location in self.locations]))
            self.tokens_count = len(tokens)
        else:
            self.tokens_count = 0


        verbs = self._compute_features_verbs(narrative)
        self._compute_features_mentions(narrative,verbs)
    def _compute_features_verbs(self,narrative):
        if not self._verbs:
            verbs = filter(None,util.flatten([[narrative.document.get_verb_by_token_id(i) for i in location.token_ids] for location in self.locations]))
            self._verbs = verbs
        return self._verbs

    def _compute_features_mentions(self,narrative,verbs):
        if not self.role_counts:
            import entitymanager
            taxonomy_labels = entitymanager.taxonomy_dict[entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES].labels
            self.role_counts = [0 for _ in taxonomy_labels]
            self._mentions_counted = set()
            self._compute_features_mentions_explicit(narrative,taxonomy_labels)
            if not DO_USE_OLD_AUTO_DATA_INSTEAD_OF_STY_GT:
                self._compute_features_mentions_implicit(narrative,taxonomy_labels,verbs)
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
            if DO_USE_OLD_AUTO_DATA_INSTEAD_OF_STY_GT:
                labels = mention.get_tag('OLD_ROLE_PRED1')
            else:
                #print mention.format_all_taxonomies()
                labels = mention.get_taxonomy(entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES)
            for label in labels:
                if label and label in taxonomy_labels:
                    self.role_counts[taxonomy_labels.index(label)]+=1
    def _compute_features_mentions_implicit(self,narrative,taxonomy_labels,verbs):
        import entitymanager
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
            self.role_counts+\
            self.get_feature_vector_verbs()+[
            self.function_group,]+\
               (["'"+self.function+"'"] if DO_EXPORT_TEXT_ANNOTATION_IN_ARFF else [])

    @classmethod
    def get_feature_vector_labels_verbs(cls):
        if VERB_FEATURES==1:
            return ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', '50', '51', '52', '53', '54', '55', '56', '57']
        elif VERB_FEATURES==2:
            return ['39.2','36.2','51.3.2','29.5','40.1.2','31.2','11.2','22.4','13.6','26.2']
        elif VERB_FEATURES==3:
            return ['move','act','change','travel','make','be','think','transfer','get','express','connect','remove','have','touch','consume']
        elif VERB_FEATURES==4:
            return ['do','be','think','talk','answer','ask','punish','help','reward','move']
        elif VERB_FEATURES==5:
            return ['Abusing','Moving_in_place','Becoming_a_member','Motion','Cause_motion','Possession','Self_motion','Becoming_aware','Body_movement','Ingestion','Request','Giving','Questioning','Communication','Manipulation','Killing','Forming_relationships','Grooming','Storing']
    def get_feature_vector_verbs(self):
        if VERB_FEATURES==1:
            vector_lst = self.__class__.get_feature_vector_labels_verbs()
            vector = [0.0 for _ in vector_lst]
            mapper = verbmanager.VerbMapper(verbmanager.VerbMapper.MODE_LEVIN_TEXT)
            for verb in self._verbs:
                verb = mapper.map(verb.token.lemma).split('.')[0]
                if verb in vector_lst:
                    vector[vector_lst.index(verb)]=1.0
            return vector
        elif VERB_FEATURES==2:
            vector_lst = [i.split('.')[0] for i in self.__class__.get_feature_vector_labels_verbs()]
            vector = [0.0 for _ in vector_lst]
            mapper = verbmanager.VerbMapper(verbmanager.VerbMapper.MODE_LEVIN_TEXT)
            for verb in self._verbs:
                verb = mapper.map(verb.token.lemma).split('.')[0]
                if verb in vector_lst:
                    vector[vector_lst.index(verb)]=1.0
            return vector
        elif VERB_FEATURES==3:
            from nltk.corpus import wordnet as wn
            verbs = set(util.flatten(util.flatten(util.flatten([[[j.lemma_names() for j in i.root_hypernyms()] for i in wn.synsets(verb.token.lemma, 'v')] for verb in self._verbs]))))
            vector_lst = self.__class__.get_feature_vector_labels_verbs()
            vector = [0.0 for _ in vector_lst]
            for verb in verbs:
                if verb in vector_lst:
                    vector[vector_lst.index(verb)]=1.0
            return vector
        elif VERB_FEATURES==4:
            vector_lst = self.__class__.get_feature_vector_labels_verbs()
            vector = []
            verbs_groups = [
                            ['do', 'act', 'get'],
                            ['be', 'can', 'want', 'have', 'live'],
                            ['think', 'love', 'hate'],
                            ['talk', 'communicate', 'say', 'tell', 'order', 'command'],
                            ['answer', 'reply', 'respond'],
                            ['ask', 'inquire', 'test', 'judge', 'try'],
                            ['punish','kill', 'kidnap', 'pursue'],
                            ['help', 'rescue', 'save','hide'],
                            ['reward', 'pay', 'give'],
                            ['move','go','transport', 'arrive','carry'],
                            ]
            from nltk.corpus import wordnet as wn

            cloud_s = []
            for verb in self._verbs:
                cloud_s.extend(wn.synsets(verb.token.lemma,pos=wn.VERB))

            for group in verbs_groups:
                cloud_a = []
                for verb in group:
                    cloud_a.extend(wn.synsets(verb, pos=wn.VERB))

                if not cloud_s or not cloud_a:
                    vector.append(0.0)
                else:
                    similarity = max([max([i.wup_similarity(j) for j in cloud_s]) for i in cloud_a])
                    vector.append(similarity)
            return vector
        elif VERB_FEATURES==5:
            vector_lst = [i for i in self.__class__.get_feature_vector_labels_verbs()]
            vector = [0.0 for _ in vector_lst]
            mapper = verbmanager.VerbMapper(verbmanager.VerbMapper.MODE_FRAMENET_TEXT)
            for verb in self._verbs:
                verb = mapper.map(verb.token.lemma)
                if verb in vector_lst:
                    vector[vector_lst.index(verb)]=1.0
            return vector

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
            "Ratio NA"]+\
            cls.get_feature_vector_labels_verbs()+[
            "Function Group Label",]+\
               (["Function Label",] if DO_EXPORT_TEXT_ANNOTATION_IN_ARFF else [])
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
            "NUMERIC",]+\
            ["NUMERIC"]*len(cls.get_feature_vector_labels_verbs())+[
            "{alpha,beta,gamma,delta,epsilon,zeta,eta,theta,A,a,B,C,depart,D,E,F,G,H,J,I,K,return,Pr,Rs,o,L,M,N,Q,Ex,T,U,W}",]+\
               (["STRING",] if DO_EXPORT_TEXT_ANNOTATION_IN_ARFF else [])
    @classmethod
    def format_feature_vector(cls,vector):
        return '\n'.join(["%s: %.2f" % i for i in zip(cls.get_feature_vector_labels(),vector)])

