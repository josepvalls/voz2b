import settings
import vozbase
import datetime
import util
import entitymanager
import verbmanager
import collections
import inspect
from nltk.tree import Tree,ParentedTree
import logging
import settings
import narrativehelper
import dependencyhelper
import parse_tree_mention_helper

logger = logging.getLogger(__name__)

class Graph(tuple):
    '''
    List of nodes
    Dict of edges
    '''
    pass

class Document(vozbase.VozContainer):
    def __init__(self,text,sentences,properties,document_id=None):
        """
        :param text: str
        :param sentences: list[Sentence]
        :param properties: dict(str,str)
        """
        super(Document, self).__init__()
        self.id = document_id
        self.text = text
        self.sentences = sentences
        self.sentences_removed = []
        self.properties = dict(self.get_default_properties(), **properties)

        self.id_manager = {} #type: dict(str,int)
        self.symbol_manager = {} #type: dict(str,dict(str,int))

        self.coreference = entitymanager.Coreference(self)
        self.coreference_aux = {} #type: dict(str,list[entitymanager.CoreferenceGroup])

        self.narrative = narrativehelper.Narrative(self)

        self._tokens_list = [] #type: list[Token]
        self._tokens_dict = {} #type: dict(int,Token)
        self._sentences_list = sentences #type: list[Sentence]
        self._sentences_dict = {} #type: dict(int,Sentence)
        self._token_to_sentence_dict = {} #type: dict(int,Sentence)
        self._token_to_mention_dict = {} #type: dict(int,entitymanager.Mention)
        self._token_to_verb_dict = {}  #type: dict(int,verbmanager.Verb)
        self._coreference_dict = {}
        self._compute_caches(self)

    def get_new_id(self,collection='Default'):
        if inspect.isclass(collection): collection = collection.__name__
        id_ = self.id_manager.get(collection,0)
        self.id_manager[collection]=id_+1
        return id_
    def get_symbol_id(self,symbol,collection='Default'):
        if collection not in self.symbol_manager:
            self.symbol_manager[collection]={}
        id_ = self.symbol_manager[collection].get(symbol,0)
        self.symbol_manager[collection][symbol]=id_+1
        return id_

    def get_sentence_by_id(self,id):
        """
        :param id: int
        :return: Sentence
        """
        if not self._sentences_dict:
            self._compute_caches(self)
        return self._sentences_dict[id]
    def get_sentence_by_off(self,off):
        last = self.sentences[0]
        for sentence in self.sentences:
            if off < sentence.offset:
                break
            last = sentence
        if not last:
            logger.error("Sentence not found by offset %d, last offset is %d, text length is %d" % (off,self.sentences[-1].offset,self.sentences[-1].offset+self.sentences[-1].len))
        return last

    def get_mention_by_token_id(self, token_id):
        return self.get_mentions_by_token_id(token_id)[0]
    def get_mentions_by_token_id(self,token_id):
        """
        :param id: int
        :return: entitymanager.Mention
        """
        if not self._token_to_mention_dict:
            self._token_to_mention_dict = collections.defaultdict(list)
            for i in self.get_all_mentions():
                for j_i,j in enumerate(i.tokens):
                    if j.pos in ['PRP$','WP$','POS','DT']: continue
                    if len(i.tokens)> j_i+2 and i.tokens[j_i+1].pos in ['PRP$','WP$','POS']: continue
                    self._token_to_mention_dict[j.id].append(i)
            for k,v in self._token_to_mention_dict.items():
                self._token_to_mention_dict[k] = sorted(v,key = lambda i:len(i.tokens))
            if False: # extra mentions in tokens, not used
                for i in self.get_all_tokens():
                    mention = i.get_mention()
                    if mention:
                        for j in mention.tokens:
                            self._token_to_mention_dict[j.id] = mention
        return self._token_to_mention_dict.get(token_id,[None])
    def get_verb_by_token_id(self,token_id):
        """
        :param id: int
        :return: verbmanager.Verb
        """
        if not self._token_to_verb_dict:
            self._token_to_verb_dict = {}
            for i in self.get_all_verbs():
                assert isinstance(i,verbmanager.Verb)
                for j in [i.token]:
                    if not j.id in self._token_to_verb_dict:
                        self._token_to_verb_dict[j.id] = i
                    else:
                        logger.error("Token %d with with multiple verbs: %d, %d" % (j.id,i.id,self._token_to_verb_dict[j.id].id))
        return self._token_to_verb_dict.get(token_id,None)
    def remove_mention(self,mention,remove_from_coref=True,remove_from_sentence=None,remove_from_parent=True,remove_children=True,propagate_parent=True):
        """
        :param mention: entitymanager.Mention
        :param remove_from_coref: bool
        :param remove_from_sentence: Sentence
        :param remove_from_parent: bool
        :param remove_children: bool
        :param make_parent_independent: bool
        :return:
        """
        if propagate_parent and mention.parent_mention:
            if mention.is_independent and mention.parent_mention.child_mentions and len(mention.parent_mention.child_mentions)==1:
                mention.parent_mention.is_independent = True
                logger.info("Removing mention %s, made parent %s independent" % (mention,mention.parent_mention))
            if mention.is_compound and not mention.parent_mention.is_compound:
                mention.parent_mention.is_compound = True
                logger.info("Removing mention %s, made parent %s compound" % (mention,mention.parent_mention))
            if mention.is_list and not mention.parent_mention.is_lis:
                mention.parent_mention.is_list = True
                logger.info("Removing mention %s, made parent %s compound" % (mention,mention.parent_mention))
        if remove_from_sentence:
            assert isinstance(remove_from_sentence,Sentence)
            try:
                remove_from_sentence.mentions.remove(mention)
            except ValueError:
                logger.error("Removing mention %s, not found in sentence %s" % (mention,remove_from_sentence))
        else:
            for sentence in self.sentences:
                try:
                    sentence.mentions.remove(mention)
                except ValueError:
                    pass
        if remove_children:
            for child in mention.child_mentions:
                self.remove_mention(child,remove_from_coref,remove_from_sentence,remove_from_parent,remove_children,False)
        if remove_from_coref:
            self.coreference.remove_mention(mention)

        # invalidate cache
        self._token_to_mention_dict = None

    def remove_sentence(self,sentence,remove_dependencies=True):
        """
        :param sentence: Sentence
        :param remove_dependencies: bool
        :return: None
        """
        occurences = 0
        if remove_dependencies:
            for mention in sentence.mentions:
                occurences += 1
                self.remove_mention(mention)
        self.sentences.remove(sentence)
        self.sentences_removed.append(sentence)

        # invalidate cache
        self._compute_caches(self)

        return occurences


    def _prepare_copy(self):
        for sentence in self.sentences:
            sentence.parse_tree = Tree.convert(sentence.parse_tree)
        for sentence in self._sentences_list:
            sentence.parse_tree = Tree.convert(sentence.parse_tree)
        for k,sentence in self._sentences_dict.items():
            sentence.parse_tree = Tree.convert(sentence.parse_tree)
        for sentence in self.sentences_removed:
            sentence.parse_tree = Tree.convert(sentence.parse_tree)




    def _compute_caches(self,parent):
        """
        :type parent: Document
        :return: None
        """
        self._tokens_list = util.flatten([i.tokens for i in self.sentences])
        self._tokens_dict = util.object_list_to_dict(self._tokens_list)
        self._sentences_list = self.sentences
        self._sentences_dict = util.object_list_to_dict(self.sentences)
        self._token_to_sentence_dict = dict(util.flatten([[(j,i.id) for j in i.tokens] for i in self.sentences]))
        self._token_to_mention_dict = {} #type: dict(int,entitymanager.Mention)
        self._token_to_verb_dict = {} #type: dict(int,entitymanager.Verb)
        for idx,i in enumerate(self.sentences):
            i.idx = idx
            i._compute_caches(self)
        self.coreference._compute_caches(self)
        for i in self.coreference_aux.values():
            i._compute_caches(self)
    def _clear_caches(self,parent):
        del self._tokens_list
        del self._tokens_dict
        del self._sentences_list
        del self._sentences_dict
        del self._token_to_sentence_dict
        del self._coreference_dict
        del self._token_to_mention_dict
        del self._token_to_verb_dict
        for i in self.sentences:
            i._clear_caches(self)
        self.coreference._clear_caches(self)
        for i in self.coreference_aux.values():
            i._clear_caches(self)



    def get_default_properties(self):
        return {'created': datetime.datetime.now()}
    def get_long_id(self):
        ret =  ""
        if self.id:
            ret += "%d" % self.id
        if 'source' in self.properties:
            ret += " (%s)" % self.properties['source']
        return ret
    def get_all(self,property,collection='sentences',pair_container=False):
        lst = []
        for item in getattr(self,collection):
            if not pair_container:
                lst += getattr(item,property)
            else:
                lst += [(item,i) for i in getattr(item,property)]
        return util.remove_duplicates(lst)
    def get_all_mentions(self, filter_only_independent = False):
        mentions = self.get_all('mentions')
        if not filter_only_independent:
            return mentions
        else:
            return [mention for mention in mentions if mention and mention.annotations and mention.is_independent and not mention.annotations.split_ignore]
    def get_all_verbs(self, pair_container=False):
        return self.get_all('verbs', pair_container=pair_container)
    def get_all_tokens(self, pair_container=False):
        return self.get_all('tokens', pair_container=pair_container)
    def compute_predictions_mentions(self, do_voting=True):
        import classificationhelper
        import featuremanager
        fm = featuremanager.FeatureContainer(self).init_features()
        for mention in self.get_all_mentions(filter_only_independent=True):
            feature_vector = fm.get_features(mention)
            mention.predictions.type = classificationhelper.get_label(feature_vector,classificationhelper.TASK_TYPE)
            if mention.predictions.is_character():
                mention.predictions.role = classificationhelper.get_label(feature_vector,classificationhelper.TASK_ROLE)
        if do_voting:
            classificationhelper.do_voting([self], classificationhelper.TASK_TYPE,filter_characters=False)
            #classificationhelper.do_voting([self], classificationhelper.TASK_CHARACTER)
            classificationhelper.do_voting([self], classificationhelper.TASK_ROLE,filter_characters=False)
    def _extend_coref(self, mention, target, mentions):
        to_assign = [i for i in mentions if i.predictions.coref == target.predictions.coref]
        for i in to_assign:
            i.predictions.coref = mention.predictions.coref
    def improve_coref(self, nn_match_aggressive=0):
        '''
        nn_match_aggressive -1: exact match
        nn_match_aggressive 0: exact match for NN, NNP and JJ
        nn_match_aggressive 1: exact match for NN, NNP
        nn_match_aggressive 2: any match for NN, NNP
        :param nn_match_aggressive: int
        :return:
        '''
        if nn_match_aggressive==-1: return
        # TODO port code from loop_aaai
        requirements = ['NN', 'NNP']
        if nn_match_aggressive == 0:
            requirements.append('JJ')
        mentions = self.get_all_mentions(filter_only_independent=True)
        mentions = [i for i in mentions if 'NNS' not in [j.pos for j in i.tokens] and 'NNPS' not in [j.pos for j in i.tokens]]

        for i in range(len(mentions)):
            for j in range(i+1,len(mentions)):
                tokens_i = [k.lemma.lower() for k in mentions[i].tokens if k.pos in requirements]
                tokens_j = [k.lemma.lower() for k in mentions[j].tokens if k.pos in requirements]
                if nn_match_aggressive in [0,1] and tokens_i and tokens_i==tokens_j or nn_match_aggressive==2 and set(tokens_i) & set(tokens_j):
                    self._extend_coref(mentions[i],mentions[j], mentions)
    def improve_verb_args(self):
        # TODO port code from loop_aaai
        pass

    def compute_predictions(self):
        self.improve_coref()
        self.improve_verb_args()
        self.compute_predictions_mentions()



    def format(self,options={'display_tokens':['lemma','pos']}):
        return "Document %s text:\n%s\nDocument Sentences:\n%s" % (self.get_long_id(),self.get_text(),util.format_list(self.sentences,glue='\n',options=options))
    def __str__(self):
        return self.format()
    def get_text(self):
        return self.text
    def get_stats(self):
        return [len(self.sentences),
                len(self.get_all_tokens()),
                len(self.get_all_verbs())
        ]+self.coreference.get_stats()

    @classmethod
    def get_stats_labels(cls):
        return ["Num. Sentences",
                "Num. Tokens",
                "Num. Verbs"
                ]+entitymanager.Coreference.get_stats_labels()
    @classmethod
    def format_stats(cls,stats):
        return '\n'.join(["%s: %.2f" % i for i in zip(cls.get_stats_labels(),stats)])

    def create_entities_from_coref(self):
        pass
        # wipe out entities
        # create an entity for each coref group
        # create singleton entities for mentions not in any coref group

SPEECH_TYPE_CONSEC = 'c'
class SentenceLevelSpan(object):
    def __init__(self, offset, len):
        self.offset = offset
        self.len = len
class SentenceLevelQuotedAnnotations(SentenceLevelSpan):
    def __init__(self, offset, len, speech, speaker='?', listener='?', hint='?', text=''):
        SentenceLevelSpan.__init__(self, offset, len)
        self.speech = speech
        self.speaker_annotation = speaker
        self.listener_annotation = listener
        self.speaker = speaker
        self.listener = listener
        self.hint = hint
        self.verb_token = None
        self.speaker_tokens = []
        self.listener_tokens = []
        self.speaker_mention = None
        self.listener_mention = None
        self.text=text
    def __str__(self):
        if self.speech=='d':
            return "%s, %s (%s) > %s (%s)" % (self.verb_token or '?', self.speaker_mention or '?', self.speaker_annotation or '?',self.listener_mention or '?', self.listener_annotation or '?')
        else:
            return "(%s)" % self.speech
    def init(self, sentence):
        assert isinstance(sentence, Sentence)
        self.offset+=sentence.offset

class SentenceLevelAnnotations(object):
    def __init__(self):
        self.speech = []
        self.speech_data = []
        self.scene_loc = []
        self.scene_time = []
        self.scene = []
        self.verbs = []
    def is_normal(self):
        return len(set(self.speech)) == 1 and self.speech[0] == SPEECH_TYPE_CONSEC
    def init(self, sentence):
        if sentence.id == 2393:
            pass
        for speech in self.speech_data:
            speech.init(sentence)
        for speech_i,speech in enumerate(self.speech_data):
            if speech.hint:
                lookup = 0
                #print speech.hint,speech_i,sentence,sentence.id
                verb=speaker=listener=None
                speaker_=listener_ = None
                if '<' in speech.hint:
                    lookup = -1
                    verb_,speaker_ = speech.hint.split('<')
                elif '>' in speech.hint:
                    lookup = 1
                    verb_,speaker_ = speech.hint.split('>')
                if speaker_ and '/' in speaker_:
                    speaker_,listener_ = speaker_.split('/')

                for i in sentence.tokens:
                    if i.offset <= speech.offset: continue
                    if i.text==verb_ and not verb: verb = i
                    if speaker_ and i.text==speaker_ and not speaker: speaker = i
                    if listener_ and i.text == listener_ and not listener: listener = i
                if speech_i + lookup < 0:
                    # TODO allow annotations to run over to the next sentence
                    speech_data = sentence.get_previous_sentence().annotations.speech_data[-1]
                elif speech_i + lookup >= len(self.speech_data):
                    speech_data = None
                    #speech_data = sentence.get_next_sentence().annotations.speech_data[0]
                    # TODO allow annotations to run over to the next sentence
                else:
                    speech_data = self.speech_data[speech_i + lookup]
                if speech_data:
                    speech_data.verb_token = verb
                    if speaker: speech_data.speaker_tokens = [speaker] # TODO this should be a list of tokens
                    if listener: speech_data.listener_tokens = [listener]  # TODO this should be a list of tokens
        for speech in self.speech_data:
            # using the initial offset of the speech act will bias it towards closest to the beginning, therefore previous
            if speech.speaker_tokens:
                #speech.speaker_mention = sentence._parent_document.get_mention_by_tokens(speech.speaker_tokens)
                speech.speaker_mention = sentence._parent_document.get_mention_by_token_id(speech.speaker_tokens[0].id)
            else:
                speech.speaker_mention = SentenceLevelAnnotations.find_closest_mention(sentence,speech.speaker,speech.offset)
            if speech.listener_tokens:
                speech.listener_mention = sentence._parent_document.get_mention_by_token_id(speech.listener_tokens[0].id)
            else:
                speech.listener_mention = SentenceLevelAnnotations.find_closest_mention(sentence, speech.listener,speech.offset)
    @classmethod
    def find_closest_mention(cls, sentence, symbol, offset):
        # TODO this needs to be one of the mentions previously selected
        mentions = sentence._parent_document.coreference.find_mentions_for_symbol(symbol)
        if mentions:

            mentions = sorted([(abs(offset-i.get_centroid()),i) for i in mentions if i.tokens])
            return mentions[0]
        else:
            logger.warning("NO MENTION FOUND FOR SYMBOL: %s" % symbol)
            return None
    def get_speech_annotations_for_quote(self, quote):
        # rank on overlap
        overlaps = [(min(quote.offset_end,speech.offset+speech.len)-max(quote.offset,speech.offset),speech) for speech in self.speech_data]
        overlaps = sorted(overlaps)
        if overlaps[-1][0]<=0:
            logger.warning('NO OVERLAPS FOR QUOTE: '+str(quote))
        return overlaps[-1][1]
        for speech in self.speech_data:
            if speech.offset>=quote.offset and (quote.offset_end-1)<=speech.offset+speech.len:
                return speech
        else:
            logger.warning('NO SPEECH ANNOTATIONS FOR QUOTE: '+str(quote))
            logger.warning('Q:%d'%quote.offset)
            logger.warning('S:'+','.join([str(i.offset) for i in self.speech_data]))

    def __str__(self):
        return '; '.join([str(i) for i in self.speech_data])


class Sentence(vozbase.VozTextContainer):
    def __init__(self,id,offset,len,tokens):
        """
        :param id: int
        :param offset: int
        :param len: int
        :param tokens: list[Token]
        """
        super(Sentence, self).__init__(id,offset,len)
        self.tokens = tokens

        self.annotations = SentenceLevelAnnotations()
        self.predictions = SentenceLevelAnnotations() # for scene segmentation et al
        self.parse_string = None #type: str
        self.parse_tree = None #type: ParentedTree
        self.parse_highlight = {} #type: dict(str,dict(tuple,int))
        self.mentions = [] #type: list[entitymanager.Mention]
        self.verbs = [] #type: list[verbmanager.Verb]
        self.dependencies = []  #type: list[dependencyhelper.Dependency]
        self.idx = -1

        self._tokens_list = tokens #type: list[Token]
        self._tokens_dict = {} #type: dict(int,Token)
        self._text = ''
        self._parent_document = None #type: Document

    def get_previous_sentence(self):
        return self.get_sentence_by_offset(-1)
    def get_next_sentence(self):
        return self.get_sentence_by_offset(1)
    def get_sentence_by_offset(self,off):
        idx = self._parent_document.sentences.index(self)
        if idx+off>= 0 and idx+off < len(self._parent_document.sentences):
            return self._parent_document.sentences[idx+off]
        else:
            return None
    def _compute_caches(self,parent):
        self._tokens_list = self.tokens
        self._tokens_dict = util.object_list_to_dict(self._tokens_list)
        self._text = parent.text[self.offset:self.offset+self.len].replace('\n', '')
        self._parent_document = parent
        self.parse_tree = ParentedTree.convert(self.parse_tree)
        self._token_to_verb_dict = None
        for idx,i in enumerate(self.tokens):
            i.idx = idx
            i._compute_caches(self)
        for idx,i in enumerate(self.verbs):
            i._compute_caches(self)

    def _clear_caches(self,parent):
        del self._tokens_list
        del self._tokens_dict
        del self._text
        del self._parent_document
        self.parse_tree = Tree.convert(self.parse_tree)
        for i in self.tokens:
            i._clear_caches(self)
        for i in self.verbs:
            i._clear_caches(self)
    def get_token_by_id(self,id):
        if not self._tokens_dict:
            self._compute_caches(self._parent_document)
        return self._tokens_dict[id]
    def get_tokens_by_id_range(self,start,end):
        # TODO token id's may not be sorted in sty files, use offset instead?
        return [self.get_token_by_id(i) for i in range(start,end)]
    def get_tokens_by_idx_range(self,start,end):
        return self.tokens[start:end]
    def get_mention_by_tokens(self,tokens,exact_match_only=False):
        """
        Retrieves the shortest mention that contains all the tokens and its parent
        :param tokens: list[Token]
        :return: typle(entitymanager.Mention,entitymanager.Mention)
        """
        def contains(mention,tokens):
            return mention.tokens[0].offset<=tokens[0].offset and mention.tokens[-1].offset>=tokens[-1].offset
        mention = self.mentions[0]
        while True:
            if mention.child_mentions:
                for child in mention.child_mentions:
                    if contains(child,tokens):
                        mention = child
                        break
                else:
                    break
            else:
                break
        if not exact_match_only or mention.tokens[0].offset==tokens[0].offset and mention.tokens[-1].offset==tokens[-1].offset:
            return mention,mention.parent_mention
        else:
            return None,mention
    def get_parse_node_by_tokens(self,tokens):
        """
        :param tokens: list[Token]
        :return: ParentedTree
        """
        if not self.parse_tree:
            return None
        if tokens[0].idx >= len(self.tokens):
            logger.warning("Parse node from another sentence when getting parse node from tokens "+str(tokens))
            return None

        end_idx = tokens[-1].idx+1
        if end_idx > len(self.tokens):
            logger.warning("Overflow on getting parse node from tokens "+str(tokens))
            end_idx = len(self.tokens)
        if not end_idx >= tokens[0].idx:
            logger.warning("Wrong offsets "+str(tokens))
            return None


        idx = self.parse_tree.treeposition_spanning_leaves(tokens[0].idx,end_idx)
        while idx and not isinstance(self.parse_tree[idx],ParentedTree):
            idx = idx[0:-1]

        return self.parse_tree[idx]
    def get_mention_by_token_dependencies(self,token):
        """
        Retrieves the longest mention including all the children dependencies for Token token
        :param token: Token
        :return: entitymanager.Mention
        """

    def get_text(self):
        if not self._text:
            self._compute_caches(self._parent_document)
        return self._text


    def format(self,options={'display_tokens':['lemma','pos']}):
        return util.format_list(self.tokens,glue=' ',options=options)
    def __str__(self):
        return self.format()
    def __repr__(self):
        return 'Sentence %d/%s' % (self.id,str(self))
    def format_quoted_annotations(self):
        if 'd' in self.annotations.speech:
            ret = ''
            text = self.get_text()
            for speech in self.annotations.speech_data:
                assert isinstance(speech,SentenceLevelQuotedAnnotations)
                text_ = text[speech.offset:speech.offset + speech.len]
                if speech.speech == 'd':
                    ret += '\t' + speech.speaker + '->' + speech.listener + " " + text_ + '\n'
                else:
                    ret += '\t(' + speech.speech +') '+ text_ + '\n'
        else:
            ret = self.format()
        return ret

class Token(vozbase.VozTextContainer):
    def __init__(self,id,offset,length,pos,lemma,text):
        """
        :param id: int
        :param offset: int
        :param length: int
        :param pos: str
        :param lemma: str
        :param text: str
        """
        super(Token, self).__init__(id,offset,length)
        self.pos = pos or '?'
        self.lemma = lemma
        self.text = text
        self.idx = -1
        self._parent_sentence = None #type: Sentence

    @classmethod
    def filter(cls,tokens,pos='',pos_list=[]):
        if pos and not pos_list:
            pos_list = [pos]
        pos_list = [i for i in pos_list if i]
        if pos_list:
            tokens = [i for i in tokens if i.pos in pos_list]
        # TODO filter by other fields
        return tokens
    @classmethod
    def centroid(cls,tokens):
        a = tokens[0].offset
        b = tokens[-1].offset + tokens[-1].len
        return (a+b)/2
    def _compute_caches(self,parent):
        self._parent_sentence = parent
    def _clear_caches(self,parent):
        del self._parent_sentence
    def equals(self,other,fields=['pos','lemma','text']):
        """
        Checks if two tokens are equals based on certain fields
        :param other: Token
        :param fields: list[str]
        :return: bool
        """
        for name in fields:
            if not getattr(self, name, '?') == getattr(other, name, '?'):
                return False
        return True

    def format(self,options={}):
        return util.format_list([getattr(self, name, '?') for name in options.get('display_tokens',['lemma','pos'])],glue='/',options=options)
    def __str__(self):
        return self.format()
    def __repr__(self):
        return 'Token %d/%s/%s' % (self.id,self.lemma,self.pos)
    def get_text(self):
        return self.text
    def get_mention_ids(self, only_independent=False):
        node = self.get_parse()
        nodes = parse_tree_mention_helper.get_node_parents(node)[0:-1]
        key = parse_tree_mention_helper.HIGHLIGHT_MENTIONS_INDEPENDNET if only_independent else parse_tree_mention_helper.HIGHLIGHT_MENTIONS
        d = self._parent_sentence.parse_highlight.get(key,{})
        mentions = filter(lambda i: i is not None, [d.get(i.treeposition(), None) for i in nodes])
        return mentions
    def get_mention(self):
        '''
        Note, this uses the parse information, to use the cached information loaded from the sty file use sentence._parent_document.get_mention_by_token_id(tokens_ref[0].id)
        :return:
        '''
        mention_ids = self.get_mention_ids(True)
        if mention_ids:
            return util.get_from_list(self._parent_sentence.mentions,mention_ids[0])
        else:
            return None
    def get_parse(self):
        if not self._parent_sentence.parse_tree:
            logger.error("PARSE NOT AVAILABLE FOR THIS SENTENCE")
            return None
        if not self.idx >= 0:
            logger.error("TOKEN IDX NOT VALID")
            return None
        parse = self._parent_sentence.parse_tree
        return parse[parse.leaf_treeposition(self.idx)[0:-1]]

    def get_previous(self):
        return self.get_by_offset(-1)
    def get_next(self):
        return self.get_by_offset(1)
    def get_by_offset(self,off):
        idx = self._parent_sentence.tokens.index(self)
        if idx+off>= 0 and idx+off < len(self._parent_sentence.tokens):
            return self._parent_sentence.tokens[idx+off]
        else:
            return None



def create_document_from_jsonpickle_file(json_file):
    """
    Loads a Document
    :param json_file: str
    :return: Document
    """
    return create_document_from_jsonpickle(open(json_file).read())
def create_document_from_jsonpickle(json):
    """
    Loads a Document
    :param json: str
    :return: Document
    """
    doc = vozbase.jsonpickle.decode(json)
    doc._compute_caches(doc)
    return doc

def create_document_from_text(str_input):
    """
    Creates a Document from text
    :param str_input:
    :return: Document
    """
    import re
    splitter = re.compile(r'\w+|[^\w\s]+')
    id_token = 0
    id_sentence = 0
    offset = 0
    sentences = []
    for str_sent in str_input.splitlines(True):
        tokens = []
        for match in splitter.finditer(str_sent):
            text = match.group()
            token = Token(id_token,offset+match.start(),len(text),None,text.lower(),text)
            tokens.append(token)
            id_token+=1
        sentence = Sentence(id_sentence,offset,len(str_sent),tokens)
        sentences.append(sentence)
        id_sentence +=1
        offset += len(str_sent)
    return Document(str_input,sentences,{'source':'create_document_from_text','created':datetime.datetime.now()})


def main():
    """
    Tests basic document creation, serializing and unserializing
    """
    doc = create_document_from_text("Hello, this is dog.\nHow are you?")
    print doc
    file_name = 'temp_document.json'
    doc.serialize_to_file(file_name)
    doc_copy = create_document_from_jsonpickle_file(file_name)
    assert(str(doc)==str(doc_copy))

if __name__ == '__main__':
    main()