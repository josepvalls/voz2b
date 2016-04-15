import settings
import voz
import logging
import os
import util
from bs4 import BeautifulSoup
import formatter
from nltk.tree import Tree,ParentedTree
import csv
import collections
import pickle
import narrativehelper
import re

logger = logging.getLogger(__name__)

class StyFile(object):
    def __init__(self, path=''):
        """
        :param path: str
        """
        logger.info('Processing '+path)
        self.d = BeautifulSoup(open(path).read(), 'xml')
        self.path = path
        self.story_id = path.split(os.path.sep)[-1].split()[0].lstrip('0')
        self.sentences = [] # :type list[voz.Sentence]
        self.tokens = [] # :type list[voz.Sentence]

    def to_document(self,properties={}):
        """
        :return: voz.Document
        """
        str_input = self.get_original_text()
        sentences = []
        properties = dict({'source':'create_document_from_sty_file'}, **properties)
        self.document = voz.Document(str_input,sentences,properties) #type: voz.Document
        self.document.id = int(self.story_id) if util.is_numeric_int(self.story_id) else -1

        try:
            afanasev = re.findall(r'corresponds[^\d]*(\d*)[^\d]*(\d*)',str_input,re.IGNORECASE)
            self.document.properties['afanasev_new'] = afanasev[0][0]
            self.document.properties['afanasev_old'] = afanasev[0][1]
        except:
            pass

        self._init_tokens()
        self._init_sentences()
        self.document._compute_caches(self.document)
        self._init_parse()
        self._init_mentions()
        self._init_coref()
        self._init_verbs()
        self._init_gt()
        self._clean_mentions_and_coref()
        self._init_gt_data_old()
        self._init_functions()
        return self.document
    def _clean_mentions_set_hierarchy(self,mentions):
        """
        :param mentions: list(voz.entitymanager.Mention)
        :return: list(voz.entitymanager.Mention)
        """
        parents = []
        mentions = sorted(mentions,key=lambda i:i.tokens[0].offset*1000-len(i.tokens))
        while mentions:
            parent = mentions.pop(0)
            children = []
            mentions_rest = []
            for mention in mentions:
                if mention.tokens[0].offset >= parent.tokens[0].offset and mention.tokens[-1].offset <= parent.tokens[-1].offset:
                    children.append(mention)
                else:
                    mentions_rest.append(mention)
            mentions = mentions_rest
            parent.child_mentions = self._clean_mentions_set_hierarchy(children)
            parents.append(parent)
        return parents
    def _clean_mentions_set_tags(self,mentions):
        for mention in mentions:
            if mention.child_mentions:
                mention.is_compound = True
                self._clean_mentions_set_tags(mention.child_mentions)
            else:
                mention.is_independent=True
    def _clean_mentions_and_coref(self):
        token_to_mention_dict = collections.defaultdict(list)
        for mention in self.document.get_all_mentions():
            for j in mention.tokens:
                token_to_mention_dict[j].append(mention)
        # fixing the hierarchy of mentions
        # fixing the mention tags
        mention_groups = util.remove_duplicates([tuple(i) for i in token_to_mention_dict.values()])
        for mentions in mention_groups:
            if len(mentions)==1:
                mentions[0].is_independent=True
            else:
                logger.info("Multiple mentions in a token: %d" % len(mentions))
                mentions = self._clean_mentions_set_hierarchy(mentions)
                self._clean_mentions_set_tags(mentions)
                for mention in mentions:
                    if not mention.is_independent: self.document.remove_mention(mention)
        # fixing split coreference groups
        for entity in self.document.coreference.entities:
            if entity.number_of_distinct_coref_groups()>1:
                groups = entity.distinct_coref_groups()
                head = groups.pop(0)
                for group in groups:
                    util.union_list_without_duplicates(head.mentions,group.mentions)
                    self.document.coreference.remove_coref_and_entity(group.id)


        # create singleton coreference groups
        mentions = set(self.document.get_all_mentions())
        for coref in self.document.coreference.get_coreference_groups():
            for mention in coref.mentions:
                try:
                    mentions.remove(mention)
                except ValueError:
                    pass
        logger.info("Singleton mentions %d"%len(mentions))
        for mention in mentions:
            if 'CH' in mention.get_taxonomy(voz.entitymanager.TaxonomyContainer.TAXONOMY_NONCHARACTER):
                logger.info("Singleton mention character %s"%mention)

        pass


    def _init_parse(self):
        idx = 0
        for i in self.d.select('rep#edu.mit.parsing.parse')[0].select('desc'):
            parse_off = int(i.attrs.get('off'))
            sentence = self.document.sentences[idx]
            idx +=1
            assert isinstance(sentence,voz.Sentence)
            #logger.info("Parse offsets %d - %d" % (sentence.offset, parse_off))
            assert sentence.offset == parse_off
            parse_string = i.text.strip()
            sentence.parse_string = parse_string
            sentence.parse_tree = ParentedTree.fromstring(parse_string)
            assert len(sentence.parse_tree.leaves())==len(sentence.tokens)
            for i in xrange(len(sentence.tokens)):
                sentence.parse_tree[sentence.parse_tree.leaf_treeposition(i)]=sentence.tokens[i]


    def _init_tokens(self):
        tokens = []
        pos_tag_dict = [(i.text.strip().split(),int(i.attrs.get('id'))) for i in self.d.select('rep#edu.mit.parsing.pos')[0].select('desc')]
        pos_tag_dict = dict([(int(i[0][0]),(i[0][1],i[1])) for i in pos_tag_dict])
        stem_tag_dict = [i.text.strip().split() for i in self.d.select('rep#edu.mit.parsing.pos')[0].select('desc')]
        stem_tag_dict = dict([(int(i[0]),i[1]) for i in stem_tag_dict])

        for i in self.d.select('rep#edu.mit.parsing.token')[0].select('desc'):
            token_id = int(i.attrs.get('id'))
            token_text = i.text.strip()
            pos,pos_id = pos_tag_dict[token_id]
            lemma = stem_tag_dict.get(pos_id,token_text)
            tokens.append(voz.Token(token_id,int(i.attrs.get('off')),int(i.attrs.get('len')),pos,lemma,token_text))
        self.document._tokens_list = tokens
        self.document._tokens_dict = dict([(i.id,i) for i in tokens])
    def _init_sentences(self):
        sentences = []
        for i in self.d.select('rep#edu.mit.parsing.sentence')[0].select('desc'):
            sentence_id = int(i.attrs.get('id'))
            tokens = [self.document._tokens_dict[int(j)] for j in i.text.strip().split('~')]
            sentences.append(voz.Sentence(sentence_id,int(i.attrs.get('off')),int(i.attrs.get('len')),tokens))
        self.document.sentences = sentences

    def _init_mentions(self):
        mentions = []
        for i in self.d.select('rep#edu.mit.discourse.rep.refexp')[0].select('desc'):
            mention_id = int(i.attrs.get('id'))
            tokens = [self.document._tokens_dict[int(j)] for j in util.flatten([j.split('~') for j in i.text.strip().split(',')])]
            mentions.append(voz.entitymanager.Mention(mention_id, tokens))
        for mention in mentions:
            sentence = mention.tokens[0]._parent_sentence
            node = sentence.get_parse_node_by_tokens(mention.tokens)
            if node:
                assert isinstance(sentence.parse_tree,ParentedTree)
                if 'mentions' not in sentence.parse_highlight: sentence.parse_highlight['mentions'] = {}
                if 'mentions_independent' not in sentence.parse_highlight: sentence.parse_highlight['mentions_independent'] = {}
                sentence.parse_highlight['mentions'][node.treeposition()]=mention.id
                # TODO properly compute mention.is_independent
                sentence.parse_highlight['mentions_independent'][node.treeposition()]=mention.id
                sentence.mentions.append(mention)
            else:
                logger.warning("No parse node found for mention "+str(mention))
        self._mentions = util.object_list_to_dict(mentions)

    def _init_coref(self):
        for i in self.d.select('rep#edu.mit.discourse.rep.coref')[0].select('desc'):
            # <desc id="1084" len="3145" off="350">A dragon|864,866,871,876,877,880,885,887,888,1142,895,919,921,922,924,926,930,946,969,981,992,994,998,999,1000,1004,1007,1010,1011,1015,1018,1019,1021,1027,1033,1041,1043,1046,1050,1058,1059,1061,1065,1066</desc>
            # <desc id="1085" len="2839" off="373">Kiev|865,929,937,945,1045,1048</desc>
            representation, data = i.text.split('|')
            id = int(i.attrs.get('id'))
            entity = voz.entitymanager.Entity(id,representation)
            entity.symbol = id
            entity._compute_caches(self.document)
            mentions = [self._mentions[int(i)] for i in data.split(',')]
            # redundant
            '''for mention in mentions:
                assert isinstance(mention,voz.entitymanager.Mention)
                mention.add_tag(voz.entitymanager.TaggableContainer.TAG_CHARACTER_SYMBOL,id)'''
            self.document.coreference.entities.append(entity)
            self.document.coreference.add_coreference_group(id,mentions,entity,representation)

    def _init_gt(self):
        self._init_gt_wordnet_senses()
        self._init_gt_roles()
        #self._init_gt_data_old()
    def _init_gt_roles(self):
        # load ground truth
        entity_id_to_key = dict([((int(j[0]),int(j[1])),j[3]) for j in [i.split('\t') for i in open(settings.STY_FILE_PATH+settings.STY_ENTITY_TO_KEY).readlines()]])
        key_to_role = csv.reader(open(settings.STY_FILE_PATH+settings.STY_KEY_TO_ROLE,'rU'))
        ENTITY_TYPE = 3
        ENTITY_ROLE3 = 4
        ENTITY_ROLE6 = 6
        ENTITY_ROLES = 7
        ENTITY_SYMBOL = 2
        ENTITY_GROUP = 8

        key_to_role = dict([((int(j[0]),j[ENTITY_GROUP]),j) for j in key_to_role if j[0].isdigit()])

        def add_annotation(entity,taxonomy,key,column):
            data = key_to_role.get(key,None)
            if data:
                if data[column].strip():
                    entity.add_taxonomy(taxonomy,data[column].strip())

        for entity in self.document.coreference.entities:
            key = (self.document.id,entity.id)
            entity.symbol = entity_id_to_key[key]

            for symbol in entity.symbol.split(','):
                key = (self.document.id,symbol.strip())

                add_annotation(entity,voz.entitymanager.TaxonomyContainer.TAXONOMY_ENTITY_TYPES,key,ENTITY_TYPE)
                add_annotation(entity,voz.entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_3ROLES,key,ENTITY_ROLE3)
                add_annotation(entity,voz.entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES,key,ENTITY_ROLE6)

                types = entity.get_taxonomy(voz.entitymanager.TaxonomyContainer.TAXONOMY_ENTITY_TYPES)
                if not types:
                    entity.add_taxonomy(voz.entitymanager.TaxonomyContainer.TAXONOMY_NONCHARACTER,'NC')
                    types = ['NC']
                else:
                    types = [voz.entitymanager.taxonomy_dict_aux_type_to_parent[(voz.entitymanager.TaxonomyContainer.TAXONOMY_NONCHARACTER,i)] for i in types]
                    entity.set_taxonomy(voz.entitymanager.TaxonomyContainer.TAXONOMY_NONCHARACTER,types)

                for mention in self.document.coreference.get_coreference_group_by_id(entity.id).mentions:
                    assert isinstance(mention,voz.entitymanager.Mention)
                    mention.add_tag(voz.entitymanager.TaggableContainer.TAG_CHARACTER_SYMBOL,entity.symbol)
                    mention.set_taxonomy(voz.entitymanager.TaxonomyContainer.TAXONOMY_NONCHARACTER,types)
                    mention.set_taxonomy(voz.entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES,entity.get_taxonomy(voz.entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES))
    def _init_gt_data_old(self):
        data = [i.split('\t') for i in open("/Users/josepvalls/voz-nlp/finlayson_gt_entities.csv").readlines()]
        if True: # load restrictions
            restrictions = pickle.load(open('/Users/josepvalls/voz-nlp/make_restriction_table.pickle', 'rb'))
            for a,b in zip(data,restrictions):
                a+=[b.strip()]
        data = [[int(i[0])]+i[1:] for i in data if i[0].isdigit()]
        data_labels = [i.split('\t') for i in open("/Users/josepvalls/voz-nlp/character-labels-ijcai2.csv").readlines()]
        data_labels = dict([((int(i[0]),int(i[1])),i) for i in data_labels[1:] if i[0].isdigit()])
        DATA_TEXT = 3
        DATA_TYPE = 11
        DATA_SYMBOL = 12

        for mention in self.document.get_all_mentions():
            if mention.is_independent:
                rows = [i for i in data if i[0]==self.document.id and i[DATA_TEXT]==mention.get_text().strip()]
                logger.info("found %d rows" % len(rows))
                if rows:
                    row = rows[0]
                    data.remove(row)
                    key = (int(row[0]),int(row[1]))
                    labels = data_labels[key]
                    mention.add_tag('OLD_SYMBOL',row[DATA_SYMBOL])
                    mention.add_tag('OLD_TYPE',row[DATA_TYPE])
                    mention.add_tag('OLD_IDX',labels[1])
                    mention.add_tag('OLD_STANFORD_COREF',labels[2])
                    mention.add_tag('OLD_NAME_COREF',labels[3])
                    mention.add_tag('OLD_ROLE_GT',labels[13])
                    mention.add_tag('OLD_ROLE_PRED1',labels[16])
                    mention.add_tag('OLD_ROLE_PRED2',labels[17])
                    mention.add_tag('OLD_ROLE_PRED3',labels[18])
                    mention.add_tag('OLD_RESTRICTION',row[-1])
                    #mention.add_tag(voz.entitymanager.TaggableContainer.TAG_CHARACTER_SYMBOL_OLD,':'.join(labels))
                    #mention.add_taxonomy(voz.entitymanager.TaxonomyContainer.TAXONOMY_ENTITY_TYPES_OLD,row[DATA_TYPE])
                else:
                    logger.warning("Mention not found %s" % mention)
    def _init_gt_wordnet_senses(self):
        # <desc id="2597" len="8" off="683">WID-01842204-V-01-go_out,,1143,1433,2346</desc>
        # <desc id="2602" len="8" off="747">WID-02074677-V-01-escape,USER:,90,1447,2349</desc>
        # <desc id="2603" len="8" off="761">WID-10474064-N-01-princess,USER:,93,1450,</desc>
        # token_id|colloc_id, pos_id, stem_id (inc phrasal verb)

        collocations = {} #type: dict(int,list[int])
        for i in self.d.select('rep#edu.mit.parsing.colloc')[0].select('desc'):
            id = int(i.attrs.get('id'))
            collocations[id] = [int(j) for j in i.text.split(',')]

        for i in self.d.select('rep#edu.mit.wordnet.sense')[0].select('desc'):
            data,user,token_id,_ = i.text.split(',',3)
            if data=='NO_SENSE' or not data.startswith('WID'): continue
            data_source,data_offset,data_pos,_ = data.split('-',3)

            token_id = int(token_id)
            if token_id in collocations:
                token_ids = collocations[token_id]
            else:
                token_ids = [token_id]

            if data_pos=='N':
                for token_id in token_ids:
                    try:
                        mention = self.document.get_mention_by_token_id(token_id)
                        assert isinstance(mention,voz.entitymanager.Mention)
                        mention.add_tag(voz.entitymanager.TaggableContainer.TAG_WORDNET_SENSE,data)
                    except Exception as e:
                        logger.warn("Couldn't assign Wordnet tag to mention at token: %s with data %s" % (self.document._tokens_dict[token_id],data))

            #elif data_pos=='V': R A
    def _init_functions(self):
        for i in self.d.select('rep#edu.mit.semantics.rep.function')[0].select('desc'):
            id = int(i.attrs.get('id'))
            offset = int(i.attrs.get('off'))
            length = int(i.attrs.get('len'))
            function,locations_str = i.text.split('|',1)
            locations = []
            for kind_group in locations_str.split('|'):
                kind,groups = kind_group.split(':')
                for group in groups.split(','):
                    locations.append(narrativehelper.NarrativeFunctionLocation(kind,[int(k) for k in group.split('~')]))

            self.document.narrative.add_function(id,offset,length,function,locations)


    def _init_verbs(self):
        import verbmanager
        def parse_args(arg, parse):
            def go_up_parse(parse, start, up):
                location = parse.leaf_treeposition(start)[0:-1]
                node = parse[location]
                for _ in range(up):
                    node = node.parent()
                return node.leaves()
            args = arg.split('-')
            srl_label = args[1].replace('RG', '')
            if args[2]:
                srl_label += '-' + args[2]
            tokens = []
            for words in args[0].split(','):
                args = words.split(':')
                tokens_ = go_up_parse(parse, int(args[0]), int(args[1]))
                tokens+=tokens_
            return (srl_label,tokens)

        #sentences = self.get_sentences()
        for e in self.d.select('rep#edu.mit.semantics.semroles')[0].select('desc'):
            #<desc id="2446" len="27" off="350">2 user appear.01 ----a 0:1-ARG1- 3:1-ARGM-LOC</desc>
            data = e.text.split()
            off = int(e.attrs.get('off', 0))
            len = int(e.attrs.get('len', 0))
            sentence = self.document.get_sentence_by_off(off)
            verb_token = sentence.tokens[int(data[0])]
            verb_frame = data[2]
            parse = ParentedTree.convert(sentence.parse_tree)
            verb_args = dict([parse_args(i, parse) for i in data[4:]])
            verb = verbmanager.Verb(int(e.attrs.get('id')),off,len,verb_token,verb_frame,verb_args)
            sentence.verbs.append(verb)
        self.document._compute_caches(self.document)

    def get_original_text(self):
        """
        Gets the full original text of the story.
        :return: str
        """
        return self.d.select('rep#edu.mit.story.char')[0].text.strip()

def create_document_from_sty_file(sty_file,properties={}):
    """
    Creates a Document from a sty file
    :param sty_file: str
    :return: voz.Document
    """

    doc = StyFile(sty_file).to_document(properties)
    return doc



def main():
    logging.basicConfig(level=logging.DEBUG)
    file_path = settings.STY_FILE_PATH
    story_file = settings.STY_FILES[0]
    doc = create_document_from_sty_file(file_path+story_file) #type: voz.Document
    #print doc
    coref = doc.coreference.get_coreference_groups()[0]
    print "Wordnet annotations for %d"%coref.id,coref.get_tag(voz.entitymanager.TaggableContainer.TAG_WORDNET_SENSE)
    for mention in doc.get_all_mentions():
        print mention
    print voz.Document.format_stats(doc.get_stats())
    #open('test_output.html','w').write(formatter.html(formatter.VozHTMLFormatter.format(doc,options={'parse_highlight':'mentions'})))

if __name__ == '__main__':
    main()