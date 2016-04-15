import settings
import voz
import networkcachemanager
from xml.dom import minidom
import logging
import parse_tree_mention_helper
import formatter
from nltk.tree import ParentedTree

logger = logging.getLogger(__name__)

def create_document_from_raw_text(str_input,properties={}):
    raw_xml_data = networkcachemanager.stanford_nlp.query(str_input)
    return annotate_document_from_corenlp(str_input,raw_xml_data)

def annotate_document_from_xml_file(str_input,xml_file,properties={}):
    return annotate_document_from_corenlp(str_input,open(xml_file).read())

def annotate_document_from_corenlp(str_input,raw_xml_data,properties={}):
    properties = dict({'source':'annotate_document_from_corenlp'}, **properties)
    sentences = []
    xmldoc = minidom.parseString(raw_xml_data)
    for sentence_xml in xmldoc.getElementsByTagName('sentences')[0].getElementsByTagName('sentence'):
        tokens = []
        id_ = int(sentence_xml.getAttribute('id'))
        tokens = [token_from_xml(xml,str_input) for xml in sentence_xml.getElementsByTagName('tokens')[0].getElementsByTagName('token')]
        if tokens:
            offset = tokens[0].offset
            offset_end = tokens[-1].offset+tokens[-1].len
        sentence = voz.Sentence(id_,offset,offset_end-offset,tokens)
        sentences.append(sentence)
    document = voz.Document(str_input,sentences,properties) #type: voz.Document
    document.id = properties.get('story_id',-1)
    document._compute_caches(document)


    _annotate_document_from_corenlp_mentions(xmldoc,document)
    _annotate_document_from_corenlp_coref(xmldoc,document)
    _annotate_document_from_corenlp_verbs()
    _compute_roles()

    return document

def _compute_roles():
    # TODO reimplement
    logger.warning("Roles not implemented yet")


def _annotate_document_from_corenlp_verbs():
    # TODO reimplement
    logger.warning("Verbs not implemented yet")
    #s.dependencies = [Dependency.from_text(i, s) for i in datamanager.stanford_parser.query({'print':'typedDependenciesCollapsed','sent':sentence}).splitlines() if i.strip()]

    '''#s.dependencies = []
        # <collapsed-ccprocessed-dependencies
        elements = sentence_xml.getElementsByTagName('collapsed-ccprocessed-dependencies')
        if elements:
            s.dependencies += [Dependency.from_xml(i, s) for i in elements[0].getElementsByTagName('dep')]
        # <dependencies type="collapsed-ccprocessed-dependencies"
        elements = sentence_xml.getElementsByTagName('dependencies')
        for element in elements:
            if element.getAttribute('type')=="collapsed-ccprocessed-dependencies":
                s.dependencies += [Dependency.from_xml(i, s) for i in element.getElementsByTagName('dep')]
        '''



def _annotate_document_from_corenlp_mentions(xmldoc,document):
    for sentence_xml in xmldoc.getElementsByTagName('sentences')[0].getElementsByTagName('sentence'):
        id_ = int(sentence_xml.getAttribute('id'))
        sentence = document.get_sentence_by_id(id_)
        sentence.parse_string = sentence_xml.getElementsByTagName('parse')[0].firstChild.nodeValue
        parse = ParentedTree.fromstring(sentence.parse_string)
        assert len(parse.leaves())==len(sentence.tokens)
        for i in xrange(len(sentence.tokens)):
            parse[parse.leaf_treeposition(i)]=sentence.tokens[i]
        sentence.parse_tree = parse
        #parse = ImmutableParentedTree.convert(parse)
        parser = parse_tree_mention_helper.StanfordTreeParser()
        sentence.mentions = parser.get_mentions(parse,sentence,document)

def _annotate_document_from_corenlp_coref(xmldoc,document):
    if xmldoc.getElementsByTagName('document')[0].getElementsByTagName('coreference'):
        _init_coreference_from_xml(xmldoc.getElementsByTagName('document')[0].getElementsByTagName('coreference')[0],document)
    # TODO create entities
    '''
    def _init_coref(self):
        for i in self.d.select('rep#edu.mit.discourse.rep.coref')[0].select('desc'):
            # <desc id="1084" len="3145" off="350">A dragon|864,866,871,876,877,880,885,887,888,1142,895,919,921,922,924,926,930,946,969,981,992,994,998,999,1000,1004,1007,1010,1011,1015,1018,1019,1021,1027,1033,1041,1043,1046,1050,1058,1059,1061,1065,1066</desc>
            # <desc id="1085" len="2839" off="373">Kiev|865,929,937,945,1045,1048</desc>
            representation, data = i.text.split('|')
            id = int(i.attrs.get('id'))
            entity = voz.entitymanager.Entity(id,representation)
            entity._compute_caches(self.document)
            mentions = [self._mentions[int(i)] for i in data.split(',')]
            self.document.entities.append(entity)
            self.document.coreference.append(voz.entitymanager.CoreferenceGroup(id,mentions,entity))
'''

def token_from_xml(xml,str_input):
    id_ = int(xml.attributes.get('id').value)
    offset = int(xml.getElementsByTagName('CharacterOffsetBegin')[0].firstChild.nodeValue)
    offset_end = int(xml.getElementsByTagName('CharacterOffsetEnd')[0].firstChild.nodeValue)
    length = offset_end-offset
    pos = xml.getElementsByTagName('POS')[0].firstChild.nodeValue
    lemma = xml.getElementsByTagName('lemma')[0].firstChild.nodeValue
    text = str_input[offset:offset_end]
    token = voz.Token(id_,offset,length,pos,lemma,text) # type: voz.Token
    return token


def _init_coreference_from_xml(xml,document):
    for coreference in xml.getElementsByTagName('coreference'):
        id_ = document.get_new_id(voz.entitymanager.CoreferenceGroup)
        entity = None
        mentions = []
        for mention_xml in coreference.getElementsByTagName('mention'):
            representative,mention = _coref_mention_from_xml(document,mention_xml)
            mentions.append(mention)
            sentence = mention.tokens[0]._parent_sentence
            sentence.mentions.append(mention)
            if representative:
                entity = mention.to_new_entity(id_)
                document.coreference.entities.append(entity)
        if not entity and mentions:
            entity = mentions[0].to_new_entity(id_)

        for mention in mentions:
            # TODO clean this shit up, move elsewhere
            if mention.id==-1: continue
            sentence = mention.tokens[0]._parent_sentence
            mention_to_parse_node = dict([(v,k) for k,v in sentence.parse_highlight.get('mentions',{}).items()])
            if parse_tree_mention_helper.HIGHLIGHT_COREF not in sentence.parse_highlight:
                sentence.parse_highlight[parse_tree_mention_helper.HIGHLIGHT_COREF]={}
            # TODO: broke when set ids for coreference groups
            #sentence.parse_highlight[parse_tree_mention_helper.HIGHLIGHT_COREF][mention_to_parse_node[mention.id]]=entity.id
        document.coreference.add_coreference_group(id_,mentions,entity)

def _coref_mention_from_xml(document,xml):
    sentence_id = int(xml.getElementsByTagName('sentence')[0].firstChild.nodeValue)
    sentence = document.get_sentence_by_id(sentence_id) #type: voz.Sentence
    start = int(xml.getElementsByTagName('start')[0].firstChild.nodeValue)
    end = int(xml.getElementsByTagName('end')[0].firstChild.nodeValue)
    representative= True if 'representative' in xml.attributes.keys() else False
    tokens = sentence.get_tokens_by_id_range(start,end)
    mention,parent_mention = sentence.get_mention_by_tokens(tokens,True)
    if not mention:
        logger.warn("Coreference: Exact mention not found, creating new children")
        parser = parse_tree_mention_helper.StanfordTreeParser()
        mention = voz.entitymanager.Mention(document.get_new_id(voz.entitymanager.Mention)+1000,tokens)
        mention.is_compound = voz.Token.filter(mention.tokens,pos_list=parser.get_tags_phrase())
        mention.is_list = voz.Token.filter(mention.tokens,pos_list=parser.get_tags_list())
        mention.is_independent = not mention.is_compound and not mention.is_list
        mention.parent_mention = parent_mention
        mention._compute_caches(sentence)
        if parent_mention:
            parent_mention.child_mentions.append(mention)
    return representative,mention


def main():
    logging.basicConfig(level=logging.DEBUG)
    settings.SERIALIZE_PRETTY_JSON_BY_DEFAULT = True
    file_path = "/Users/josepvalls/voz2/data/"
    story_file = "TestInput.txt"
    doc = create_document_from_raw_text(open(file_path+story_file).read())
    print doc
    file_name = 'temp_document.json'
    #doc.serialize_to_file(file_name,use_deep_copy=False)
    open('test_output.html','w').write(formatter.html(formatter.VozHTMLFormatter.format(doc,options={'parse_highlight':parse_tree_mention_helper.HIGHLIGHT_MENTIONS_INDEPENDNET})))
    print voz.Document.format_stats(doc.get_stats())


if __name__ == '__main__':
    main()