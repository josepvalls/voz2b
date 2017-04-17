import settings
import voz
import logging
import util
from quotedbase import *
import entitymanager,verbmanager

logger = logging.getLogger(__name__)

class QsaFile(object):
    def __init__(self, path=''):
        """
        :param path: str
        """
        from bs4 import BeautifulSoup

        logger.info('Processing '+path)
        self.d = BeautifulSoup(open(path).read(), 'xml')
        self.path = path
        self.story_id = -1
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
        self.document.id = int(self.story_id) if util.is_numeric_int(self.story_id) else properties.get('story_id',-1)
    def tokenize(self):
        quotes = [] #type: list[Quote]
        mentions = [] #type: list[entitymanager.Mention]
        verbs = [] #type: list[verbmanager.Verb]
        output = []

        mention_id_to_mention = {}

        # TODO create a sentence per paragraph?
        for p in self.d.select('DOC')[0].select('PARAGRAPH'):
            # TODO parse/tag the text: p.getText()
            for child in p.children:
                if child.name in ['PERSON','ORGANIZATION']:
                    m = entitymanager.Mention(-1, [], is_independent=True)
                    m.annotations.character = True
                    m.annotations.coref = child.attrs.get('entity', None)
                    m.add_tag(entitymanager.TaggableContainer.TAG_CHARACTER_SYMBOL,m.annotations.coref)
                    id_ = child.attrs.get('id', None)
                    mention_id_to_mention[id_]=m
                    if not m.get_most_likely_symbol():
                        print m.annotations.coref
                    m.type = child.attrs.get('gender', None)
                    m.role = None
                    m.split_ignore = False
                    mentions.append(m)
                    output.append(m)
                elif child.name =='QUOTE':
                    q = Quote(-1,-1,None)
                    q._text = child.getText()
                    quotes.append(q)
                    output.append(q)
                else:
                    pass
                # TODO verbs and punctuation








        for e in self.d.select('rep#edu.mit.semantics.semroles')[0].select('desc'):
            #<desc id="2446" len="27" off="350">2 user appear.01 ----a 0:1-ARG1- 3:1-ARGM-LOC</desc>
            data = e.text.split()
            off = int(e.attrs.get('off', 0))
            len = int(e.attrs.get('len', 0))


        pass
    def tokenize_document(doc, verbose=False):
        assert isinstance(doc,voz.Document)
        quotes = [] #type: list[Quote]
        mentions = [] #type: list[entitymanager.Mention]
        verbs = [] #type: list[verbmanager.Verb]
        # parse
        in_quotes_offset = None
        output = []
        input = doc.get_all_tokens(pair_container=True)
        ignore_tokens = set()
        while input:
            sentence, token = input.pop(0)
            assert isinstance(sentence,voz.Sentence)
            assert isinstance(token,voz.Token)

            if in_quotes_offset is None and token.text[-1] in ['.',':']:
                output.append(Punctuation(token.text[-1]))
            elif in_quotes_offset is None and token.text!='``':
                # decide what to do with tokens outside of quotes
                if not token in ignore_tokens:
                    mentions_ = filter(None,doc.get_mentions_by_token_id(token.id))
                    mentions_ = util.flatten([i.get_all_children_recursively() for i in mentions_])
                    for mention in set(mentions_):
                        if mention and mention.is_independent:
                            if mention.get_most_likely_symbol():
                                output.append(mention)
                                mentions.append(mention)
                            ignore_tokens.update(mention.tokens)
                    verb = doc.get_verb_by_token_id(token.id)
                    if verb:
                        # TODO whitelist only expression verbs?
                        verbs.append(verb)
                        output.append(verb)
            elif in_quotes_offset is None and token.text=='``':
                in_quotes_offset = token.offset
            elif in_quotes_offset is not None and token.text=="''":
                quote = Quote(in_quotes_offset,token.offset+token.len, doc)
                quote.endp = token.get_previous().text
                quotes.append(quote)
                output.append(quote)
                if sentence.annotations and sentence.annotations.speech_data:
                    quote.annotations = sentence.annotations.get_speech_annotations_for_quote(quote)
                else:
                    logger.warning("NO ANNOTATION DATA FOR SENTENCE")
                in_quotes_offset = None

        return output,quotes,mentions,verbs

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

    def get_original_text(self):
        """
        Gets the full original text of the story.
        :return: str
        """
        return self.d.select('rep#edu.mit.story.char')[0].text.strip()

def create_document_from_qsa_file(qsa_file,properties={}):
    """
    Creates a Document from an XML file from the Columbia_QSA_Corpus_1.01
    :param qsa_file: str
    :return: voz.Document
    """

    doc = QsaFile(qsa_file).to_document(properties)
    return doc

def tokenized_string_from_qsa_file(qsa_file):
    return QsaFile(qsa_file).tokenize()





def main():
    logging.basicConfig(level=logging.DEBUG)
    file_path = settings.QSA_FILE_PATH
    story_file = settings.QSA_FILES[0]
    t = tokenized_string_from_qsa_file(file_path+story_file) #type: voz.Document
    output, quotes, mentions, verbs = t
    print output

if __name__ == '__main__':
    main()