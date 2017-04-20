import settings
import voz
import logging
import util
from quotedbase import *
import entitymanager,verbmanager
import networkcachemanager
import stanfordhelper
from bs4 import BeautifulSoup,element
logger = logging.getLogger(__name__)

class DummyDocument(object):
    id = -1

class QsaFile(object):
    def __init__(self, path=''):
        """
        :param path: str
        """


        logger.info('Processing '+path)
        self.d = BeautifulSoup(open(path,'rb').read(), 'xml')
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
        def consume(tokens,text):
            car = []
            cdr = list(tokens)
            to_consume = len(normalize_string_spacing(text))
            consumed = 0
            while consumed < to_consume:
                token = cdr.pop(0)
                consumed += len(normalize_string_spacing(token.text.encode('utf-8')))
                car.append(token)
            return car,cdr
        def child_as_text(child):
            if isinstance(child, element.NavigableString):
                return unicode(child)
                #return str(unicode(child.decode('utf-8')).encode('utf-8'))
                #return str(unicode(child).encode('ascii','ignore'))
            else:
                return unicode(child.getText())
                #return str(unicode(child.getText().decode('utf-8')).encode('utf-8'))
                #return str(unicode(child.getText()).encode('ascii','ignore'))
        for p in self.d.select('DOC')[0].select('PARAGRAPH'):
            #text = p.getText() # doesn't work because of: <PERSON>the bride</PERSON>-people
            text = ' '.join([child_as_text(child) for child in p.children])
            tokens = stanfordhelper.tokenized_string(unicode(text))
            print self.path,p.attrs.get('parnum')
            #if p.attrs.get('parnum')=='22': pass
            for child in p.children:
                car, tokens = consume(tokens, child_as_text(child))
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
                    # Note, wrong annotations:
                    # in austen_emma_1.xml: <PARAGRAPH parnum="13"><QUOTE id="0">"Poor
                    q = Quote(-1,-1,DummyDocument())
                    print child,child_as_text(child)
                    q._text = child_as_text(child).encode('ascii','ignore')
                    q.annotations = voz.SentenceLevelQuotedAnnotations(-1, -1, 'd', child.attrs.get('speaker',None))
                    q.endp = q._text.strip('"')
                    quotes.append(q)
                    output.append(q)
                else:
                    for token in car:
                        if token.pos in stanfordhelper.VERB_POS:
                            verb = verbmanager.Verb(-1,-1,-1,token,None,{})
                            verbs.append(verb)
                            output.append(verb)
                        if token.pos[-1] in ['.', ':']:
                            output.append(Punctuation(token.text[-1]))
        return output, quotes, mentions, verbs

def tokenized_string_from_qsa_file(qsa_file):
    return QsaFile(qsa_file).tokenize()

def main():
    logging.basicConfig(level=logging.DEBUG)
    file_path = settings.QSA_FILE_PATH
    story_file = settings.QSA_FILES[0]
    story_file = 'chekhov_lady.xml'
    #story_file = 'doyle_boscombe.xml'
    t = tokenized_string_from_qsa_file(file_path+story_file) #type: voz.Document
    output, quotes, mentions, verbs = t
    print tokenized_string_to_string(output,2)

if __name__ == '__main__':
    main()