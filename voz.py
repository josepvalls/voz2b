import jsonpickle

class VozContainer(object):
    def serialize(self):
        return jsonpickle.encode(self)
    def serialize_to_file(self,file_name):
        open(file_name,'w').write(self.serialize())

class VozTextContainer(object):
    def __init__(self,id,offset,length):
        """
        :param id: int
        :param offset: int
        :param length: int
        :return:
        """
        self.id = id
        self.offset = offset
        self.len = length


class Document(VozContainer):
    def __init__(self,text,sentences,properties):
        """
        :param text: str
        :param sentences: list[Sentence]
        :param properties: dict(str,str)
        :return:
        """
        self.text = text
        self.sentences = sentences
        self.properties = properties

    def __str__(self):
        return "Document %s text:\n%s\nSentences:\n%s" % (self.__repr__(),self.text,'\n'.join([str(sent) for sent in self.sentences]))

class Sentence(VozTextContainer):
    def __init__(self,id,offset,length,tokens):
        """
        :param id: int
        :param offset: int
        :param length: int
        :param tokens: list[Token]
        :return: Sentence
        """
        super(Sentence, self).__init__(id,offset,length)
        self.tokens = tokens

    def __str__(self):
        return ' '.join([str(token) for token in self.tokens])
    def __repr__(self):
        return 'Sentence %d/%s' % (self.id,str(self))


class Token(VozTextContainer):
    def __init__(self,id,offset,length,pos,lemma,text):
        """
        :param id: int
        :param offset: int
        :param length: int
        :param pos: str
        :param lemma: str
        :param text: str
        :return: Token
        """
        super(Token, self).__init__(id,offset,length)
        self.pos = pos or '?'
        self.lemma = lemma
        self.text = text

    def __str__(self):
        return '%s/%s' % (self.lemma,self.pos)
    def __repr__(self):
        return 'Token %d/%s/%s' % (self.id,self.lemma,self.pos)



def create_document_from_sty_file(sty_file):
    """
    Creates a Document from a sty file
    :param sty_file: str
    :return: Document
    """
    doc = Document()
    return doc

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
    return jsonpickle.decode(json)

def create_document_from_text(str_input):
    """
    Creates a Document from text
    :param str_input:
    :return: Document
    """
    import re,datetime
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
    doc = create_document_from_text("Hello, this is dog.\nHow are you?")
    print doc
    file_name = 'temp_document.json'
    doc.serialize_to_file(file_name)
    doc2 = create_document_from_jsonpickle_file(file_name)
    print doc2

if __name__ == '__main__':
    main()