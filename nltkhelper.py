import time
import settings
import re
import codecs
import json
import logging
import collections, itertools
import util
import operator

logger = logging.getLogger(__name__)

class Acquirer(object):
    pass

class ANLTKCorpusNames(Acquirer):
    def query(self, q):
        return nltk_names_db.words(q)

try:
    from nltk.corpus import names as nltk_names_db

    nltk_names = ANLTKCorpusNames()
except:
    nltk_names = None
    logger.warning("CANNOT LOAD nltk_names")

class ANLTKCorpusVerbnet(Acquirer):
    def query(self, q=None, q_wordnet=None):
        if not q and not q_wordnet:
            return []
        if q:
            return vn.classids(lemma=q)  # wordnetid, fileid, classid
        if q_wordnet:
            return vn.classids(wordnetid=q_wordnet)  # wordnetid, fileid, classid

try:
    from nltk.corpus import verbnet as vn

    nltk_verbnet = ANLTKCorpusVerbnet()
except:
    nltk_verbnet = None
    logger.warning("CANNOT LOAD nltk_verbnet")


def wordnet_pos(pos):
    if not pos:
        return wn.NOUN
    pos = pos.lower()
    if pos.startswith('v'):
        pos = wn.VERB
    elif pos.startswith('n'):
        pos = wn.NOUN
    elif pos.startswith('j'):
        pos = wn.ADJ
    elif pos.startswith('a'):
        pos = wn.ADV
    else:
        pos = wn.NOUN
    # TODO wn.ADJ_SAT?
    return pos


class ANLTKCorpusWordnet(Acquirer):
    def query(self, q, pos='VB'):
        return wn.synsets(q, wordnet_pos(pos))

    def query_expand(self, token):
        synsets = self.query(token.lemma, token.POS)
        return util.flatten([
                                [('isa', i) for i in synset.hypernyms()] +
                                [('has', i) for i in synset.member_holonyms()] +
                                [('partof', i) for i in synset.member_meronyms()] +
                                [('related', i) for i in synset.entailments()] +
                                [('desc', synset.definition)]
                                for synset in synsets])

    def query_similarity(self, t1, t2, aggregate=min):
        s1 = wn.synsets(t1.lemma, pos=wordnet_pos(t1.POS))
        s2 = wn.synsets(t2.lemma, pos=wordnet_pos(t2.POS))
        return aggregate([wn.wup_similarity(*i) for i in itertools.product(s1, s2)])

    def query_common_ancestors(self, a, b, pos='VB', lowest_ancestor=True, all_children=False, as_lemmas=True):
        synsets_a = wn.synsets(a, wordnet_pos(pos))
        synsets_b = wn.synsets(b, wordnet_pos(pos))
        if synsets_a and synsets_b:
            x = []
            for i in synsets_a:
                for j in synsets_b:
                    if lowest_ancestor:
                        x += i.lowest_common_hypernyms(j)
                    else:
                        x += i.common_hypernyms(j)
            x = set(x)
            if all_children:
                y = set(x)
                while y:
                    for i in y.pop().hyponyms():
                        if i not in x:
                            x.add(i)
                            y.add(i)

            if as_lemmas:
                return list(set(reduce(operator.add, [[i.name for i in j.lemmas] for j in x])))
            else:
                return list(set(x))
        else:
            return []

    def query_related_words(self, q, pos='VB', add_vertical_related=0):
        synsets = wn.synsets(q, wordnet_pos(pos))
        if add_vertical_related & 1:
            synsets += reduce(operator.add, [i.hyponyms() for i in synsets])
        if add_vertical_related & 2:
            synsets += reduce(operator.add, [i.hypernyms() for i in synsets])
        return set(reduce(operator.add, [[i.name for i in j.lemmas] for j in synsets]))


try:
    from nltk.corpus import wordnet as wn

    nltk_wordnet = ANLTKCorpusWordnet()
except:
    nltk_wordnet = None
    logger.warning("CANNOT LOAD nltk_wordnet")


class ASentiWordnet(Acquirer):
    def __init__(self, filename):
        self.filename = filename
        self.db = {}
        self.parse_src_file()

    def parse_src_file(self):
        lines = codecs.open(self.filename, "r", "utf8").read().splitlines()
        lines = filter((lambda x: not re.search(r"^\s*#", x)), lines)
        for line in lines:
            fields = re.split(r"\t+", line)
            fields = map(unicode.strip, fields)
            pos, offset, pos_score, neg_score, synset_terms, gloss = fields
            if pos and offset:
                offset = int(offset)
                self.db[(pos, offset)] = (float(pos_score), float(neg_score))

    def query(self, q=None, pos='VB', synset=None):
        pos = wordnet_pos(pos)
        if not q and not synset: return 0.0
        if not synset:
            synsets = wn.synsets(q, pos)
        else:
            synsets = [synset]
        synsets = [self.db.get((pos, i.offset), (0.0, 0.0)) for i in synsets]
        return sum([i[0] for i in synsets]) - sum([i[1] for i in synsets])


sentiwordnet = None
if settings.SENTIWORDNET_ENABLED:
    try:
        sentiwordnet = ASentiWordnet(settings.SENTIWORDNET_FILE)
    except:
        logger.warning("CANNOT LOAD sentiwordnet")
else:
    logger.info("DISABLED sentiwordnet")

class APhrasalVerbs(Acquirer):
    # From: http://www.englishclub.com/vocabulary/phrasal-verbs-list.htm
    def __init__(self, filename):
        lines = codecs.open(filename, "r", "utf8").read().splitlines()
        self.data = collections.defaultdict(lambda: [])

        def from_line(line):
            return '_'.join(
                [i for i in line.strip().split('\t')[0].split() if '/' not in i and i not in ['someone', 'something']])

        for line in lines:
            self.data[from_line(line)] += [line.split('\t')[1]]

    def query(self, q):
        return self.data.get(q, [])


phrasal_verbs = None
if settings.PHRASAL_VERB_FILE:
    try:
        phrasal_verbs = APhrasalVerbs(settings.PHRASAL_VERB_FILE)
    except:
        logger.warning("CANNOT LOAD phrasal_verbs")
else:
    logger.info("DISABLED phrasal_verbs")




# Other tools

class WordNetLemmatizer(Acquirer):
    def __init__(self):
        self.wnl = NLTKWordNetLemmatizer()

    def query(self, q, pos='N'):
        return self.wnl.lemmatize(q, wordnet_pos(pos))


try:
    from nltk.stem import WordNetLemmatizer as NLTKWordNetLemmatizer
    lemmatizer = WordNetLemmatizer()
except:
    logger.warning("CANNOT LOAD lemmatizer")
    lemmatizer = None


class NLTKSentenceTokenizer(Acquirer):
    def query(self, q):
        return sent_tokenize(q)

try:
    from nltk.tokenize import sent_tokenize
    sentence_tokenizer = NLTKSentenceTokenizer()
except:
    sentence_tokenizer = None
    logger.warning("CANNOT LOAD sentence_tokenizer")

try:
    from pattern import en as pattern_en
except:
    pattern_en = None
    logger.warning("CANNOT LOAD pattern_en")

'''try:
    import senticlassifier
    swn = senticlassifier.SentiWordNetCorpusReader('resources/SentiWordNet_3.0.0_20130122.txt')
except:
    swn = None

class SentiWordnetWrapper(ExternalTool):
    def query(self,q,pos='v'):
        return reduce(util.sum_lists,[[[i.pos_score],[i.neg_score],[i.obj_score]] for i in swn.senti_synsets(q, pos)],[[],[],[]])
    def query_binary(self,q, pos='v'):
        v = self.query(q, pos)
        return util.average(v[0])-util.average(v[1])
sentiwn = SentiWordnetWrapper()'''
if __name__ == '__main__':
    print "NLTK helper"