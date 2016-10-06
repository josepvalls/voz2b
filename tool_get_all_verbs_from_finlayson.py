import settings
import vozbase
import voz
import styhelper
import logging
import csv
import re
import quotedspeechhelper
import os
import networkcachemanager
import util
import collections
from nltk.corpus import wordnet as wn
import pprint
import sys

logger = logging.getLogger(__name__)

DO_REMOVE_DIALOG = False

def get_verbs():
    logging.root.setLevel(logging.ERROR)
    file_path = settings.STY_FILE_PATH
    verbs = []
    frames = []
    functions = collections.defaultdict(list)

    import verbmanager
    mapper = verbmanager.VerbMapper(verbmanager.VerbMapper.MODE_FRAMENET_TEXT)

    for sty_file in settings.STY_FILES:
        try:
            0/0
            doc = voz.create_document_from_jsonpickle_file('/Users/josepvalls/temp/voz2/'+sty_file+'.json')
            logger.info("Loading JSON %s" % sty_file)
        except:
            logger.info("Processing %s" % sty_file)
            quoted_speech_file = sty_file.split()[0]+"/sentences.csv"
            doc = styhelper.create_document_from_sty_file(file_path+sty_file)
            assert isinstance(doc,voz.Document)
            if DO_REMOVE_DIALOG:
                quotedspeechhelper.annotate_quoted_speech(doc,file_path+quoted_speech_file)
                quotedspeechhelper.clean_quoted_speech_from_document(doc)
            doc.serialize_to_file('/Users/josepvalls/temp/voz2/'+sty_file+'.json',use_deep_copy=True)
        #print len(doc.get_all_tokens())
        logger.info(util.string_as_print(doc.id,doc.properties.get('afanasev_new',doc.id),doc.properties.get('afanasev_old',doc.id), doc.narrative.format_summary()))
        assert isinstance(doc,voz.Document)
        doc.narrative.compute_features()
        print sum([f.tokens_count for f in doc.narrative.functions(filter_non_actual=False)])
        continue

        for f in doc.narrative.functions:
            assert isinstance(f,voz.narrativehelper.NarrativeFunction)

            #functions[f.function_group].extend([i.token.lemma for i in f._verbs])
            functions[f.function_group].extend([mapper.map(i.token.lemma,fallback=False) for i in doc.get_all_verbs()])
        verbs.extend([i.token.text for i in doc.get_all_verbs()])
        #frames.update([i.frame for i in doc.get_all_verbs()])
        #frames.extend(filter(None,[mapper.map(i.token.lemma,fallback=False) for i in doc.get_all_verbs()]))
        frames.extend([mapper.map(i.token.lemma,fallback=False) for i in doc.get_all_verbs()])



        #break
    sys.exit()
    roots = util.flatten(util.flatten([[i.root_hypernyms() for i in wn.synsets(verb, 'v')] for verb in verbs]))
    print len(verbs),len(set(verbs))
    print len(frames),len(set(frames))
    print len(roots)
    print collections.Counter(roots).most_common()
    print collections.Counter(frames).most_common()
    print collections.Counter(verbs).most_common()
    pprint.pprint(functions)
    vozbase.serialize_to_file([verbs,frames,functions],'/Users/josepvalls/temp/voz2/verbs.json',False,False)
    mapper.save_cache()


def main():
    get_verbs()
    #do_tfidf()


if __name__=="__main__":
    main()