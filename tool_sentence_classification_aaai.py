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

logger = logging.getLogger(__name__)

def main():
    load_sentence_annotations()

def load_sentence_annotations():
    logging.basicConfig(level=logging.DEBUG)
    file_path = settings.STY_FILE_PATH
    mentions = []
    for sty_file in settings.STY_FILES[2:3]:
        logger.info("Processing %s" % sty_file)
        quoted_speech_file = "all_sentences.tsv"
        doc = styhelper.create_document_from_sty_file(file_path+sty_file)
        quotedspeechhelper.annotate_sentences(doc, file_path + quoted_speech_file, format='tsv',single_sentences_file_story_id=doc.id)
        for sentence in doc.sentences:
            assert(isinstance (sentence,voz.Sentence))
            if sentence.annotations.is_normal():
                print sentence.get_text()



if __name__=='__main__':
    main()