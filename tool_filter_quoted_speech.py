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
    #generate_filtered_text_files()
    #cache_results_from_corenlp()
    generate_filtered_entity_file()


def generate_filtered_text_files():
    """
    Generate files to be processed by parsers.
    ClearNLP:
     source /Users/josepvalls/soft/clearnlp/setup_classpath.sh
     java -Xmx5g -XX:+UseConcMarkSweepGC edu.emory.clir.clearnlp.bin.NLPDecode -mode ner -c config_decode_ner.xml -i /Users/josepvalls/voz2/stories/dialog_filtered -ie txt
    ClearNLP Coref
     java /Users/josepvalls/Dropbox/projects/clearnlp/src/main/java/edu/drexel/valls
    Stanford CoreNLP
     use CoreNLP server cache
    Open NLP
     sh /Users/josepvalls/Dropbox/projects/coref-opennlp/Coref/run.cmd
    """
    logging.basicConfig(level=logging.DEBUG)
    file_path = settings.STY_FILE_PATH
    for sty_file in settings.STY_FILES:
        logger.info("Processing %s" % sty_file)
        quoted_speech_file = sty_file.split()[0]+"/sentences.csv"
        doc = styhelper.create_document_from_sty_file(file_path+sty_file)
        quotedspeechhelper.annotate_quoted_speech(doc,file_path+quoted_speech_file)
        sentences = []
        for sentence in doc.sentences:
            if not sentence.quoted_speech:
                sentences.append(sentence.get_text()+'\n')
        file_name = settings.STORY_TXT_PATH+str(doc.id)+'.txt'
        logger.info("Writing %d sentences to %s" % (len(sentences),file_name))
        with open(file_name,'w') as f:
            f.writelines(sentences)

def cache_results_from_corenlp():
    for name in os.listdir(settings.STORY_TXT_PATH):
        file_name = settings.STORY_TXT_PATH+name
        if file_name.endswith('.txt'):
            logger.info("Caching %s" % file_name)
            networkcachemanager.stanford_nlp.query(open(file_name).read())


def generate_filtered_entity_file():
    logging.basicConfig(level=logging.DEBUG)
    file_path = settings.STY_FILE_PATH
    mentions = []
    for sty_file in settings.STY_FILES[2:3]:
        logger.info("Processing %s" % sty_file)
        quoted_speech_file = sty_file.split()[0]+"/sentences.csv"
        doc = styhelper.create_document_from_sty_file(file_path+sty_file)
        quotedspeechhelper.annotate_quoted_speech(doc,file_path+quoted_speech_file)
        for sentence in doc.sentences:
            assert(isinstance (sentence,voz.Sentence))
            if not sentence.quoted_speech:
                for mention in sentence.mentions:
                    mentions.append(mention.get_text().lower()+'\n')
    file_name = '/Users/josepvalls/voz2/stories/finlayson-entities.txt'
    logger.info("Writing %d mentions to %s" % (len(mentions),file_name))
    with open(file_name,'w') as f:
        f.writelines(mentions)


if __name__=='__main__':
    main()