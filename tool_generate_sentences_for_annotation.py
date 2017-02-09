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
import entitymanager

logger = logging.getLogger(__name__)

def main():

    get_docs_stats()


def get_docs_stats():
    logging.root.setLevel(logging.INFO)

    acumm_stats_before = [0 for _ in voz.Document.get_stats_labels()]
    acumm_stats_after = [0 for _ in voz.Document.get_stats_labels()]
    acumm_count = 0
    logging.basicConfig(level=logging.WARNING)
    file_path = settings.STY_FILE_PATH
    if not os.path.isfile('all_coreferenced-entities2.tsv.csv'):
        entities = open('all_coreferenced-entities2.tsv.csv','w')
        for sty_file in settings.STY_FILES:
            logger.info("PROCESSING "+sty_file)
            acumm_count +=1
            logger.info("Processing %s" % sty_file)
            doc = styhelper.create_document_from_sty_file(file_path+sty_file)
            try:
                os.mkdir(file_path+str(doc.id))
            except:
                pass
            with open(file_path + str(doc.id) + '/story.txt', 'w') as sentences:
                o1 = doc.sentences[0].offset
                o2 = doc.sentences[-1].offset+doc.sentences[-1].len
                sentences.write(doc.get_text()[o1:o2])
            sentences_file = file_path+str(doc.id)+'/sentences.csv'
            if not os.path.isfile(sentences_file):
                with open(sentences_file, 'w') as sentences:
                    for sentence in doc.sentences:
                        #sentence = voz.Sentence()
                        sentences.write("%s\t%s\t\t\t\"%s\"\n" % (str(sentence.id),str(sentence.idx),sentence.get_text().replace('"','""')))
            for group in doc.coreference.coreference_groups:
                #group = entitymanager.CoreferenceGroup
                entities.write("%d\t%d\t%s\t%s\t\t\t\t\t\n" % (
                    doc.id,
                    group.id,
                    group.get_representation(),
                    ';'.join([str(i) for i in group.mentions])
                ))


        entities.close()

if __name__=='__main__':
    main()