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

    get_docs_stats()


def get_docs_stats():
    logging.root.setLevel(logging.ERROR)

    acumm_stats_before = [0 for _ in voz.Document.get_stats_labels()]
    acumm_stats_after = [0 for _ in voz.Document.get_stats_labels()]
    acumm_count = 0
    logging.basicConfig(level=logging.WARNING)
    file_path = settings.STY_FILE_PATH
    for sty_file in settings.STY_FILES:
        acumm_count +=1
        logger.info("Processing %s" % sty_file)
        quoted_speech_file = sty_file.split()[0]+"/sentences.csv"
        doc = styhelper.create_document_from_sty_file(file_path+sty_file)
        doc_stats = doc.get_stats()
        for i in xrange(len(acumm_stats_before)):
            acumm_stats_before[i]+=doc_stats[i]
        quotedspeechhelper.annotate_quoted_speech(doc,file_path+quoted_speech_file)
        quotedspeechhelper.clean_quoted_speech_from_document(doc)
        doc_stats = doc.get_stats()


        for i in xrange(len(acumm_stats_before)):
            acumm_stats_after[i]+=doc_stats[i]

    print "Counts"
    #print voz.Document.format_stats(acumm_stats_before)
    print voz.Document.format_stats(acumm_stats_after)
    print "Averages"
    for i in xrange(len(acumm_stats_before)):
        #acumm_stats_before[i]=1.0*acumm_stats_before[i]/acumm_count
        acumm_stats_after[i]=1.0*acumm_stats_after[i]/acumm_count
    #print voz.Document.format_stats(acumm_stats_before)
    print voz.Document.format_stats(acumm_stats_after)




if __name__=='__main__':
    main()