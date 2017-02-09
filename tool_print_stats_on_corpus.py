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
import collections

logger = logging.getLogger(__name__)

def main():
    get_docs_stats()
    #check_tsv_csv_annotation_files()


def check_tsv_csv_annotation_files():
    logging.basicConfig(level=logging.ERROR)
    file_path = settings.STY_FILE_PATH
    corpus_annotation_stats = collections.defaultdict(lambda:0)

    entities = [i.split('\t') for i in open(file_path+'/all_coreferenced-entities.tsv.csv').readlines()]
    entities_wrong = [i for i in entities if len(i)<6]
    if entities_wrong: corpus_annotation_stats['E_entities_short'] = len(entities_wrong)
    corpus_annotation_stats['coref groups'] = len(entities)
    entities_dict = dict([((i[0],i[3]),i) for i in entities if i[3]])
    corpus_annotation_stats['entities'] = len(entities_dict)
    print entities_dict
    #return
    for sty_file in settings.STY_FILES:
        sty_file_path = sty_file.split()[0]
        sty_file_id = sty_file.split()[0].lstrip('0')
        corpus_annotation_stats['stories']+=1
        logger.info("Processing %s" % sty_file)
        quoted_speech_file = file_path + sty_file_path + "/sentences.tsv.csv"
        sentences = [i.split('\t') for i in open(quoted_speech_file).readlines()]
        for sentence_i in xrange(len(sentences)):
            sentence = sentences[sentence_i]
            corpus_annotation_stats['sentences'] += 1
            if len(sentence)<7:
                logger.warning("too short/"+sty_file_id+"/"+str(sentence))
                corpus_annotation_stats['E_sentences_short'] += 1
                continue
            if sentence[2] not in 'cdnqsp':
                logger.warning("wrong sentence/" + sty_file_id + "/" + str(sentence))
                continue
            corpus_annotation_stats['sentences_%s' % sentence[2]] += 1
            if sentence[2]=='d':
                if not sentence[3].strip():
                    logger.warning("no entity/" + str(sentence))
                    corpus_annotation_stats['E_no_entity'] += 1
                for speaker in sentence[3].split(','):
                    key = (sty_file_id,speaker.strip())
                    if key not in entities_dict:
                        logger.warning("speaker not found/" + str(key))
                        corpus_annotation_stats['E_bad_speaker'] += 1
                for speaker in sentence[4].split(','):
                    if not speaker: continue
                    key = (sty_file_id, speaker.strip())
                    if key not in entities_dict:
                        logger.warning("listener not found/" + str(key))
                        corpus_annotation_stats['E_bad_listener'] += 1
            if sentence[5]:
                if '<' in sentence[5]:
                    # quote before
                    sentence_ = sentences[sentence_i-1]
                    sentence__ = -1
                # both may be possible now!
                if '>' in sentence[5]:
                    # quote next
                    sentence_ = sentences[sentence_i + 1]
                    sentence__ = 0
                if not sentence_ or sentence_[3]=='d' and not sentence_[6].strip()[sentence__]=='"':
                    logger.warning("bad dialog attachemt/" + str(sentence) + '/'+sentence_)
                    corpus_annotation_stats['E_bad_attachment'] += 1
                for attachment in sentence[5].split(';'):
                    delim = attachment.index('<') if '<' in attachment else attachment.index('>')
                    for participant in attachment[(delim+1):].split('/'):
                        if participant not in sentence[6]:
                            logger.warning("participant not found/" + str(participant)+'/'+str(sentence))
                            corpus_annotation_stats['E_bad_participant'] += 1
            #break
    for k,v in sorted(corpus_annotation_stats.items()):
        print k.ljust(20)+"\t"+str(v).rjust(5)


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
        doc = styhelper.create_document_from_sty_file(file_path+sty_file)
        doc_stats = doc.get_stats()
        for i in xrange(len(acumm_stats_before)):
            acumm_stats_before[i]+=doc_stats[i]
        quoted_speech_file = sty_file.split()[0] + "/sentences.tsv.csv"
        quotedspeechhelper.annotate_sentences(doc, file_path + quoted_speech_file)
        quotedspeechhelper.clean_quoted_speech_from_document(doc)
        doc_stats = doc.get_stats()
        for i in xrange(len(acumm_stats_before)):
            acumm_stats_after[i]+=doc_stats[i]
        #break

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