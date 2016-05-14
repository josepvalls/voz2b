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
import oldannotationhelper

logger = logging.getLogger(__name__)


DO_REMOVE_DIALOG = False
DO_WRITE_FILES = False
DO_PRINT_TO_SCREEN = False
DO_FILTER_NONACTUAL = True

DO_AUTO_COREF_ROLES = True
DO_AUTO_VERBS = True
DO_USE_OLD_AUTO_DATA_INSTEAD_OF_STY_GT = False

def main():
    get_docs_stats(5,True)
    return
    for i in range(5):
        for j in [True,False]:
            get_docs_stats(i+1,j)


def get_docs_stats(feature_group,feature_distribution):
    tsv = None
    arff = None
    idxlst = ''
    logging.root.setLevel(logging.ERROR)
    file_path = settings.STY_FILE_PATH
    documents = []
    #for sty_file in []:
    for sty_file in settings.STY_FILES:
    #for sty_file in ['03 - Bukhtan Bukhtanovich.sty']:
        try:
            0/0
            doc = voz.create_document_from_jsonpickle_file('/Users/josepvalls/temp/voz2/'+sty_file+'.json')
            logger.info("Loading JSON %s" % sty_file)
        except:
            logger.info("Processing %s" % sty_file)
            quoted_speech_file = sty_file.split()[0]+"/sentences.csv"
            doc = styhelper.create_document_from_sty_file(file_path+sty_file)
            if DO_REMOVE_DIALOG:
                quotedspeechhelper.annotate_quoted_speech(doc,file_path+quoted_speech_file)
                quotedspeechhelper.clean_quoted_speech_from_document(doc)
            doc.serialize_to_file('/Users/josepvalls/temp/voz2/'+sty_file+'.json',use_deep_copy=True)
        # print util.string_as_print(doc.id,doc.properties.get('afanasev_new',doc.id),doc.properties.get('afanasev_old',doc.id), doc.narrative.format_summary())
        documents.append(doc)
    if not DO_USE_OLD_AUTO_DATA_INSTEAD_OF_STY_GT:
        for document_id in [1001,1002,1003,1004,2001]:
        #for document_id in [1004]:
            documents.append(oldannotationhelper.load_old_annotations_into_document(document_id))
    for doc in documents:
        import narrativehelper
        narrativehelper.VERB_FEATURES = feature_group
        narrativehelper.DO_COMPUTE_ROLE_DISTRIBUTION = feature_distribution
        narrativehelper.DO_USE_OLD_AUTO_DATA_INSTEAD_OF_STY_GT = DO_USE_OLD_AUTO_DATA_INSTEAD_OF_STY_GT
        assert isinstance(doc,voz.Document)
        #print doc.id,"Narrative: ",doc.narrative.format(options={'one_liner':True,'use_function_group':True})
        #continue
        doc.narrative.filter_non_actual_default = DO_FILTER_NONACTUAL
        doc.narrative.compute_features()
        print sum([i.tokens_count for i in doc.narrative.function_list])
        if DO_WRITE_FILES:
            for _ in doc.narrative.functions():
                idxlst += "%d\n" % doc.id
            if not tsv:
                tsv = doc.narrative.format_tsv()
                arff = doc.narrative.format_arff()
                idxlst
            else:
                tsv += doc.narrative.format_tsv(False)
                arff += doc.narrative.format_arff(False)
        if DO_PRINT_TO_SCREEN:
            #print doc.id
            for function in doc.narrative.functions():
                print doc.id,function.get_feature_vector()
    if DO_WRITE_FILES:
        open('tool_corpus_functions_summary/story_indices%s%s.txt' % (('_filtered' if DO_FILTER_NONACTUAL else ''),('_nodiag' if DO_REMOVE_DIALOG else '')),'w').write(idxlst)
        open('tool_corpus_functions_summary/tool_corpus_functions_summary_%d_%s%s%s%s.tsv' % (feature_group,'dist' if feature_distribution else 'abs','_filtered' if DO_FILTER_NONACTUAL else '','_auto' if DO_USE_OLD_AUTO_DATA_INSTEAD_OF_STY_GT else '','_nodiag' if DO_REMOVE_DIALOG else ''), 'w').write(tsv)
        open('tool_corpus_functions_summary/tool_corpus_functions_summary_%d_%s%s%s%s.arff' % (feature_group,'dist' if feature_distribution else 'abs','_filtered' if DO_FILTER_NONACTUAL else '','_auto' if DO_USE_OLD_AUTO_DATA_INSTEAD_OF_STY_GT else '','_nodiag' if DO_REMOVE_DIALOG else ''), 'w').write(arff)


if __name__=='__main__':
    main()