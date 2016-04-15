def get_docs_stats():
    tsv = None
    arff = None
    logging.root.setLevel(logging.ERROR)
    file_path = settings.STY_FILE_PATH
    for sty_file in settings.STY_FILES:
    #for sty_file in ['03 - Bukhtan Bukhtanovich.sty']:
        try:
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
        logger.info(util.string_as_print(doc.id,doc.properties.get('afanasev_new',doc.id),doc.properties.get('afanasev_old',doc.id), doc.narrative.format_summary()))
        print doc.narrative.format()
        doc.narrative.compute_features()
        if not tsv:
            tsv = doc.narrative.format_tsv()
            arff = doc.narrative.format_arff()
        else:
            tsv += doc.narrative.format_tsv(False)
            arff += doc.narrative.format_arff(False)
        if True:
            print doc.id
            for function in doc.narrative.functions:
                print function.get_feature_vector()
    if DO_WRITE_FILES: