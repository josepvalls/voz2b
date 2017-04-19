import settings
import vozbase
import voz
import styhelper
import logging
import csv
import re
from quotedbase import normalize_string
logger = logging.getLogger(__name__)

SPEECH_TYPE_CONSEC = 'c'
SPEECH_TYPE_DIALOG = 'd'
SPEECH_TYPE_NARRAT = 'n'
SPEECH_TYPE_QUOTED = 'q'
SPEECH_TYPE_NOQOUO = 'p'


def annotate_sentences(document, quoted_speech_file, format='tsv', single_sentences_file_story_id = None):
    """
    :param document: voz.Document
    :param quoted_speech_file: str
    :return:
    """
    logger.debug("annotate_quoted_speech %s to %s" % (quoted_speech_file,document.get_long_id()))

    for sentence in document.sentences:
        pass

    sty_cur= 0
    text_count_sty = 0
    text_count_csv = 0

    all_rows = []
    if format == 'csv':
        offset = 0
        COL_FID = 0
        COL_IDX = 1
        COL_TYP = 2
        COL_SPK = 3
        COL_TXT = 4
        with open(quoted_speech_file, 'rbU') as csv_file:
            csv_reader = csv.reader(csv_file)
            for row in csv_reader:
                all_rows.append(list(row))
    elif format == 'tsv':
        if single_sentences_file_story_id:
            offset = 1
            found = False
            for i in open(quoted_speech_file, 'r').readlines():
                row = i.split('\t')
                if row[0] and found:
                    break
                elif row[0] and not found and int(row[0]) == single_sentences_file_story_id:
                    found = True
                if found:
                    all_rows.append(row)

        else:
            offset = 0
            all_rows = [i[0:-1].split('\t') for i in open(quoted_speech_file,'r').readlines()]

        COL_FID = 0 + offset
        COL_IDX = 1 + offset
        COL_TYP = 2 + offset
        COL_SPK = 3 + offset
        COL_LST = 4 + offset
        COL_HNT = 5 + offset
        COL_TXT = 6 + offset
        COL_SCE_LOC = 7 + offset
        COL_SCE_TME = 8 + offset
        COL_SCE = 9 + offset

    row_annotation = voz.SentenceLevelAnnotations()
    _aligned_sentences_annotations = []
    row_offset = 0
    newline_offset = 0
    for row_i,row in enumerate(all_rows):
        # read lines from the annotation file
        logger.info('< '+str(len(_aligned_sentences_annotations)-1)+'/'+str(row_i)+' '+row[COL_TXT])
        row_annotation.speech.append(row[COL_TYP])
        if offset:
            row_annotation.speech_data.append(
                voz.SentenceLevelQuotedAnnotations(
                    row_offset, len(row[COL_TXT]), row[COL_TYP], row[COL_SPK],row[COL_LST], row[COL_HNT],text=row[COL_TXT]))
            row_annotation.scene_loc.append(row[COL_SCE_LOC])
            row_annotation.scene_time.append(row[COL_SCE_TME])
            row_annotation.scene.append(int(row[COL_SCE].strip()))
        else:
            row_annotation.speech_data.append(voz.SentenceLevelQuotedAnnotations(row_offset, len(row[COL_TXT]), row[COL_TYP]),None, None, None)
        # line offsets
        row_offset += len(row[COL_TXT]) + newline_offset
        # global, normalized offsets
        text_count_csv += len(normalize_string(row[COL_TXT]))
        while text_count_sty <= text_count_csv and sty_cur < len(document.sentences):
            # ingest any necessary sentences from the ground sty text and apply annotations read so far to the sentence
            logger.info('> '+str(sty_cur)+' '+str(document.sentences[sty_cur]))
            if _aligned_sentences_annotations:
                row_annotation = voz.SentenceLevelAnnotations()
            _aligned_sentences_annotations.append(row_annotation)
            text_count_sty += len(normalize_string(document.sentences[sty_cur].get_text()))
            sty_cur += 1
            row_offset = 0
    if not len(document.sentences)==len(_aligned_sentences_annotations):
        logger.warning("MISMATCH BETWEEN ANNOTATED SENTENCES LENGTH")
    for sentence,annotations in zip(document.sentences,_aligned_sentences_annotations):
        sentence.annotations = annotations
        sentence.annotations.init(sentence)

def clean_quoted_speech_from_document(document):
    """
    :param document: voz.Document
    :return: None
    """
    assert isinstance(document,voz.Document)
    occurences = 0
    for sentence in list(document.sentences):
        assert isinstance(sentence,voz.Sentence)
        if not sentence.annotations.is_normal():
            occurences += document.remove_sentence(sentence)
    logger.info("Removed %d mentions in quoted speech" % occurences)



def main_single():
    # not used anymore, all the annotations are all in a single file
    logging.basicConfig(level=logging.DEBUG)
    file_path = "/Users/josepvalls/voz2/stories/annotation-finlayson-01/"
    story_file = "01 - Nikita the Tanner.sty"
    quoted_speech_file = "01/sentences.csv"
    doc = styhelper.create_document_from_sty_file(file_path+story_file)
    annotate_sentences(doc, file_path + quoted_speech_file)
    for sentence in doc.sentences:
        print sentence.format_quoted_annotations()
    print voz.Document.format_stats(doc.get_stats())
    clean_quoted_speech_from_document(doc)
    print voz.Document.format_stats(doc.get_stats())

    pass


def main_all():
    logging.basicConfig(level=logging.DEBUG)
    file_path = "/Users/josepvalls/voz2/stories/annotation-finlayson-01/"
    story_file = "01 - Nikita the Tanner.sty"
    doc = styhelper.create_document_from_sty_file(file_path+story_file)
    styhelper.fix_sty_annotations(doc)

    annotate_sentences(doc, settings.STORY_ALL_SENTENCES, single_sentences_file_story_id = doc.id)
    for sentence in doc.sentences:
        print sentence.format_quoted_annotations()

    print voz.Document.format_stats(doc.get_stats())
    clean_quoted_speech_from_document(doc)
    print voz.Document.format_stats(doc.get_stats())

    pass

def main_signal_verbs():
    offset = 1
    rows = [i[0:-1].split('\t') for i in open(settings.STORY_ALL_SENTENCES,'r').readlines()]
    COL_FID = 0 + offset
    COL_IDX = 1 + offset
    COL_TYP = 2 + offset
    COL_SPK = 3 + offset
    COL_LST = 4 + offset
    COL_HNT = 5 + offset
    COL_TXT = 6 + offset
    COL_SCE_LOC = 7 + offset
    COL_SCE_TME = 8 + offset
    COL_SCE = 9 + offset
    verbs = filter(None,[i[COL_HNT].replace('<','>').split('>')[0] for i in rows])
    verbs = [i for i in verbs]
    import nltkhelper
    import collections
    print collections.Counter(verbs).most_common()

    for verb in sorted(set(verbs)):
        print verb
        print ' ',[j.lemmas() for j in nltkhelper.nltk_wordnet.query(verb)]


if __name__=='__main__':
    main_signal_verbs()
    #main_all()