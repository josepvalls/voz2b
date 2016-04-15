import settings
import vozbase
import voz
import styhelper
import logging
import csv
import re

logger = logging.getLogger(__name__)

def normalize_string(s):
    return re.sub('[^a-z]','',s.lower())
def normalize_string_spacing(s):
    return re.sub('[\n\s\r]','',s.lower())


class QuotedSpeechTag(vozbase.VozTextContainer):
    def __init__(self,id_,offset,length,speech_type,speaker=None):
        super(QuotedSpeechTag, self).__init__(id_,offset,length)
        self.speech_type = speech_type
        self.speaker = speaker


def annotate_quoted_speech(document,quoted_speech_file):
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
    row_annotation = []
    row_speakers = []
    _get_align_sentences = []
    with open(quoted_speech_file, 'rbU') as csv_file:
        csv_reader = csv.reader(csv_file)
        for row in csv_reader:
            logger.info('< '+row[4])
            row_annotation.append(row[2])
            row_speakers.append(row[3])
            text_count_csv += len(normalize_string(row[4]))
            while text_count_sty <= text_count_csv and sty_cur < len(document.sentences):
                logger.info('> '+str(document.sentences[sty_cur]))
                if _get_align_sentences:
                    row_annotation = []
                    row_speakers = []
                _get_align_sentences.append((row_annotation,row_speakers))
                text_count_sty += len(normalize_string(document.sentences[sty_cur].get_text()))
                sty_cur += 1
    for sentence,annotations in zip(document.sentences,_get_align_sentences):
        if len(annotations[0])==1 and annotations[0][0]=='c':
            pass
        else:
            sentence.quoted_speech = annotations

def clean_quoted_speech_from_document(document):
    """
    :param document: voz.Document
    :return: None
    """
    assert isinstance(document,voz.Document)
    occurences = 0
    for sentence in list(document.sentences):
        assert isinstance(sentence,voz.Sentence)
        if sentence.quoted_speech:
            document.remove_sentence(sentence)
    logger.info("Removed %d mentions in quoted speech" % occurences)



def main():
    logging.basicConfig(level=logging.DEBUG)
    file_path = "/Users/josepvalls/voz2/stories/annotation-finlayson-01/"
    story_file = "01 - Nikita the Tanner.sty"
    quoted_speech_file = "01/sentences.csv"
    doc = styhelper.create_document_from_sty_file(file_path+story_file)
    annotate_quoted_speech(doc,file_path+quoted_speech_file)
    for sentence in doc.sentences:
        if sentence.quoted_speech:
            print "QUOTED SPEECH"
        else:
            print sentence.get_text()
    print voz.Document.format_stats(doc.get_stats())
    clean_quoted_speech_from_document(doc)
    print voz.Document.format_stats(doc.get_stats())

    pass

if __name__=='__main__':
    main()