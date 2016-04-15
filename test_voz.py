from unittest import TestCase
import voz

class TestDocument(TestCase):
    def test_get_coreference_group_by_id(self):
        self.fail()

    def test_get_sentence_by_id(self):
        self.fail()

    def test_get_mention_by_token_id(self):
        self.fail()

    def test_get_long_id(self):
        self.fail()

    def test_get_statistics(self):
        self.fail()

    def test_get_all(self):
        self.fail()

    def test_get_all_mentions(self):
        self.fail()

    def test_get_all_verbs(self):
        self.fail()

    def test_format(self):
        self.fail()

    def test_get_text(self):
        self.fail()
        pass
    def test_create_document_from_text(self):
        doc = voz.create_document_from_text("Hello, this is dog.\nHow are you?")
        assert(str(doc)=="""Document  (create_document_from_text) text:
Hello, this is dog.
How are you?
Document Sentences:
hello/? ,/? this/? is/? dog/? ./?
how/? are/? you/? ?/?""")
    def test_create_document_from_jsonpickle(self):
        doc = voz.create_document_from_text("Hello, this is dog.\nHow are you?")
        file_name = 'temp_document.json'
        doc.serialize_to_file(file_name)
        doc_copy = voz.create_document_from_jsonpickle_file(file_name)
        assert(str(doc)==str(doc_copy))


