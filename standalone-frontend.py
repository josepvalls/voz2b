import webapp2

import logging

import settings
import voz
import stanfordhelper
import formatter
import util
import parse_tree_mention_helper
import styhelper
import autoreloader

logging.basicConfig(level=logging.DEBUG)

def get_default_options():
    return {
            'include_parse':False,
            'parse_highlight':parse_tree_mention_helper.HIGHLIGHT_MENTIONS_INDEPENDNET,
            'include_raw':False,
            'include_text':True,
            'text_highlight':parse_tree_mention_helper.HIGHLIGHT_MENTIONS_INDEPENDNET,
            'include_mentions':False,
            'include_verbs':False,
        }

def call_voz(text,doc_source='stanfordhelper',doc_method='create_document_from_raw_text',options={}):
    doc = stanfordhelper.create_document_from_raw_text(text)
    options = dict(get_default_options(), **options)
    return formatter.html(
        formatter.VozHTMLFormatter.format(doc,options=options)
    )

def call_voz_sty(story_file):
    doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH+story_file) #type: voz.Document
    return formatter.html(
        formatter.VozHTMLFormatter.format(doc,get_default_options())
    )


def get_form(text):
    form = '<form action="text" method="POST"><textarea name="text" cols="60" rows="20">'+text+'</textarea>'
    form +='<br/><input type="submit"></input>'
    form += '</form>'
    form += '<form action="sty" method="POST">'
    form += formatter.html_wrap('select name="sty_file"',''.join([formatter.html_wrap('option',i) for i in settings.STY_FILES]))
    form +='<br/><input type="submit"></input>'
    form += '</form>'
    return formatter.html(form)

class VozFrontend(webapp2.RequestHandler):
    def get(self):
        file_path = "/Users/josepvalls/voz2/data/"
        story_file = "TestInput.txt"
        text = open(file_path+story_file).read()
        self.response.write(formatter.html(get_form(text)))
    def post(self):
        text = self.request.get('text')
        if not text:
            self.get()
            return
        self.response.write(call_voz(text))

class VozStyFrontend(webapp2.RequestHandler):
    def get(self):
        self.response.write("No story selected...")
    def post(self):
        self.response.write(call_voz_sty(self.request.get('sty_file')))


import thread
thread.start_new_thread(autoreloader.reloader_thread, ())

app = webapp2.WSGIApplication([
    ('/', VozFrontend),
    ('/text', VozFrontend),
    ('/sty', VozStyFrontend),
], debug=True)

def main():
    from paste import httpserver
    httpserver.serve(app, host='127.0.0.1', port='8008', use_threadpool=False)

if __name__ == '__main__':
    main()