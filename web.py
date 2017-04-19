#!/usr/bin/env python

import webapp2
import json, datetime, time
import voz
import traceback
import logging
import settings
settings.DATA_DISABLE_CACHING = True
import stanfordhelper
import voz
import verbmanager
import collections
import sys
import os
import re
import verbmanager
import formatter,parse_tree_mention_helper
import graphhelper

BASE_TEXT = "One morning, Bob met Alice for brunch but she didn't eat her food."

def base_form(text, stories=[]):
    def mybool(k):
        return True if k == 'True' else False
    def inputfor(k):
        if k == 'OPTIONS': return None
        if not k.upper() == k: return None
        if k in settings.OPTIONS:
            return ('''%s: ''' % k) + ''.join(
                ['''<input name="%s" type="radio" value="%s" %s/>%s''' % (
                k, i, 'checked' if getattr(settings, k) == i else '', i) for i in settings.OPTIONS[k]])
        elif isinstance(getattr(settings, k), bool):
            return ('''%s: ''' % k) + ''.join(
                ['''<input name="%s" type="radio" value="%s" %s/>%s''' % (
                k, i, 'checked' if getattr(settings, k) == mybool(i) else '', i) for i in ['True', 'False']])
        else:
            return '''%s: <input name="%s" type="text" value="%s"/>''' % (k, k, getattr(settings, k))
    body = '''<html><body><form method="POST"><textarea name="text" cols="60" rows="20">''' + text + '''</textarea>'''
    body += '''<br/><input type="submit"></input>'''
    #body += str(settings.STY_FILES) + str(type(settings.STY_FILES))
    #body += "<br/>" + "<br/>".join(['<input name="story" type="checkbox" value="%s"%s/>%s' % (i, ' checked' if i in stories else '', i) for i insettings.STY_FILES])
    body += '''<br/><input type="submit"></input>'''
    body += "<br/>" + "<br/>".join([i for i in [inputfor(k) for k in dir(settings)] if i])
    body += '''<br/><input type="submit"></input>'''
    body += '''</form></body></html>'''
    return body

class JSONDateTimeEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, datetime.datetime):
            return int(time.mktime(obj.timetuple()))
        try:
            return json.JSONEncoder.default(self, obj)
        except:
            return str(obj)

class MainForm(webapp2.RequestHandler):
    def get(self):
        self.response.write(base_form(BASE_TEXT))

    def post(self):
        try:
            for k in dir(settings):
                if k == 'OPTIONS': continue
                if not k.upper() == k: continue
                try:
                    if isinstance(getattr(settings, k), bool):
                        setattr(settings, k, True if self.request.get(k) == 'True' else False)
                    elif k and isinstance(getattr(settings, k), int):
                        setattr(settings, k, int(self.request.get(k)))
                    else:
                        setattr(settings, k, self.request.get(k))
                except Exception as e:
                    self.response.write('Error setting parameters in the default form: ' + str(e))
                    return
            text = self.request.get('text')
            if self.request.get_all('story'):
                body = 'Not supported yet'
            else:
                body = ''
                document = stanfordhelper.create_document_from_raw_text(text,{'cache':False})
                document.compute_predictions()
                body += formatter.VozHTMLFormatter.format(document,options={
                'include_parse':True,
                'parse_highlight':parse_tree_mention_helper.HIGHLIGHT_MENTIONS_INDEPENDNET,
                'include_raw':False,
                'include_text':True,
                'text_highlight':parse_tree_mention_helper.HIGHLIGHT_MENTIONS_INDEPENDNET,
                'include_mentions':False,
                'include_verbs':True,
                })
                g = graphhelper.doc_to_graph(document)
                body += formatter.VozHTMLFormatter.format_story_graph(g)
                body = formatter.html(body)
        except Exception as e:
            # body = str(e)
            body = '<pre>' + traceback.format_exc() + '</pre>'
        self.response.write(body)


class RController(webapp2.RequestHandler):
    def get(self):
        return self.post(True)

    def post(self, default=False):
        # try:
        if True:
            if not default:
                params = json.loads(self.request.body)
            else:
                params = {}
            text = params.get('text', BASE_TEXT)
            cmd = params.get('cmd', '[i.get_text() for i in document.sentences]')
            document = stanfordhelper.create_document_from_raw_text(text,{'cache':False})
            resp = eval(cmd)
            # except Exception as e:
            # import traceback
            # resp = '['+str(traceback.format_exc())+']'
            # resp = str(e)
        self.response.headers['Content-Type'] = 'application/json'
        self.response.write(json.dumps(resp, sort_keys=True, indent=2, cls=JSONDateTimeEncoder))

app = webapp2.WSGIApplication([
    ('/', MainForm),
    ('/rpc', RController),
], debug=True)
if __name__ == '__main__':
    from paste import httpserver
    httpserver.serve(app, host=settings.WEB_HOST, port=settings.WEB_PORT)