from BaseHTTPServer import HTTPServer, BaseHTTPRequestHandler
from SocketServer import ThreadingMixIn
import threading
import cgi,urlparse
import os
import uuid
import datetime
import re

try:
    import simplejson as json
except ImportError:
    import json

PORT = 8787
DATA_PATH = 'saved_data'
address = 'localhost'
address = ''

def get_message(path,uid='None'):
    if path=='/form1':
        return open("tool_irb_form_3.html").read().replace('%UUID%',uid)
    elif path=='/form2':
        return open("tool_irb_thanks.html").read().replace('%UUID%',uid)
    else:
        return open("tool_irb_consent.html").read()

def save_data(handler):
    content_type, pdict = cgi.parse_header(handler.headers.getheader('content-type'))
    if content_type == 'multipart/form-data':
        postvars = cgi.parse_multipart(handler.rfile, pdict)
    elif content_type == 'application/x-www-form-urlencoded':
        content_length = int(handler.headers.getheader('content-length'))
        postvars = urlparse.parse_qs(handler.rfile.read(content_length), keep_blank_values=1)
    else:
        postvars = {}
    postvars['timestamp'] = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    postvars['client'] = handler.client_address
    #print postvars
    if 'uuid' in postvars and postvars['uuid'] and postvars['uuid'][0] and postvars['uuid'][0].strip():
        uid = postvars['uuid'][0].strip()
    else:
        uid = uuid.uuid4().hex
    uid = re.sub(r'[^a-z0-9]', '_', uid)

    path = os.path.dirname(os.path.abspath(__file__))
    path = path + '/' + DATA_PATH
    try:
        os.makedirs(path)
    except:
        pass
    path += '/' + uid + re.sub(r'[^a-z0-9]', '_',handler.path.strip())
    with open(path,'w') as f:
        f.write(json.dumps(postvars))
    return uid

def field_names_to_dict(d):
    return dict([(i['name'], i['value']) for i in d])

class Handler(BaseHTTPRequestHandler):

    def do_GET(self):
        self.send_response(200, "OK")
        self.send_header('Content-type', 'text/html')
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        message =  get_message(self.path.strip())
        self.wfile.write(message)
        self.wfile.write('\n')
        return

    def do_POST(self):
        if self.path.strip()=="/json":
            content_type, pdict = cgi.parse_header(self.headers.getheader('content-type'))
            content_length = int(self.headers['Content-length'])
            content = self.rfile.read(content_length)
            data = field_names_to_dict(json.loads(content))
            data['timestamp'] = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            data['client'] = self.client_address
            # print postvars
            if 'uuid' in data and data['uuid'].strip():
                uid = data['uuid']
            else:
                uid = uuid.uuid4().hex
            uid = re.sub(r'[^a-z0-9]', '_', uid)
            path = os.path.dirname(os.path.abspath(__file__))
            path = path + '/' + DATA_PATH
            path += '/' + uid + '_' + uuid.uuid4().hex
            #print path,data
            with open(path, 'w') as f:
                f.write(json.dumps(data))
            self.send_response(200, "OK")
            self.send_header('Content-type', 'application/json')
            self.send_header("Access-Control-Allow-Origin", "*")
            self.end_headers()
            self.wfile.write(json.dumps({"status": "OK"}))
        else:
            uid = save_data(self)
            self.send_response(200, "OK")
            self.send_header('Content-type', 'text/html')
            self.send_header("Access-Control-Allow-Origin", "*")
            self.end_headers()
            message = get_message(self.path.strip(),uid)
            self.wfile.write(message)
            self.wfile.write('\n')
        return

    def do_OPTIONS(self):
        '''CORS'''
        self.logRequest()
        self.send_response(200, "OK")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header("Access-Control-Allow-Headers", "content-type")
        self.send_header("Access-Control-Allow-Headers", "X-Requested-With")
        self.end_headers()
        self.wfile.write("OK")


class ThreadedHTTPServer(ThreadingMixIn, HTTPServer):
    """Handle requests in a separate thread."""
    pass

if __name__ == '__main__':
    server = ThreadedHTTPServer((address, PORT), Handler)
    print 'Starting server on http://%s:%d, use <Ctrl-C> to stop' % (address,PORT)
    server.serve_forever()