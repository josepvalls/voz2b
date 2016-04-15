import urllib,urllib2
import hashlib,base64
import os
import time
import json
import logging
import util
import settings

if os.environ.get('SERVER_NAME',None):
    GOOGLEDB = True
else:
    GOOGLEDB = False

if GOOGLEDB:
    try:
        from google.appengine.api import urlfetch
        from google.appengine.ext import db
        class Cache(db.Model):
            value = db.BlobProperty()
            date = db.DateTimeProperty(auto_now_add=True)
        urlfetch.set_default_fetch_deadline(45)
    except:
        pass
else:
    import sqlite3

try:
    import httplib2
    httplib2.Http(timeout=45)
except:
    pass

logger = logging.getLogger(__name__)

class CacheManager(object):
    pass

class CacheManagerGAE(CacheManager):
    def store(self,key,value):
        cd = Cache(key_name=key,value=value.encode('utf-8') )
        cd.put()
    def retrieve(self,key):
        cd = db.get(db.Key.from_path('Cache', key))
        return cd.value.decode('utf-8') if cd else None

class CacheManagerLocal(CacheManager):
    def __init__(self):
        self.con = sqlite3.connect(settings.DATA_LOCAL_CACHE_DB_FILE)
        self.cur = self.con.cursor()
        self.cur.execute("CREATE TABLE IF NOT EXISTS cache ( key TEXT, value TEXT, time NUMERIC);")
    def store(self,key,value):
        try:
            self.cur.execute("INSERT INTO cache (key, value, time) VALUES (?, ?, ?);", (key, value, int(time.time())))
            self.con.commit()
        except:
            logger.warn('Couldn\'t store to cache with key %s.' % key)
    def retrieve(self,key):
        self.cur.execute("SELECT value FROM cache WHERE key = '%s';" % (key))
        rs = self.cur.fetchone()
        return rs[0] if rs and rs[0] else None

if GOOGLEDB:
    cm = CacheManagerGAE()
else:
    cm = CacheManagerLocal()

# Data Acquirers
class Acquirer(object):
    def __repr__(self):
        return "%s (%d)" % (self.__class__.__name__ ,id(self))
    def test(self):
        return -1 # Not implemented


class CachedAcquirer(Acquirer):
    def __init__(self):
        self.cache_manager = cm
    def __repr__(self):
        return "%s at %s (%d)" % (self.__class__.__name__ ,self.get_url(),id(self))
    def get_url(self):
        return 'URL'
    def query(self,q):
        key = self.key(q)
        data = None
        if not settings.DATA_DISABLE_CACHING:
            data = self.cache_manager.retrieve(key)
        if data == None:
            logger.warn('CachedAcquirer fetching %s using: %s' % (key,self.__class__.__name__))
            try:
                data = self.fetch(q)
            except:
                pass
                logger.error('CachedAcquirer error %s in: %s' % (key,self.__class__.__name__))
            if not data == None:
                try:
                    open('temp_xmldoc.xml','w').write(data)
                except:
                    pass
                data = data.decode('utf-8', errors='ignore')
                #data = data.decode('latin-1', errors='ignore')
                self.cache_manager.store(key,data)
        else:
            logger.info('CachedAcquirer cache hit fetching %s using: %s' % (key,self.__class__.__name__))
        return data

class CAStanfordCoreNLPLocal(CachedAcquirer):
    def key(self,q):
        return 'nlp_'+base64.b64encode(hashlib.sha1(q).digest())
    def fetch(self,q):
        query = {'sent':q,'annotators':'tokenize, ssplit, pos, lemma, ner, parse, dcoref','print':'xml'}
        url = self.get_url()
        return urllib2.urlopen(url,urllib.urlencode(query),240).read()
    def test(self):
        query = {'sent':'Colorless green ideas sleep furiously.','annotators':'tokenize','print':'xml'}
        url = self.get_url()
        try:
            urllib2.urlopen(url,urllib.urlencode(query),3).read()
            return 1 # Ok
        except:
            return 0 # Fail
    def get_url(self):
        return settings.DATA_USE_STANFORD_SYSTEM_LOCAL_URL+'/nlp'


class CAStanfordCoreNLPDrexel(CAStanfordCoreNLPLocal):
    def get_url(self):
        return settings.DATA_USE_STANFORD_SYSTEM_LOCAL_URL_ALT+'/nlp'


class CAStanfordCoreNLPOnline(CachedAcquirer):
    def key(self,q):
        return 'nlp_'+base64.b64encode(hashlib.sha1(q).digest())
    def fetch(self,q):
        url = self.get_url()
        query = {'input':q,'outputFormat':'visualise','Process':'Submit'}
        data = urllib2.urlopen(url,urllib.urlencode(query),45).readlines()
        data = [i for i in [i.strip() for i in data] if i.startswith('stanfordXML')]
        if not data:
            raise util.KnownException('Input text is too long for the Stanford Core NLP web service.')
        return data[0][15:-2].replace('\\','')
    def get_url(self):
        return 'http://nlp.stanford.edu:8080/corenlp/process'
    def test(self):
        query = {'input':'Colorless green ideas sleep furiously.','outputFormat':'visualise','Process':'Submit'}
        try:
            data = urllib2.urlopen(self.get_url(),urllib.urlencode(query),45).readlines()
            return 1 # Ok
        except:
            return 0 # Fail

class CAStanfordParserLocal(CachedAcquirer):
    def key(self,q):
        return 'spo_'+base64.b64encode(hashlib.sha1(json.dumps(sorted(q.items()))).digest())
    def fetch(self,q):
        return urllib2.urlopen(self.get_url(),urllib.urlencode({'sent':q})).read()
    def get_url(self):
        return settings.DATA_USE_STANFORD_SYSTEM_LOCAL_URL+'/parse'
    def test(self):
        try:
            q = 'Colorless green ideas sleep furiously.'
            urllib2.urlopen(self.get_url(),urllib.urlencode({'sent':q}),timeout=2).read()
            return 1 # Ok
        except:
            return 0 # Fail



class CAStanfordParserDrexel(CAStanfordParserLocal):
    def get_url(self):
        return settings.DATA_USE_STANFORD_SYSTEM_LOCAL_URL_ALT+'/parse'


class CAStanfordParserGoogle(CAStanfordParserLocal):
    def get_url(self):
        return settings.DATA_USE_STANFORD_SYSTEM_MIRROR_URL+'/parse'

class CAStanfordParserOnline(CAStanfordParserGoogle):
    def get_url(self):
        return 'http://nlp.stanford.edu:8080/parser/index.jsp'
    def test(self):
        return 0


if settings.DATA_USE_STANFORD_SYSTEM_LOCAL:
    if settings.DATA_USE_STANFORD_SYSTEM_LOCAL_ALT:
        stanford_nlp = CAStanfordCoreNLPDrexel()
        stanford_parser = CAStanfordParserDrexel()
    else:
        stanford_nlp = CAStanfordCoreNLPLocal()
        stanford_parser = CAStanfordParserLocal()
else:
    stanford_nlp = CAStanfordCoreNLPOnline()
    stanford_parser = CAStanfordParserOnline()


class CAConceptnetOnline(CachedAcquirer):
    def key(self,q):
        return 'cnu_'+q
    def fetch(self,url):
        logger.debug('CAConceptnet.fetch %s' % url)
        return urllib2.urlopen(url,None,240).read()
    def query_concept(self,lemma,edge=None,limit=None):
        try:
            # http://conceptnet5.media.mit.edu/data/5.4/c/en/toast?offset=5&limit=5
            url = self.get_url+'/c/en/' + urllib.quote(lemma.replace(' ','_'))
            query = {'filter':'core','limit':1}
            data = json.loads(self.query(url+'?'+urllib.urlencode(query)))
            try:
                desc=(data['edges'][0]['surfaceEnd'])
            except:
                desc=None
            # http://conceptnet5.media.mit.edu/data/5.4/search?rel=/r/PartOf&end=/c/en/car&limit=10
            url = self.get_url+'/search'
            #query = {'filter':'core','start':'/c/en/'+lemma}
            #data_search = self.query(url+'?'+urllib.urlencode(query))
            #urlencode is encoding / as %2F
            query_string = url+'?start=/c/en/'+urllib.quote(lemma,'/')+'/'
            if edge:
                query_string += '&rel=' + urllib.quote(edge,'/')
            if limit:
                query_string += '&limit=%d' % limit
            data_search = self.query(query_string)
            return (desc,json.loads(data_search))
        except:
            logger.warning('CAConceptnet.query_concept not found %s' % lemma)
            return (None,{'numFound':None})
    def query_assoc(self,a,b):
        #http://conceptnet5.media.mit.edu/data/5.1/assoc/c/en/live/v?filter=/c/en/be/v&limit=10
        #http://conceptnet5.media.mit.edu/data/5.4/assoc/c/en/cat?filter=/c/en/dog/.&limit=1
        url = self.get_url+'/assoc%s?filter=%s/.&limit=1' % (urllib.quote(a),urllib.quote(b))
        try:
            data = self.query(url)
            return json.loads(data)
        except:
            logger.warning('CAConceptnet.query_assoc not found %s - %s' % (a,b))
            return {u'similar':[]}
    def query_expand(self, token):
        data = None
        edge_translator = {'/r/HasProperty':'related',
                           #'/r/HasA':'related',
                           '/r/ConceptuallyRelatedTo':'related',
                           '/r/RelatedTo':'related',
                           '/r/DerivedFrom':'related',
                           '/r/IsA':'isa',
                           #'/c/en/be_in':'partof',
                           #'/c/en/be_near':'partof',
                           '/r/AtLocation':'partof',
                           #'/c/en/in':'partof',
                           #'/c/en/have_or_involve':'related',
                           '/r/UsedFor':'partof',
                           '/r/CapableOf':'can',

                           }
        #if token.POS.lower()[0] in ['n','v']:
            #desc,data = self.query_concept(token.lemma)
            #desc,data = self.query_concept(token.lemma + '/' + token.POS.lower()[0])
        #if not data or not data['numFound']:
            #desc,data = self.query_concept(token.lemma)

        relations = []
        for k,v in edge_translator.items():
            desc,data = self.query_concept(token.lemma.lower(),edge=k,limit=100)
            if data['numFound']:
                if not relations:
                    relations.append(('desc',desc))
                for edge in data['edges']:
                    # ignore DBPedia?
                    #if [source for source in edge['sources'] if 'dbpedia' in source]]: continue
                    relations.append((v,edge['end'].split('/')[3]))
        return relations
    def test(self):
        url = self.get_url()+'/c/en/dog'
        try:
            urllib2.urlopen(url,timeout=2).read()
            return 1 # Ok
        except:
            return 0 # Fail
    def get_url(self):
        return 'http://conceptnet5.media.mit.edu/data/5.4'



conceptnet = CAConceptnetOnline()

services = ['stanford_nlp','stanford_parser','conceptnet']
services_impl = {'stanford_nlp':[CAStanfordCoreNLPOnline,CAStanfordCoreNLPDrexel,CAStanfordCoreNLPLocal],
                 'stanford_parser':[CAStanfordParserOnline,CAStanfordParserGoogle,CAStanfordParserDrexel,CAStanfordParserLocal],
                 'conceptnet':[CAConceptnetOnline]}

def main():
    # Testing connectivity
    import sys
    service_status_codes={-1:"Not implemented",0:"Offline",1:"Success"}
    for service in services:
        service_impl = getattr(sys.modules[__name__],service)
        print "Service %s using implementation %s is %s" % (service,str(service_impl),service_status_codes[service_impl.test()])
        for alternative in services_impl[service]:
            alt_impl = alternative()
            print "Service %s alternative %s is %s" % (service,str(alt_impl),service_status_codes[alt_impl.test()])


if __name__ == '__main__':
    main()