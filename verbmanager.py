import vozbase
import collections
import util

class Dependency(object):
    def __init__(self, label, governor, dependent):
        self.label, self.governor, self.dependent = label, governor, dependent


class Verb(vozbase.VozTextContainer):
    def __init__(self,id,offset,len,token,frame,arguments):
        super(Verb, self).__init__(id,offset,len)
        self.token = token
        self.frame = frame #unused
        self.arguments = arguments
        self._subjects = None
        self._objects = None
        self._sentence = None
    def is_negated(self):
        return ('AM-NEG' in self.arguments.keys())
    def get_subjects(self):
        return [i for i in self._subjects if i and i.is_independent]
    def get_objects(self):
        return [i for i in self._objects if i and i.is_independent]
    def _clear_caches(self,sentence):
        del self._subjects
        del self._objects
        del self._sentence
    def _compute_caches(self,sentence=None):
        if sentence:
            self._sentence = sentence
        subjects = set()
        objects = set()

        for arg,tokens in self.arguments.items():
            matches = [sentence._parent_document.get_mention_by_token_id(i.id) for i in tokens]
            #matches = set([i for i in matches if i and i.is_independent]) # is_independent is not properly initialized here?
            matches = set([i for i in matches])
            if not matches:
                pass
                # TODO add the children of non-independent mentions by checking mention.contains(mention)
            else:
                if arg.startswith('A0'):
                    subjects.update(matches)
                elif (arg.startswith('A1') or arg.startswith('A2') or arg.startswith('A3')):
                    objects.update(matches)
                elif arg == 'nsubj':
                    subjects.update(matches)
                elif arg == 'expl':
                    subjects.update(matches)
                elif arg == 'nsubjpass':
                    objects.update(matches)
                elif arg == 'dobj':
                    objects.update(matches)
                elif arg == 'iobj':
                    objects.update(matches)
                elif arg == 'pobj':
                    objects.update(matches)
        self._subjects = subjects
        self._objects = objects
    def get_tokens(self):
        return [self.token]
    def get_text(self):
        return self.token.get_text()
    def __str__(self):
        return "Verb %d" % (self.id)


class VerbMapper(object):
    MODE_NO_MAP = 'NoMap'
    MODE_FRAMENET_TEXT = 'FNT'
    MODE_LEVIN_TEXT = 'LVT'
    MODE_WORDNET_TEXT = 'WNT'
    _fn = None
    _wn = None
    _verb_mapping_levin = None
    _verb_mapping_100 = None
    _verb_mapping_50 = None
    def __init__(self,mode=None):
        self._verb_mapping_cache = {}
        self._default_mode = mode or VerbMapper.MODE_NO_MAP
    def map(self,verb,mode=None,fallback=True):
        if not mode:
            mode = self._default_mode

        if mode==VerbMapper.MODE_NO_MAP:
            return verb
        else:
            if mode not in self._verb_mapping_cache:
                self._load_cache(mode)
            if not verb in self._verb_mapping_cache[mode]:
                if mode==VerbMapper.MODE_FRAMENET_TEXT:
                    self._verb_mapping_cache[mode][verb] = VerbMapper.verb_mapping_framenet(verb)
                elif mode==VerbMapper.MODE_WORDNET_TEXT:
                    self._verb_mapping_cache[mode][verb] = VerbMapper.verb_mapping_wordnet(verb)
                elif mode==VerbMapper.MODE_LEVIN_TEXT:
                    # if not found in cache, not in file
                    pass
            verb_return = self._verb_mapping_cache[mode].get(verb,None)
            if verb_return:
                return verb_return
            else:
                return verb if fallback else None

    def _load_cache(self,mode):
        if mode == VerbMapper.MODE_LEVIN_TEXT:
            self._verb_mapping_cache[mode] = self._verb_mapping_levin_load()
        else:
            try:
                0/0
                cache = vozbase.unserialize_file('/Users/josepvalls/temp/voz2/VerbMapperCache-%s.json' % mode)
            except:
                cache = {}
            self._verb_mapping_cache[mode]=cache
    def save_cache(self,mode=None):
        if not mode:
            mode = self._default_mode
        vozbase.serialize_to_file(self._verb_mapping_cache[mode],'/Users/josepvalls/temp/voz2/VerbMapperCache-%s.json' % mode)

    @classmethod
    def verb_mapping_framenet(cls,verb):
        if not cls._fn:
            try:
                from nltk.corpus import framenet as fn
                cls._fn = fn
            except:
                print "Cannot load NLTK.CORPUS"
        try:
            return [f for f in cls._fn.frames() if any(luName==verb+'.v' for luName in f.lexUnit)][0].name
        except:
            return None
    @classmethod
    def verb_mapping_wordnet(cls,verb):
        if not cls._wn:
            try:
                from nltk.corpus import wordnet as wn
                cls._wn = wn
            except:
                print "Cannot load NLTK.CORPUS"
        try:
            return collections.Counter(util.flatten([i.root_hypernyms() for i in cls._wn.synsets(verb, 'v')])).most_common()[0][0].lemma_names()[0]
        except:
            return None


    @classmethod
    def verb_mapping_100(cls,verb):
        if not cls._verb_mapping_100:
            verb_mapping_files = ['clustered-verb-counts-100.txt','clustered-verb-counts-50.txt','clustered-verbs-100.txt','clustered-verbs-50.txt']
            cls._verb_mapping_100 = cls._verb_mapping_load(verb_mapping_files[2])
        return cls._verb_mapping_100.get(verb,None)

    @classmethod
    def verb_mapping_50(cls,verb):
        if not cls._verb_mapping_50:
            verb_mapping_files = ['clustered-verb-counts-100.txt','clustered-verb-counts-50.txt','clustered-verbs-100.txt','clustered-verbs-50.txt']
            cls._verb_mapping_50 = cls._verb_mapping_load(verb_mapping_files[3])
        return cls._verb_mapping_50.get(verb,None)


    @classmethod
    def _verb_mapping_load(filename):
        verb_mapping_data = {}
        for line in open(filename).readlines():
            key = line.split(' ',1)[0]
            for i in line.split(' '):
                if i:
                    verb_mapping_data[i]=key
        return verb_mapping_data


    def _verb_mapping_levin_load(self):
        c = {}
        current = ''
        cd = 0
        data = open('data/levin.verbs.txt').readlines()
        verbs = []
        for line in data:
            if line.strip() and not line.startswith('  '):
                current = line.strip().split()[0]
                try:
                    cd = int(current.split('.')[0])
                except:
                    cd = -1
                c[current] = []
                if '"' in line:
                    c[current]=[line.split('"')[1]]
            elif cd > 8:
                c[current]+=line.strip().split()
                for i in line.strip().split():
                    verbs.append((i,current))
        verbmap = {}
        for verb,group in verbs:
            if verb in verbmap:
                if len(c[verbmap[verb]])>len(c[group]):
                    verbmap[verb] = group
            else:
                verbmap[verb] = group
        #print collections.Counter(util.flatten(c.values())).most_common()
        #print verbmap
        return verbmap

def main():
    vm = VerbMapper()
    #for mode in [VerbMapper.MODE_FRAMENET_TEXT,VerbMapper.MODE_LEVIN_TEXT,VerbMapper.MODE_WORDNET_TEXT]:
    for mode in [VerbMapper.MODE_WORDNET_TEXT]:
        vm.map('walk',mode)
        vm.map('run',mode)
        vm.map('crawl',mode)
        vm.map('jump',mode)
        vm.map('leap',mode)
    import pprint
    pprint.pprint(vm._verb_mapping_cache)


if __name__ == "__main__":
    main()