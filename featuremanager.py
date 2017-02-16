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
import entitymanager
import parse_tree_mention_helper
import logging
import util
import voz
import settings
from voz import Token, Sentence
import nltkhelper
import networkcachemanager
import dependencyhelper
import verbmanager

wn = None

DO_FORCE_FEATURE_RELOAD = False

verbs_list = ['be', 'have', 'do', 'go', 'want', 'make', 'give', 'call', 'take', 'tell', 'get', 'know', 'ask',
              'come', 'look', 'happen', 'put', 'say', 'leave', 'become', 'think', 'run', 'sit', 'fall', 'bring',
              'see', 'hear', 'stand', 'pass', 'seem', 'find']

verbs_groups = [
    ['do', 'act', 'get'],
    ['be', 'can', 'want', 'have', 'live'],
    ['think', 'love', 'hate'],
    ['talk', 'communicate', 'say', 'tell', 'order', 'command'],
    ['answer', 'reply', 'respond'],
    ['ask', 'inquire', 'test', 'judge', 'try'],
    ['punish', 'kill', 'kidnap', 'pursue'],
    ['help', 'rescue', 'save', 'hide'],
    ['reward', 'pay', 'give'],
    ['move', 'go', 'transport', 'arrive', 'carry'],
]

dep_list = ['prep_to', 'prep_from', 'prep_of', 'prep_with', 'prep_for', 'prep_in_on_at', 'prep_other'] +\
           ['poss', 'appos'] + \
           ['det_the', 'det_other'] + \
           ['dobj', 'iobj', 'pobj']

pronoun_list = [
    'SingularFirstPerson i me myself mine my',
    'PluralFirstPerson we us ourselves ourself ours our',
    'SingularSecondPerson thou thee yourself thyself thine thy',
    'PluralSecondPerson you yourselves your yours',
    'SingularThirdPersonMale he him himself his',
    'SingularThirdPersonFemale she her herself hers',
    'SingularThirdPersonNeuter it its itself',
    'SingularThirdPersonGeneric zero one oneself another other different a the this that which whichever such that',
    'PluralThirdPerson they them themselves their theirs',
    'PluralThirdPersonGeneric zero two many much few little couple several most these those each every whichever such that all both plenty several many'
]


class Features(object):
    def __init__(self):
        self.features = []
    def compute(self, verb, sentence):
        pass
class MentionFeatures(Features):
    feature_names = []
    offset_dep_features = 0
    offset_verb_features = 0
    def compute(self, mention, sentence):
        mention_nodes, mention_from_nodes = parse_tree_mention_helper.get_mention_nodes(mention, sentence)
        tokens = mention.tokens
        self.features = []
        self.features += FeatureProviderTreeNodeLabelClass().features_from_tree(mention_nodes,mention_from_nodes)
        self.features += FeatureProviderTokenLabelClass().features_from_tokens(tokens)
        self.features += FeatureProviderWordnet().features_from_tokens(tokens)
        self.features += FeatureProviderLists().features_from_tokens(tokens)
        self.features += FeatureProviderNNTokens().features_from_tokens(tokens)
        self.features += [-1 for _ in FeatureProviderDependenciesM.features_names()]
        self.features += [0 for _ in FeatureProviderVerbsM.features_names()]
        return self.features
    @classmethod
    def get_feature_names(cls):
        if not cls.feature_names:
            cls.feature_names = []
            cls.feature_names += FeatureProviderTreeNodeLabelClass().features_names()
            cls.feature_names += FeatureProviderTokenLabelClass().features_names()
            cls.feature_names += FeatureProviderWordnet().features_names()
            cls.feature_names += FeatureProviderLists().features_names()
            cls.feature_names += FeatureProviderNNTokens().features_names()
            cls.offset_dep_features = len(cls.feature_names)
            cls.feature_names += FeatureProviderDependenciesM.features_names()
            cls.offset_verb_features = len(cls.feature_names)
            cls.feature_names += FeatureProviderVerbsM.features_names()
        return cls.feature_names

class VerbFeatures(Features):
    feature_names = []
    @classmethod
    def get_feature_names(cls):
        return []
    def compute(self, verb, sentence):
        return []

class FeatureContainer(object):
    def __init__(self, document):
        self._feature_managers = {}
        self.document = document
    def print_features(self,obj):
        print len(self.get_feature_labels(obj)),
        print len(self.get_features(obj))
        for k,v in zip(self.get_feature_labels(obj),self.get_features(obj)):
            print k,v
    def init_features(self):
        '''
        param document: document for which to init features, otherwise computed lazily
        :type document: voz.Document
        :return: None
        '''
        self._feature_managers = {'v':{},'m':{}}
        if not self._load_feature_cache():
            #for mention in self.document.get_all_mentions():
            #for verb in self.document.get_all_verbs():
            MentionFeatures.get_feature_names() # ensure offsets are computed
            for sentence in self.document.sentences:
                sentence._compute_caches(self.document)

                for mention in sentence.mentions:
                    self._feature_managers['m'][mention.id] = MentionFeatures().compute(mention, sentence)
                for dep in sentence.dependencies:
                    FeatureProviderDependenciesM.features_from_dependencies(dep, sentence, self)
                for verb in sentence.verbs:
                    FeatureProviderVerbsM.features_from_verbs(verb, sentence, self)
                    self._feature_managers['v'][verb.id] = VerbFeatures().compute(verb, sentence)
            self._persist_feature_cache()
        return self
    def document_filename(self):
        return settings.FEATURE_MANAGER_COMPUTED_FEATURES_PATH + str(self.document.id) + '.json'

    def _persist_feature_cache(self):
        if not self.document.properties.get('cache', True): return False
        vozbase.serialize_to_file(self._feature_managers,self.document_filename())

    def _load_feature_cache(self):
        if DO_FORCE_FEATURE_RELOAD: return False
        if not self.document.properties.get('cache',True): return False
        try:
            self._feature_managers = vozbase.unserialize_file(self.document_filename())
            # json uses strings for the keys!
            for i in ['m','v']:
                self._feature_managers[i] = dict([(int(k),v) for k,v in self._feature_managers[i].items()])
        except:
            return False
        return True
    def get_feature_labels(self, obj):
        if isinstance(obj, entitymanager.Mention):
            return MentionFeatures.get_feature_names()
        elif isinstance(obj, verbmanager.Verb):
            return VerbFeatures.get_feature_names()
        else:
            raise Exception()

    def get_features(self,obj,t=None):
        if not t:
            if isinstance(obj, entitymanager.Mention):
                t='m'
            elif isinstance(obj, verbmanager.Verb):
                t='v'
            else:
                raise Exception()
        if not self._feature_managers:
            self.init_features()
        return self._feature_managers[t].get(obj.id, [])

    def get_features_verb(self, verb):
        '''
        :param verb:
        :return: Feature Manager
        :rtype: VerbFeatureManager
        '''
        return self.get_features(verb,'v')
    def get_features_mention(self, mention):
        '''
        :param mention:
        :return: Feature Manager
        :rtype: MentionFeatureManager
        '''
        return self.get_features(mention,'m')


class FeatureProvider(object):
    pass

class FeatureProviderTreeNodeLabelClass(FeatureProvider):
    _from_node = ['S', 'VP', 'NP', 'ADJP', 'PP', 'ADVP', 'NP-TMP', 'SQ', 'SBAR', 'SINV', 'SBARQ']
    _has_node = ['--', 'ADJP', 'ADVP', 'CC', 'CD', 'DT', 'EX', 'IN', 'JJ', 'JJR', 'JJS', 'NN', 'NNP', 'NNS', 'NP','PDT', 'POS', 'PP', 'PRP', 'PRP$', 'QP', 'RB', 'VBG', 'VBN', 'WDT', 'WP']
    def features_names(self):
        return ['fromNode' + i for i in self._from_node] + \
               ['hasNode' + i for i in self._has_node]
    def features_from_tree(self,mention_nodes,mention_from_nodes):
        features = []
        for node_label in self._from_node:
            if node_label in mention_from_nodes:
                features.append(1.0)
            else:
                features.append(0.0)
        for node_label in self._has_node:
            if node_label in mention_nodes:
                features.append(1.0)
            else:
                features.append(0.0)
        return features

class FeatureProviderTokenLabelClass(FeatureProvider):
    def features_names(self):
        return ['hasPRP','hasIt','hasPOS','hasNN','hasNNP','hasEX','hasJJ','hasDT','hasWDT','hasRB'] + \
               ['hasSingularFirstPerson','hasPluralFirstPerson','hasSingularSecondPerson','hasPluralSecondPerson','hasSingularThirdPersonMale','hasSingularThirdPersonFemale','hasSingularThirdPersonNeuter','hasSingularThirdPersonGeneric','hasPluralThirdPerson','hasPluralThirdPersonGeneric']

    def features_from_tokens(self, tokens):
        features = []
        if [i for i in voz.Token.filter(tokens, pos_list=['PRP']) if not i.lemma == 'it']:
            features.append(1.0)
        else:
            features.append(0.0)
        if [i for i in voz.Token.filter(tokens, pos_list=['PRP']) if i.lemma == 'it']:
            features.append(1.0)
        else:
            features.append(0.0)
        if [i for i in voz.Token.filter(tokens, pos_list=['PRP$', 'POS'])]:
            features.append(1.0)
        else:
            features.append(0.0)
        if voz.Token.filter(tokens, pos_list=['NN', 'NNS']):
            features.append(1.0)
        else:
            features.append(0.0)
        if voz.Token.filter(tokens, pos_list=['NNP', 'NNPS']):
            features.append(1.0)
        else:
            features.append(0.0)
        if voz.Token.filter(tokens, pos_list=['EX']):
            features.append(1.0)
        else:
            features.append(0.0)
        if voz.Token.filter(tokens, pos_list=['JJ', 'JJR']):
            features.append(1.0)
        else:
            features.append(0.0)
        if voz.Token.filter(tokens, pos_list=['DT']):
            features.append(1.0)
        else:
            features.append(0.0)
        if voz.Token.filter(tokens, pos_list=['WDT']):
            features.append(1.0)
        else:
            features.append(0.0)
        if voz.Token.filter(tokens, pos_list=['RB']):
            features.append(1.0)
        else:
            features.append(0.0)
        for group in pronoun_list:
            items = group.split()
            for token in tokens:
                if token.pos not in ['NN', 'NNP', 'NNS', 'NNPS', 'PRP', 'PRP$'] and token.text.lower() in items[1:]:
                    features.append(1.0)
                    break
            else:
                features.append(0.0)
        return features

class FeatureProviderWordnet(FeatureProvider):
    def features_names(self):
        return ['wnYoung',
                'wnOld',
                'wnVillain',
                'wnAnimal',
                'wnPerson',
                'wnLocation',
                'wnVehicle',
                'wnCreature']
    def reference_data(self):
        return ['young/j boy/n girl/n baby/n baby/j',
         'old/j man/n father/n grandfather/n',
         'villian/n terrorist/n dragon/n which/n evil/n devil/n evil/*',
         'animal/n pet/n creature/n',
         'person/n human/n being/n occupation/n job/n title/n',
         'place/n location/n city/n town/n house/n home/n cabin/n harbor/n path/n field/n forest/n land/n sea/n ocean/n room/n palace/n island/n',
         'vehicle/n car/n ship/n boat/n sleigh/n',
         'goblin/n witch/n god/n dragon/n creature/n']
    def features_from_tokens(self, tokens):
        features = []
        for reference_data in self.reference_data():
            data_targets = [tuple(i.split('/')) for i in reference_data.lower().split()]
            cloud = util.flatten(
                [nltkhelper.nltk_wordnet.query(i[0], i[1]) for i in data_targets if i[1]])
            matches = self.wordnet_sysnset_comparison(
                [i for i in tokens if i.pos in ['NN', 'NNS', 'JJ', 'JJR', 'JJS']],cloud)
            if matches:
                if settings.FEATURE_MANAGER_USE_TOKEN_WEIGHTS_FOR_FEATURE_VALUE:
                    weights = [i[0].weight * i[1] for i in matches]
                    feature_value = sum(weights) / len(matches)
                else:
                    feature_value = sum([i[1] for i in matches]) / len(matches)
                features.append(feature_value)
            else:
                features.append(0.0)
        return features
    def wordnet_sysnset_comparison(self, tokens, cloud):
        matches = []
        for leaf in tokens:
            weight = [[target.wup_similarity(synset) for target in cloud] for synset in
                      nltkhelper.nltk_wordnet.query(leaf.lemma, leaf.pos)] + [[0.0]]
            weight = util.flatten(weight)
            weight = max(weight)
            if weight > 0.1:
                matches.append((leaf, weight))
        return matches

class FeatureProviderLists(FeatureProvider):
    def features_names(self):
        return [
            'ProperNamesMale',
            'ProperNamesFemale',
            'CommonNamesMale',
            'CommonNamesFemale',
            'CommonNamesPersons',
            'CommonTitles',
            'CommonOccupations',
        ]
    def reference_data(self):
        return [
            ('NNP',nltkhelper.nltk_names.query('male.txt')),
            ('NNP',nltkhelper.nltk_names.query('female.txt')),
            ('NN', [i.strip().split('\t')[0] for i in open(settings.RESOURCE_GENDER_COMMON_NAMES, 'r').readlines()]),
            ('NN', [i.strip().split('\t')[1] for i in open(settings.RESOURCE_GENDER_COMMON_NAMES, 'r').readlines()]),
            ('NN', 'parent stepparent child grandchild stepchild'.split()),
            ('NN', 'princess prince king queen tsarevna tsarita nobleman master tsar'.split()),
            ('NN', 'tanner merchant workman pirate laborer abecedarian acater accomptant accoucheus accoutrer actuary administrator administratrix advocate aeronaut affeerer affeeror alabasterer alderman ale-wife alnagar alnager amanuensis anchoress anchoret anchorite anchorsmith ancress anilepman apothecary appraiser aproneer apronman aquafortist arbalester armourer arpenteur ashman assayer auctioneer aulnagar axeman axman backmann backster bagman bailie bailiff baillee baillie bairman baizeweaver baler balister bandster banker bard bareman bargee bargeman barkeeper barkman basher basketman battuere baxter beadle beadman beadsman beagle beamer bearer beater beaver beck becker bedel bedell bedesman bedwevere beetler belleyetere belter bender besswarden biddy billboarder billiter billyman binder blacksmith bladesmith blemmere blentonist blockcutter blocker bloodletter bloodman bloomer blower blowfeeder bluestocking bluffer boardwright boastsman boatswain bodger bolter bondman bonesetter bookholder bootbinder borreler bottiler bottler bottomer bowdler bower bowermaiden bowler bowlwoman bowyer bozzler braider brakeman brakesman brazier brightsmith broderer broiderer broom-dasher browderer brownsmith bucklesmith buddleboy buddler bunter burgomaster burmaiden burner burnisher bushelman bushelwoman butner caddie caddy cadger cainer caird calendarer calenderman calker cambist camlet caner canon canoness canter canvaser capitalist captain cardmaker carman carrier carry-in-boy carter cartomancer cartwright carver caseman caster castrator catcher catchpolla catechista cattle-man ceapman cellarman chamberlain chambermaid chandler chaperone charley charlie chaunter check-taker cheeseman chingler chippy chiropodist chirurgeon chronologist clark clayman clerk cleyman climber clipper clogger clothesman clothman coacher coachman coalman coastguard cobleman codder coleter coloratora colotier comber combere conder conner cooper copeman coper copperbeater copperbeter coppersmith cordwainer corviner corvisor cotiler cottager cottar cottier coupler cowkeeper coxswain crier crocker cropper crozier culver cuper currier cutler cutter daguerreotypist dairyman danter dealer deathsman delver deputy deviller dikeman dipper distiller diviner docker domesman dowser dragoon draper drawer dredger dredgerman dresser driver drowner drummer drysalter dubbere dustman dykeman earer eggler empresario engraver ensign equerry estafette exchequer expressman eyer fabricator fagetter faker falconer fanner farmer farrier fawkner featherman feller feroner ferreter ferrour fettler feuterer fewster filler finisher fireman fishdryver fitter flasher flesher fleshewer fletcher floater floatman flycoachman flyman fogler footman forgeman fossetmaker founder fowler fruiterer fugler fuller funambulist furbisher furrier gaffer gager galvaniser ganger garlekmonger garlicmonger gater gatherer gatherers gauger gavelkinder gaveller gavelman gelder gilder glasswright glover goldsmith grace-wife grainer graver grazier greave greensmith grieve grimbribber haberdasher hack hackman hammerman handwoman hanger hanger-on hankyman hatchler hawker haybinders hayward heaver heckler hedger hellier hender hensman herder hermit hewer higgler highwayman hillard hillier hodcarrier holer hoofer hooker-on hooper hopper horse-capper hosteller howdie howdy hoyman huckster hunter hurdleman husbandman iceman impresario infirmarian interfactor ironer jagger japanner jiggerman jobber jobmaster joiner jorman journeyman joyner keeker keelman keeper kempster kilner knacker knappers knitter knocker-upper lace-drawer lace-master lagger lamplighter lapper lardner lattener launderer layer leader leder leech legger leightonward lengthsman limer limner liner linkerman litster loder lorimer loriner luthier lyner maiden maker malster manager marbler mariner marshman mason melder mercer merchant meterer milesman milleress milliner miner mintmaker mintmaster mistress monger moulder mudlark mugseller muleteer mustardman nailora navigator neatherd nedeller nightman nipper noterer nurse officer oilman onsetter operator orderly ostler overlooker owler packer packman painter paintress panter paperer paritor pasteler paviour peeler pelterer perfumer pewterer phrenologist picker piecener piecer piker pilot pimpmaker pinder piper plaicher planker plasher plasterer platcher platelayer ploughman ploughwright plowman plumer pointer pointman poleman polisher porter poster potter powler preceptress proctor propman prostitute puddler puerfinder purefinder purser quarrier quarryman quilter quiltress rafter ranger ratoner rattener reddsman redesman redsman reedmaker reever render renovator revenuer ripper riveter rockman rover rugmaker runner rustler sandhog saucer sawbones sawyer screener scribe scrutineer searcher seedsman seller sempster sergeant sewster sheargrinder shearman sheepman sherman shifter shingler shipwright sidesman signalman silversmith sinker skeiner skelper skepperne skepprne skinner skipper slaper slaymaker slopseller smiddy smith snob soilman solicitor souter spalder spallier spinner spooner spurrer spurrier stabler stallman stapler statist steersman steward stoker storeman stripper surgeon surveyor sutler sweep swineherder tackler tailor tallier tanney tapicer taverner tawyer teinter tenter third-borough tiger tiller tinctor tinman tinter tirewoman tollie tolman tosher trapper trimmer troner trotter tubber tucker tunner turner turnspit tweeny twister twisterer ulnager unholsterer upholder usher valuator vatman venur victualer victualler vintager vinter wabster wagoner wainwright waiter waller walloper warder warper warrener watchman waterman way-maker weaver webster weighman wellmaster wharfinger whiffler whipmaker whipper whitener whitesmith whitester whitster windster winnower woolcomber woontcatcher worker wright'.split()),
        ]
    # pending, from http://medieval.stormthecastle.com/medieval-jobs.htm : ['metalsmith', 'stonemason', 'hedgewarder', 'guildsman', 'reeve', 'brewer', 'fishmonger', 'miller', 'baker', 'squire', 'carpenter', 'butcher', 'groom', 'grocer', 'armorer', 'craftsman', 'page']

    def features_from_tokens(self, tokens):
        features = []
        for f,lst in self.reference_data():
            tokens_ = [i.lemma for i in tokens if i.pos == f or i.pos == (f+'S')]
            s = set(lst)
            for i in tokens_:
                if i in s:
                    features.append(1.0)
                    break
            else:
                features.append(0.0)
        return features

class FeatureProviderNNTokens(FeatureProvider):
    def features_names(self):
        return [
            'relAnimal',
            'relThing',
            'relRoyal',
            'relMagic',
            'relHuman',
            'relPerson',
            'relPersonMale',
            'relPersonFemale',
            'relHumanCapabilities',
            'relHumanProperty',
            ]

    def features_from_tokens(self, tokens):
        features = set()
        for rel in util.flatten([networkcachemanager.conceptnet.query_expand(token) for token in
                                 voz.Token.filter(tokens, pos_list=['NN', 'NNS'])]):
            if rel[0] in ['isa'] and rel[1] in ['animal', 'pet']:
                features.add('relAnimal')

            if rel[0] in ['isa'] and rel[1] in ['food', 'tree', 'rock', 'stuff']:
                features.add('relThing')

            if rel[0] in ['isa', 'related'] and rel[1] in ['royal', 'monarchy', 'castle', 'palace', 'royalty',
                                                           'prince','queen', 'princess', 'king']:
                features.add('relRoyal')

            if rel[0] in ['isa', 'related'] and rel[1] in ['magical', 'magic', 'wiccan', 'pagan', 'wizard']:
                features.add('relMagic')

            if rel[0] in ['related'] and rel[1] in ['hand', 'eye', 'glasses', 'clothes', 'shoes', 'body']:
                features.add('relHuman')

            if rel[0] in ['isa'] and rel[1] in ['person']:
                features.add('relPerson')

            if rel[0] in ['isa'] and rel[1] in ['man', 'male', 'mr', 'boy', 'sir']:
                features.add('relPersonMale')

            if rel[0] in ['isa'] and rel[1] in ['woman', 'lady', 'girl', 'female', 'miss', 'mrs']:
                features.add('relPersonFemale')

            if rel[0] in ['can'] and rel[1] in ['laugh', 'walk', 'feel', 'love', 'speak', 'talk', 'learn', 'play',
                                                'live', 'think', 'read']:
                features.add('relHumanCapabilities')
            if rel[0] in ['partof', 'isa'] and rel[1] in ['person', 'human', 'being', 'sentient', 'intelligent',
                                                          'social']:
                features.add('relHumanProperty')
        return [1.0 if i in features else 0.0 for i in self.features_names()]

class FeatureProviderDependenciesM(FeatureProvider):
    @classmethod
    def features_names(self):
        features = []
        for i in dep_list:
            features.append('dep_head_' + i)
            features.append('dep_dep_' + i)
        return features

    @classmethod
    def features_from_dependencies(self, dep, sentence, feature_container):
        '''

        :param dep: dependencyhelper.Dependency
        :param sentence: voz.Sentence
        :param features: featuremanager.FeatureManager
        :return:
        '''
        features = {}
        dep_type = None
        if dep.type.startswith('prep_'):
            if dep.type[5:] in ['to', 'from', 'of', 'with', 'for']:
                dep_type = dep.type
            else:
                dep_type = 'prep_other'

        if dep.type in ['poss', 'appos']:
            dep_type = dep.type

        if dep.type in ['det']:
            if 'the' == dep.dependent.lemma:
                dep_type = 'det_the'
            else:
                dep_type = 'det_other'
        if dep.type in ['dobj', 'iobj', 'pobj']:
            dep_type = 'det_' + dep.type

        if dep.type in ['det']:
            return  # may add a dep to the head!

        governor = dep.governor.get_mention()
        if governor and dep_type and 'dep_head_'+dep_type in MentionFeatures.get_feature_names():
            dep_idx = MentionFeatures.get_feature_names().index('dep_head_'+dep_type)
            if dep_idx >= 0:
                feature_container._feature_managers['m'][governor.id][dep_idx] = 1.0

        dependent = dep.dependent.get_mention()
        if dep.type == 'appos':
            # TODO create a coreference link between these mentions?
            # dependent =
            # This code used to look for the "representative" among this mention's parent's children
            return


        if dep_type and dependent and 'dep_dep_'+dep_type in MentionFeatures.get_feature_names():
            dep_idx = MentionFeatures.get_feature_names().index('dep_dep_'+dep_type)
            if dep_idx >= 0:
                feature_container._feature_managers['m'][dependent.id][dep_idx] = 1.0


class FeatureProviderVerbsM(FeatureProvider):
    @classmethod
    def features_names(self):
        features = []
        for group in verbs_groups:
            features.append('v-subject-g-' + group[0])
            features.append('v-object-g-' + group[0])
        for verb in verbs_list:
            features.append('v-subject-' + verb)
            features.append('v-object-' + verb)
        return features

    @classmethod
    def features_from_verbs(self, verb, sentence, feature_container):
        if not verb.get_subjects() and not verb.get_objects(): return

        cloud_synsets = nltkhelper.nltk_wordnet.query(verb.token.lemma,pos=nltkhelper.wn.VERB)
        for group in verbs_groups:
            cloud_ref = []
            for j in group:
                synsets = nltkhelper.nltk_wordnet.query(j, pos=nltkhelper.wn.VERB)
                if synsets:
                    cloud_ref.append(synsets[0])
            if cloud_synsets and cloud_ref:
                similarity = max([max([i.wup_similarity(j) for j in cloud_synsets]) for i in cloud_ref])
                if verb.get_subjects():
                    feature_name = 'v-subject-g-' + group[0]
                    feature_idx = MentionFeatures.get_feature_names().index(feature_name)
                    matches = []
                    verbmanager.add_children_mentions_to_list(verb.get_subjects(), matches)
                    for mention in matches:
                        if not mention.id in feature_container._feature_managers['m']: continue
                        feature_container._feature_managers['m'][mention.id][feature_idx] = max(
                            feature_container._feature_managers['m'][mention.id][feature_idx],
                            similarity
                        )
                if verb.get_objects():
                    feature_name = 'v-object-g-' + group[0]
                    feature_idx = MentionFeatures.get_feature_names().index(feature_name)
                    matches = []
                    verbmanager.add_children_mentions_to_list(verb.get_objects(), matches)
                    for mention in matches:
                        if not mention.id in feature_container._feature_managers['m']: continue
                        feature_container._feature_managers['m'][mention.id][feature_idx] = max(
                            feature_container._feature_managers['m'][mention.id][feature_idx],
                            similarity
                        )
        if verb.token.lemma in verbs_list:
            matches = []
            verbmanager.add_children_mentions_to_list(verb.get_subjects(), matches)
            for mention in matches:
                if not mention.id in feature_container._feature_managers['m']: continue
                feature_name = 'v-subject-' + verb.token.lemma
                feature_idx = MentionFeatures.get_feature_names().index(feature_name)
                feature_container._feature_managers['m'][mention.id][feature_idx] = max(
                    feature_container._feature_managers['m'][mention.id][feature_idx],
                    similarity
                )
            matches = []
            verbmanager.add_children_mentions_to_list(verb.get_objects(), matches)
            for mention in matches:
                if not mention.id in feature_container._feature_managers['m']: continue
                feature_name = 'v-object-' + verb.token.lemma
                feature_idx = MentionFeatures.get_feature_names().index(feature_name)
                feature_container._feature_managers['m'][mention.id][feature_idx] = max(
                    feature_container._feature_managers['m'][mention.id][feature_idx],
                    similarity
                )


class FeatureProviderVerbs(FeatureProvider):
    def features_names(self):
        return [
            ]

    def features_from_verbs(self, verb):
        pass
