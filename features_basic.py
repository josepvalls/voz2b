

# trash

'''

class FeatureManager(object):
    def __init__(self, entity):
        self.entity = entity
        self.features = []
        self._feature_keys = None


    def features_filtered(self):
        return sorted([i for i in self.features if i.value], key=lambda f: f.order())
        return [i for i in self.features if i.value]


    def add(self, feature):
        self.features.append(feature)

    def feature_keys(self):
        if not self._feature_keys:
            self._feature_keys = set([i.key() for i in self.features])
        return self._feature_keys


    def getVector(self, retrieve=None):
        vector = []
        def getValue(featureN, label="Class", default='?'):
            feature_name = '%s-%s' % (featureN.__name__, label)
            # values = [i for i in self.features if i.__class__==featureN and (not label or i.label==label)]
            values = [i for i in self.features if i.name() == feature_name]
            if not values:
                vector.append((feature_name, default))
                return
            if featureN.is_numeric:
                value = sum([i.value for i in values]) / len(values)
            else:
                value = reduce(featureN.merge, values)
                value = value.value
            if featureN == FeatureLabeledList:
                s = ';'.join(value)
                vector.append((feature_name, (s if s else '-')))
            else:
                vector.append((feature_name, value))
                # if value.value > 0: vector.append('1')
                # elif value.value < 0: vector.append('0')
                # else: vector.append('?')

        def getValueList(feature, label=None):
            values = [i for i in self.features if i.__class__ == feature and (not label or i.label == label)]
            if not values:
                return []
            value = reduce(feature.merge, values)
            return value.value

            # FeatureLabel
        current_features = []
        # FeatureProperNames
        current_features += [FeatureProperNamesMale, FeatureProperNamesFemale]
        # FeatureCommonNames
        current_features += [FeatureVerbSubject, FeatureVerbObject]
        # current_features+=[FeatureVerbSubject,FeatureVerbObject,FeatureVerbSubjectObject]
        for i in current_features:
            getValue(i)
        for i in dep_list:
            getValue(FeatureLabel, 'dep_head_' + i, '0')
            getValue(FeatureLabel, 'dep_dep_' + i, '0')

        vsubject = getValueList(FeatureLabeledList, 'v-subject')
        vobject = getValueList(FeatureLabeledList, 'v-object')
        global wn
        if not wn:
            from nltk.corpus import wordnet as wn
        cloud_s = []
        for i in vsubject:
            synsets = wn.synsets(i, pos=wn.VERB)
            if synsets:
                cloud_s.append(synsets[0])
        cloud_o = []
        for i in vobject:
            synsets = wn.synsets(i, pos=wn.VERB)
            if synsets:
                cloud_o.append(synsets[0])
        for group in verbs_groups:
            cloud_a = []
            for j in group:
                synsets = wn.synsets(j, pos=wn.VERB)
                if synsets:
                    cloud_a.append(synsets[0])
            if not cloud_s or not cloud_a:
                vector.append(('v-subject-g-' + group[0], 0))
            else:
                similarity = max([max([i.wup_similarity(j) for j in cloud_s]) for i in cloud_a])
                vector.append(('v-subject-g-' + group[0], similarity))
            if not cloud_o or not cloud_a:
                vector.append(('v-object-g-' + group[0], 0))
            else:
                similarity = max([max([i.wup_similarity(j) for j in cloud_o]) for i in cloud_a])
                vector.append(('v-object-g-' + group[0], similarity))

        for i in verbs_list:
            vector.append(('v-subject-' + i, ('1' if i in vsubject else '0')))
        for i in verbs_list:
            vector.append(('v-object-' + i, ('1' if i in vobject else '0')))
        if not retrieve:
            return vector

    def getVerbs(self, slot):
        return self.entity.verbs(slot)

    def getVectorNew(self, get_labels_too=False):
        vector = []

        def getValue(featureN, label="Class", default='?'):
            feature_name = '%s-%s' % (featureN.__name__, label)
            values = [i for i in self.features if i.name() == feature_name]
            if not values:
                vector.append((feature_name, default))
                return
            if featureN.is_numeric:
                value = sum([i.value for i in values]) / len(values)
            else:
                value = reduce(featureN.merge, values)
                value = value.value
            if featureN == FeatureLabeledList:
                s = ';'.join(value)
                vector.append((feature_name, (s if s else '-')))
            else:
                vector.append((feature_name, value))

        # FeatureLabel
        for i in ['hasPRP', 'hasNN', 'hasNNP', 'relAnimal', 'relRoyal', 'relMagic', 'relHuman', 'relPerson',
                  'relPersonMale', 'relPersonFemale', 'relHumanCapabilities', 'relHumanProperty']:
            getValue(FeatureLabel, i)
        for i in ['hasPOS', 'hasEX', 'hasDT', 'hasJJ', 'hasIt', 'hasWDT', 'hasRB', 'hasSingularFirstPerson ',
                  'hasPluralFirstPerson', 'hasSingularSecondPerson ', 'hasPluralSecondPerson ',
                  'hasSingularThirdPersonMales', 'hasSingularThirdPersonFemale ', 'hasSingularThirdPersonNeuter ',
                  'hasSingularThirdPersonGeneric ', 'hasPluralThirdPerson', 'hasPluralThirdPersonGeneric']:
            getValue(FeatureLabel, i)
        for i in ['S', 'VP', 'NP', 'ADJP', 'PP', 'ADVP', 'NP-TMP', 'SQ', 'SBAR', 'SINV', 'SBARQ']:
            getValue(FeatureLabel, 'fromNode' + i)
        for i in ['--', 'ADJP', 'ADVP', 'CC', 'CD', 'DT', 'EX', 'IN', 'JJ', 'JJR', 'JJS', 'NN', 'NNP', 'NNS', 'NP',
                  'PDT', 'POS', 'PP', 'PRP', 'PRP$', 'QP', 'RB', 'VBG', 'VBN', 'WDT', 'WP']:
            getValue(FeatureLabel, 'hasNode' + i)
        current_features = []
        # FeatureConceptSimilarity
        current_features += [FeatureAgeYoung, FeatureAgeOld, FeatureVillian, FeatureAnimal, FeaturePerson,
                             FeatureLocation, FeatureVehicle, FeatureCreature]
        # FeatureProperNames
        current_features += [FeatureProperNamesMale, FeatureProperNamesFemale]
        # FeatureCommonNames
        current_features += [FeatureCommonNamesMale, FeatureCommonNamesFemale, FeatureCommonNamesPersons,
                             FeatureCommonNamesTitles]
        current_features += [FeatureVerbSubject, FeatureVerbObject]
        # current_features+=[FeatureVerbSubject,FeatureVerbObject,FeatureVerbSubjectObject]
        for i in current_features:
            getValue(i)
        for i in ['prep_to', 'prep_from', 'prep_of', 'prep_with', 'prep_for', 'prep_in_on_at', 'prep_other'] + ['poss',
                                                                                                                'appos'] + [
            'det_the', 'det_other'] + ['dobj', 'iobj', 'pobj']:
            getValue(FeatureLabel, 'dep_head_' + i, '0')
            getValue(FeatureLabel, 'dep_dep_' + i, '0')

        vsubject = self.getVerbs(0)
        vobject = self.getVerbs(1)
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
        global wn
        if not wn:
            from nltk.corpus import wordnet as wn
        cloud_s = []
        for i in vsubject:
            synsets = wn.synsets(i, pos=wn.VERB)
            if synsets:
                cloud_s.append(synsets[0])
        cloud_o = []
        for i in vobject:
            synsets = wn.synsets(i, pos=wn.VERB)
            if synsets:
                cloud_o.append(synsets[0])
        for group in verbs_groups:
            cloud_a = []
            for j in group:
                synsets = wn.synsets(j, pos=wn.VERB)
                if synsets:
                    cloud_a.append(synsets[0])
            if not cloud_s or not cloud_a:
                vector.append(('v-subject-g-' + group[0], 0))
            else:
                similarity = max([max([i.wup_similarity(j) for j in cloud_s]) for i in cloud_a])
                vector.append(('v-subject-g-' + group[0], similarity))
            if not cloud_o or not cloud_a:
                vector.append(('v-object-g-' + group[0], 0))
            else:
                similarity = max([max([i.wup_similarity(j) for j in cloud_o]) for i in cloud_a])
                vector.append(('v-object-g-' + group[0], similarity))
        verbs_list = ['be', 'have', 'do', 'go', 'want', 'make', 'give', 'call', 'take', 'tell', 'get', 'know', 'ask',
                      'come', 'look', 'happen', 'put', 'say', 'leave', 'become', 'think', 'run', 'sit', 'fall', 'bring',
                      'see', 'hear', 'stand', 'pass', 'seem', 'find']
        for i in verbs_list:
            vector.append(('v-subject-' + i, ('1' if i in vsubject else '0')))
        for i in verbs_list:
            vector.append(('v-object-' + i, ('1' if i in vobject else '0')))
        if get_labels_too:
            return vector
        else:
            return [i[1] for i in vector]


################################################################################




class FeatureProviderSentiWordnet(FeatureProvider):
    @classmethod
    def ready(self, feature_manager):
        return 'sentiwordnet' in datamanager.__dict__

    def features(self):
        return [FeatureSentimentPosNeg]

    def features_from_tokens(self, tokens):
        features = []
        for feature_class in self.features():
            sentiment = sum(
                [datamanager.sentiwordnet.query(leaf.lemma, leaf.pos) * leaf.weight for leaf in tokens]) / len(tokens)
            sentiment = util.clamp(-1, 1, sentiment)
            features.append(feature_class(sentiment, 1.0, self.__class__.__name__))
        return features



class FeatureProviderFromVerbs(FeatureProvider):
    @classmethod
    # FIXME feature_manager is None for some entities, probably related to hierarchies
    def features_from_verbs(cls, verb):
        if settings.DISABLE_FEATURES_HIERARCHICAL_PROPAGATION_VERBS:
            if verb.subject and verb.subject.feature_manager:
                verb.subject.feature_manager.add(FeatureVerbSubjectObject(1.0, 1.0, cls.__name__))
                verb.subject.feature_manager.add(FeatureVerbSubject(1.0, 1.0, cls.__name__))
                verb.subject.feature_manager.add(FeatureLabeledList('v-subject', [verb.verb], cls.__name__))
            if verb.object and verb.object.feature_manager:
                verb.object.feature_manager.add(FeatureVerbSubjectObject(-1.0, 1.0, cls.__name__))
                verb.object.feature_manager.add(FeatureVerbObject(1.0, 1.0, cls.__name__))
                verb.object.feature_manager.add(FeatureLabeledList('v-object', [verb.verb], cls.__name__))
        else:
            if verb.subject:
                for e in verb.subject.entity_manager.entities_independent(verb.subject):
                    if e and e.feature_manager:  # FIXME: when is there no feature_manager???
                        e.feature_manager.add(FeatureVerbSubjectObject(1.0, 1.0, cls.__name__))
                        e.feature_manager.add(FeatureVerbSubject(1.0, 1.0, cls.__name__))
                        e.feature_manager.add(FeatureLabeledList('v-subject', [verb.verb], cls.__name__))
            if verb.object:
                for e in verb.object.entity_manager.entities_independent(verb.object):
                    if e and e.feature_manager:
                        e.feature_manager.add(FeatureVerbSubjectObject(-1.0, 1.0, cls.__name__))
                        e.feature_manager.add(FeatureVerbObject(1.0, 1.0, cls.__name__))
                        e.feature_manager.add(FeatureLabeledList('v-object', [verb.verb], cls.__name__))






################################################################################

class Feature(object):
    def __init__(self, value=None, confidence=0.0, provider=None):
        self.value, self.confidence, self.provider = value, confidence, provider

    def __repr__(self):
        return 'Feature %s: %s (%s %s)' % (self.__class__.__name__, self.value, self.confidence, self.provider)

    def name(self):
        return '%s-%s' % (self.__class__.__name__, "Class")

    def key(self):
        return '%s-%s' % (self.__class__.__name__, self.provider)

    def html(self):
        return '<em>%s:</em> %s <small>(%.2f %s)</small>' % (
        self.__class__.__name__, '%.2f' % self.value if self.value else 'N/A', self.confidence, self.provider)

    @staticmethod
    def is_numeric():
        return True

    @staticmethod
    def merge(self, other):
        if self.value == other.value:
            # TODO if not self.provider == other.provider: new feature with unknown or merging provider
            return self
        else:
            return Feature(0.0)



class FeatureLabeledList(FeatureLabel):
    def __init__(self, label=None, value=[], confidence=1.0, provider=None):
        self.label, self.value, self.confidence, self.provider = label, value, confidence, provider

    def html(self):
        return '<em>%s:</em> %s' % (self.label, '; '.join([str(i) for i in self.value]))

    @staticmethod
    def is_numeric():
        return False

    @staticmethod
    def merge(self, other):
        self.value = list(set(self.value) | set(other.value))
        return self




class FeatureSentimentPosNeg(Feature):
    pass


class FeatureEnum(object):
    PERSON = 'person'
    NUMBER = 'number'
    GENDER = 'gender'


class FeatureTokenLabel(Feature):
    def __init__(self, label=None, token=None, confidence=1.0, provider=None):
        self.label, self.token, self.confidence, self.provider = label, token, confidence, provider
        self.value = self.token.lemma

    def html(self):
        return '<em>%s:</em> %s' % (self.label, self.token.lemma) + (
        '<small> %.2f</small>' % self.token.weight if self.token.weight < 0 else '')


class FeatureTokenLabelValue(FeatureTokenLabel):
    def __init__(self, label=None, value=None, token=None, confidence=1.0, provider=None):
        self.label, self.value, self.token, self.confidence, self.provider = label, value, token, confidence, provider

    def html(self):
        return '<em>%s:</em> %s' % (self.label, self.value) + (
        '<small> %.2f</small>' % self.token.weight if self.token.weight < 0 else '')


# This class is only used for information purposes DISABLE_FEATURES_INFORMATION
class FeatureWN(FeatureTokenLabelValue):
    @classmethod
    def attributes(cls, tokens, provider):
        attributes = []
        for token in tokens:
            for attribute in datamanager.nltk_wordnet.query_expand(token):
                if attribute[0] == 'desc':
                    attributes.append(FeatureTokenLabelValue(attribute[0], attribute[1], token, confidence=1.0,
                                                             provider=provider) if isinstance(attribute[1],
                                                                                              str) else cls(
                        attribute[0], attribute[1], token))
                else:
                    attributes.append(
                        cls(attribute[0], attribute[1], token, confidence=1.0, provider=provider) if isinstance(
                            attribute[1], str) else cls(attribute[0], attribute[1], token))
        return attributes

    def html(self):
        return '<em>%s:</em> [%s]' % (self.label, ', '.join(self.value.lemma_names)) + (
        '<small> %.2f</small>' % self.token.weight if self.token.weight < 0 else '')


# This class is only used for information purposes DISABLE_FEATURES_INFORMATION
class FeatureCN(FeatureTokenLabelValue):
    @classmethod
    def attributes(cls, tokens, provider):
        attributes = []
        for token in tokens:
            for attribute in datamanager.conceptnet.query_expand(token):
                attributes.append(cls(attribute[0], attribute[1], token, confidence=1.0, provider=provider))
        return attributes
'''