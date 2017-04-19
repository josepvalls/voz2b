import verbmanager,entitymanager
import re

def normalize_string(s):
    return re.sub('[^a-z]','',s.lower())
def normalize_string_spacing(s):
    return re.sub('[\n\s\r]','',s.lower())

def normalize_string_alphapunkt(s):
    return re.sub('[^a-z\.\,\:\;]','',s.lower())


VERBS_EXPRESS = {u'consider', u'proposition', u'preface', u'debate', u'vowelise', u'scold', u'decry', u'add', u'berate', u'announce', u'dispute', u'babble', u'advocate', u'denounce', u'stipulate', u'mention', u'convoke', u'discuss', u'insert', u'cry', u'ordain', u'proclaim', u'compliment', u'summarise', u'preach', u'attest', u'pledge', u'bark', u'enounce', u'troll', u'accurse', u'decree', u'curse', u'defend', u'testify', u'state', u'hiss', u'lecture', u'salute', u'orchestrate', u'sing', u'remark', u'assure', u'swear', u'boo', u'argue', u'coo', u'comment', u'mumble', u'vocalize', u'shout', u'indicate', u'attack', u'call', u'recommend', u'cite', u'invite', u'convene', u'apologise', u'exclaim', u'warn', u'vouch', u'observe', u'hold', u'specify', u'cast', u'posit', u'voice', u'declare', u'bet', u'claim', u'provoke', u'allege', u'condemn', u'suggest', u'note', u'read', u'discredit', u'advise', u'opine', u'orate', u'urge', u'confess', u'quote', u'scald', u'censure', u'abuse', u'retroflex', u'slang', u'sass', u'scream', u'repay', u'command', u'yell', u'order'}
VERBS_SAY = {u'say', u'tell', u'talk'}
VERBS_THINK = {u'suppose', u'recall', u'draw', u'postulate', u'wonder', u'fawn', u'consider', u'generalize', u'focus',
               u'ruminate', u'conjure', u'disoblige', u'calculate', u'theorise', u'intonate', u'coconspire',
               u'conjecture', u'rise', u'induce', u'foretell', u'conceive', u'laud', u'contemplate', u'entertain',
               u'theorize', u'study', u'plot', u'rationalise', u'predict', u'doubt', u'recall', u'process', u'reflect',
               u'reassess', u'trifle', u'derive', u'hypothesize', u'hypothesise', u'reason', u'formalize', u'think',
               u'scheme', u'postulate', u'ponder', u'ratiocinate', u'refute', u'reckon', u'wonder', u'raise',
               u'dismiss', u'understand', u'suppose', u'rethink', u'worry', u'underestimate', u'infer', u'solve',
               u'evaluate', u'deduct', u'deduce', u'recollect', u'assess', u'approximate', u'plunge', u'regard',
               u'compute', u'recognize', u'examine', u'reconsider', u'assume', u'philosophize'}
VERBS_ASK = {u'survey', u'bait', u'demand', u'query', u'dare', u'solicit', u'propose', u'bid', u'summon', u'entice', u'direct', u'agitate', u'allure', u'expect', u'enjoin', u'sue', u'interpellate', u'incite', u'appeal', u'instigate',  u'ask', u'interrogate', u'tempt', u'raise', u'question', u'quiz', u'offer', u'interdict', u'impeach', u'consult', u'grill',u'inquire', u'pry',u'compel', u'requisition', u'enquire',  u'test',  u'defy',  u'request', u'challenge',  u'propose', u'question',}
VERBS_REPLY = {u'respond', u'return', u'counter', u'retort', u'answer', u'reply',u'protest',  u'acknowledge',}
VERBS_EXPRESS |= VERBS_SAY
VERBS_EXPRESS |= VERBS_THINK
VERBS_EXPRESS |= VERBS_ASK
VERBS_EXPRESS |= VERBS_REPLY

VERBS_EXPRESS = {'say','ask','answer','think','reply','cry','call','tell','beg','yell'}
VERBS_SAY ={'say'}
VERBS_ASK = {'ask', 'question'}
VERBS_REPLY = {'answer','reply'}
VERBS_THINK = {'think'}

RULE_LANGUAGE = {
    'q~': 'Q', # unfinished quote
    'q!':'Q', # finished quote
    'Q': '*', # quote
    'S': 'E', # say
    'A': 'E', # ask
    'T': 'E', # think
    'R': 'E', # reply
    'E': 'V', # express
    'V': 'V', # verb
    'P': '*', # person (character mention)
}

FOLLOWUP_RULE = '_'


class TokenizedToken(object):
    def __init__(self):
        self.matched_rules = []
    def add_matched_rules(self, token, rule):
        self.matched_rules.append(rule)

class EmptyTokenizedToken(TokenizedToken):
    def __init__(self, placeholder):
        self.placeholder = placeholder

class Punctuation(TokenizedToken):
    def __init__(self, endp):
        TokenizedToken.__init__(self)
        self.endp = endp

class Quote(TokenizedToken):
    def __init__(self, offset, offset_end, doc):
        self.offset = offset
        self.offset_end = offset_end
        self.doc = doc
        self.speaker_mention = None
        self.listener_mention = None
        self.speaker_mentions = {}
        self.listener_mentions = {}
        self.annotations = None # type voz.SentenceLevelQuotedAnnotations
        self.endp = None
        self._text = None
    def __str__(self):
        if self.speaker_mention:
            speaker = '%s (%s)' % (self.speaker_mention.get_text(),self.speaker_mention.get_most_likely_symbol())
        else:
            speaker = '?'
        if self.listener_mention:
            listener = '%s (%s)' % (self.listener_mention.get_text(),self.listener_mention.get_most_likely_symbol())
        else:
            listener = '?'
        ret = speaker + '>' + listener + ': ' + self.get_text()
        if self.annotations:
            ret += '\n\t(%s)' % self.annotations
        return ret
    def get_text(self):
        return self._text or self.doc.get_text()[self.offset:self.offset_end].replace('\n', ' ').replace('  ', ' ')
    def set_speaker_mention(self, mention, rule):
        # rules starting with _ are the followup rules
        if rule.startswith('_') or rule=='aggregated3':
            if mention:
                self.speaker_mention = mention
                self.speaker_mentions[rule] = mention
        else:
            self.speaker_mentions[rule] = mention
    def set_listener_mention(self, mention, rule):
        if rule.startswith('_') or rule=='aggregated3':
            if mention:
                self.listener_mention = mention
                self.listener_mentions[rule] = mention
        else:
            self.listener_mentions[rule] = mention
    def get_speaker_mention(self):
        return self.speaker_mention
    def get_listener_mention(self):
        return self.listener_mention


def token_to_string(token, verbose=0):
    ret = ''
    if isinstance(token, entitymanager.Mention):
        ret += 'P'
        if verbose > 0:
            ret += '(%s)' % token.get_most_likely_symbol()
    # elif isinstance(token, Quote): ret += 'Q'
    elif isinstance(token, Quote):
        if token.endp[-1] not in ['.', '!', '?']:
            ret += 'q~'
        else:
            ret += 'q!'
        if verbose > 0:
            ret += '(%s)' % ((token.annotations.speaker_annotation or 'NONE').replace(' ', '') + '/' + (
            token.annotations.listener_annotation or 'NONE').replace(' ', ''))
        if verbose > 1:
            if not token.speaker_mentions:
                ret += '?s'
            if not token.listener_mentions:
                ret += '?l'
            ret += '(%s)' % token.get_text()

    # elif isinstance(token, Quote) and token.endp[-1] not in ['.','!','?']: ret += 'q~' #+ token.endp
    # elif isinstance(token, Quote) and token.endp[-1] in ['.','!','?']: ret += 'q!' #+ token.endp
    elif isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_SAY:
        ret += 'S'
    elif isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_ASK:
        ret += 'A'
    elif isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_THINK:
        ret += 'T'
    elif isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_REPLY:
        ret += 'R'
    elif isinstance(token, verbmanager.Verb) and token.token.lemma in ['say', 'ask', 'answer', 'think', 'reply']:
        ret += 'E'
    elif isinstance(token, verbmanager.Verb):
        ret += 'V'
        if verbose:
            ret += "(%s)" % token.token.lemma
    elif isinstance(token, Punctuation):
        ret += token.endp
    return ret

def tokenized_string_to_string(output,verbose=0):
    ret = ''
    for token in output:
        ret += token_to_string(token,verbose)
        ret += ' '
    return ret

def print_quoted_speech_tuple(input_tuple):
    output, quotes,mentions,verbs = input_tuple
    # sanity
    print 'QUOTES'
    for i,quote in enumerate(quotes):
        print i,quote
    print 'TOKENIZED STRING'
    print "TOKENIZED-0",tokenized_string_to_string(output,0)
    print "TOKENIZED-1",tokenized_string_to_string(output,1)
    print "TOKENIZED-2",tokenized_string_to_string(output,2)