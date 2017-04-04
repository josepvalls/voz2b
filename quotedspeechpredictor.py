import vozbase
import voz
import quotedspeechhelper
import styhelper
import settings
import logging
import entitymanager
import verbmanager
import sys
logger = logging.getLogger(__name__)
import random,collections
import copy
import util
import utterancegenerator

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
        return self.doc.get_text()[self.offset:self.offset_end].replace('\n', ' ').replace('  ', ' ')
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


def tokenize_document(doc, verbose=False):
    assert isinstance(doc,voz.Document)
    quotes = [] #type: list[Quote]
    mentions = [] #type: list[entitymanager.Mention]
    verbs = [] #type: list[verbmanager.Verb]
    # parse
    in_quotes_offset = None
    output = []
    input = doc.get_all_tokens(pair_container=True)
    ignore_tokens = set()
    while input:
        sentence, token = input.pop(0)
        assert isinstance(sentence,voz.Sentence)
        assert isinstance(token,voz.Token)

        if in_quotes_offset is None and token.text[-1] in ['.',':']:
            output.append(Punctuation(token.text[-1]))
        elif in_quotes_offset is None and token.text!='``':
            # decide what to do with tokens outside of quotes
            if not token in ignore_tokens:
                mentions_ = filter(None,doc.get_mentions_by_token_id(token.id))
                mentions_ = util.flatten([i.get_all_children_recursively() for i in mentions_])
                for mention in set(mentions_):
                    if mention and mention.is_independent:
                        if mention.get_most_likely_symbol():
                            output.append(mention)
                            mentions.append(mention)
                        ignore_tokens.update(mention.tokens)
                verb = doc.get_verb_by_token_id(token.id)
                if verb:
                    # TODO whitelist only expression verbs?
                    verbs.append(verb)
                    output.append(verb)
        elif in_quotes_offset is None and token.text=='``':
            in_quotes_offset = token.offset
        elif in_quotes_offset is not None and token.text=="''":
            quote = Quote(in_quotes_offset,token.offset+token.len, doc)
            quote.endp = token.get_previous().text
            quotes.append(quote)
            output.append(quote)
            if sentence.annotations and sentence.annotations.speech_data:
                quote.annotations = sentence.annotations.get_speech_annotations_for_quote(quote)
            else:
                logger.warning("NO ANNOTATION DATA FOR SENTENCE")
            in_quotes_offset = None

    return output,quotes,mentions,verbs

def predict_quoted_speech_informed(input_tuple, overwrite_mode_default=True, rule_name='baseline'):
    output, quotes,mentions,verbs = input_tuple
    symbol_to_mention = collections.defaultdict(list)
    for mention in mentions:
        symbol = mention.get_most_likely_symbol()
        if symbol:
            symbol_to_mention[symbol].append(mention)
    s = sorted([(len(v),k,v) for k,v in symbol_to_mention.items()],reverse=True)
    a = s[0][2][0]
    b = s[1][2][0]
    for quote in quotes:
        if overwrite_mode_default:
            quote.set_speaker_mention(a,rule_name)
            quote.set_listener_mention(b,rule_name)
        else:
            if not quote.speaker_mention:
                quote.speaker_mention = a
            if not quote.listener_mention:
                quote.listener_mention = b
    return output, quotes,mentions,verbs

def weighted_assignment(input_tuple,weights):
    output, quotes, mentions, verbs = input_tuple
    for quote in quotes:
        quote.speaker_mention = None
        quote.listener_mention = None
        if quote.speaker_mentions:
            quote.speaker_mention = sorted(quote.speaker_mentions.items(),key=lambda i:weights[i[0]][0])[-1][1]
        if quote.listener_mentions:
            quote.listener_mention = sorted(quote.listener_mentions.items(), key=lambda i: weights[i[0]][1])[-1][1]



def eval_quoted_speech(input_tuple,rule=None, laplace = 0, verbose=False):
    output, quotes,mentions,verbs = input_tuple
    s_ms = laplace # matching
    s_ml = laplace
    s_ts = laplace * 2 # total checked
    s_tl = laplace * 2
    s_cs = 0 # correct annotations
    s_cl = 0
    s_os = 0 # only rule
    s_ol = 0
    s_as = 0 # agreement
    s_al = 0
    s_ns = 0 # conflict
    s_nl = 0
    for quote in quotes:
        if not rule:
            pass
            '''if quote.speaker_mentions:
                quote.speaker_mention = quote.speaker_mentions.values()[0]
            if quote.listener_mentions:
                quote.listener_mention = quote.listener_mentions.values()[0]'''
        else:
            quote.speaker_mention = None
            quote.listener_mention = None
            if quote.speaker_mentions and rule in quote.speaker_mentions:
                quote.speaker_mention = quote.speaker_mentions[rule]
                if len(quote.speaker_mentions)==1:
                    s_os +=1
                else:
                    s_as = quote.speaker_mentions.values().count(quote.speaker_mention) - 1
                    s_ns = len(quote.speaker_mentions.values()) - s_as - 1

            if quote.listener_mentions and rule in quote.listener_mentions:
                quote.listener_mention = quote.listener_mentions[rule]
                if len(quote.speaker_mentions) == 1:
                    s_ol += 1
                else:
                    s_al = quote.speaker_mentions.values().count(quote.speaker_mention) - 1
                    s_nl = len(quote.speaker_mentions.values()) - s_al - 1

        if quote.annotations.speaker_annotation and ',' not in quote.annotations.speaker_annotation:
            s_cs +=1
            if quote.speaker_mention:
                s_ts +=1
                if quote.speaker_mention.get_most_likely_symbol() == quote.annotations.speaker_annotation:
                    s_ms += 1
                else:
                    logger.info('WRONG SPEAKER ASSIGNMENT %d, %s' % (quote.doc.id, str(quote)))
        else:
            logger.info('WRONG ANNOTATION IN SPEAKER QUOTE %d, %s' % (quote.doc.id, str(quote)))

        if quote.annotations.listener_annotation and ',' not in quote.annotations.listener_annotation:
            s_cl += 1
            if quote.listener_mention:
                s_tl +=1
                if quote.listener_mention.get_most_likely_symbol() == quote.annotations.listener_annotation:
                    s_ml += 1
                else:
                    logger.info('WRONG LISTENER ASSIGNMENT %d, %s' % (quote.doc.id, str(quote)))
        else:
            pass
            logger.info('WRONG ANNOTATION IN LISTENER QUOTE %d, %s' % (quote.doc.id, str(quote)))

        if verbose:
            print quote.speaker_mention.get_most_likely_symbol() if quote.speaker_mention else '?',quote.annotations.speaker_annotation,'>', quote.listener_mention.get_most_likely_symbol() if quote.listener_mention else '?', quote.annotations.listener_annotation
    return s_ms, s_ml, s_ts, s_tl, s_cs, s_cl, len(quotes), s_os, s_as, s_ns, s_ol, s_al, s_nl

def print_eval_quoted_speech(input_tuple,verbose=False):
    s_ms, s_ml, s_ts, s_tl, s_cs, s_cl, len_quotes, s_os, s_as, s_ns, s_ol, s_al, s_nl = input_tuple
    print 'ACCURACY (SPEAKER,LISTENER,TOTAL):',1.0*s_ms/s_ts if s_ts else 0.0,1.0*s_ml/s_tl if s_tl else 0.0,0.5*(s_ms+s_ml)/s_ts if s_ts else 0.0
    print 'COVERAGE (SPEAKER,LISTENER,TOTAL):', 1.0 * s_ts / s_cs, 1.0 * s_tl/s_cl, 1.0 * (s_ts + s_tl) / (s_cs+ s_cl), 'SKIPPED', 1.0*(len_quotes - s_cs)/len_quotes, 1.0*(len_quotes - s_cl)/len_quotes

def compute_eval_quoted_speech(input_tuple,verbose=False):
    s_ms, s_ml, s_ts, s_tl, s_cs, s_cl, len_quotes, s_os, s_as, s_ns, s_ol, s_al, s_nl = input_tuple
    return 'ACCURACY',1.0*s_ms/s_ts if s_ts else 0.0,1.0*s_ml/s_tl if s_tl else 0.0,0.5*(s_ms+s_ml)/s_ts if s_ts else 0.0,'COVERAGE',1.0 * s_ts / s_cs, 1.0 * s_tl/s_cl, 1.0 * (s_ts + s_tl) / (s_cs+ s_cl),'SKIPPED', 1.0*(len_quotes - s_cs)/len_quotes, 1.0*(len_quotes - s_cl)/len_quotes, 'CONFLICTS',s_os, s_as, s_ns, s_ol, s_al, s_nl, 'MATCHES', s_ms, s_ml

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
    print "TOKENIZED",tokenized_string_to_string(output,False)
    print "TOKENIZED",tokenized_string_to_string(output,True)


FOLLOWUP_RULE = '_'

class QuotedSpeechPredictorRule(object):
    def __init__(self, pattern, actions, rule_type=None):
        self.pattern = pattern
        self.actions = actions
        self.rule_type = rule_type
    @classmethod
    def from_string(cls, s, t=None):
        logger.info("Loading rule: %s" % s )
        pattern,actions = s.split('>')
        pattern = pattern.strip().split()
        actions = actions.strip().split()
        actions = [i.replace('=','.').split('.') for i in actions]
        return cls(pattern, actions, t)
    @classmethod
    def format_action(cls, action):
        if len(action) == 3:
            return '%s.%s=%s' % tuple(action)
        elif len(action) == 4:
            return '%s.%s=%s.%s' % tuple(action)
        else:
            return 'ERR'

    def __str__(self):
        return (self.rule_type or '') + ' '.join(self.pattern) + ' > ' + ' '.join([QuotedSpeechPredictorRule.format_action(i) for i in self.actions])

class QuotedSpeechMatcher(object):
    property_setters = {'s':'set_speaker_mention','l':'set_listener_mention','m':'add_matched_rules'}
    property_getters = {'s': 'get_speaker_mention', 'l': 'get_listener_mention'}
    def __init__(self, rule, tokens = None):
        self.rule = rule
        self.tokens = tokens or []
    def clone(self):
        return QuotedSpeechMatcher(self.rule,self.tokens)
    @classmethod
    def match(cls, token, target):
        t = target[0]
        if t=='Q' and isinstance(token, Quote): return True
        if t=='P' and isinstance(token, entitymanager.Mention): return True
        if t=='E' and isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_EXPRESS: return True
        if t=='S' and isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_SAY: return True
        if t=='A' and isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_ASK: return True
        if t=='R' and isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_REPLY: return True
        if t=='T' and isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_THINK: return True
        if t=='V' and isinstance(token, verbmanager.Verb) : return True
        if t==':' and isinstance(token, Punctuation) and token.endp == ':': return True
        if t=='.' and isinstance(token, Punctuation) and token.endp == '.': return True
        if t=='q' and target.startswith('q~') and isinstance(token, Quote) and token.endp[-1] not in ['.','!','?']: return True
        if t=='q' and target.startswith('q!') and isinstance(token, Quote) and token.endp[-1] in ['.','!','?']: return True
        return False
        if target.startswith('Q') and isinstance(token, Quote): return True
        if target.startswith('P') and isinstance(token, entitymanager.Mention): return True
        if target.startswith('E') and isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_EXPRESS: return True
        if target.startswith('S') and isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_SAY: return True
        if target.startswith('A') and isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_ASK: return True
        if target.startswith('R') and isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_REPLY: return True
        if target.startswith('T') and isinstance(token, verbmanager.Verb) and token.token.lemma in VERBS_THINK: return True
        if target.startswith('V') and isinstance(token, verbmanager.Verb) : return True
        if target.startswith(':') and isinstance(token, Punctuation) and token.endp == ':': return True
        if target.startswith('.') and isinstance(token, Punctuation) and token.endp == '.': return True
        # [('said', 202), ('asked', 46), ('answered', 22), ('cried', 21), ('thought', 14), ('replied', 10), ('called', 9), ('saying', 7), ('told', 5), ('begged', 4), ('sang', 4), ('yelled', 4), ('whispered', 3), ('repeated', 3), ('croaked', 1), ('urge', 1), ('ran', 1), ('issued', 1), ('winked', 1), ('related', 1), ('ordered', 1), ('hissing', 1), ('muttered', 1), ('coaxed', 1), ('asks', 1), ('implore', 1), ('began', 1), ('asking', 1), ('questioned', 1), ('crying', 1), ('roared', 1), ('repeating', 1), ('Saying', 1), ('cuckooed', 1), ('warned', 1), ('groaned', 1), ('beg', 1), ('exclaimed', 1), ('went', 1)]
    @classmethod
    def match_first(cls, rule, token):
        target = rule.pattern[0]
        if target[0]=='?': return True
        return cls.match(token, target)

    def matched(self):
        return len(self.tokens) >= len(self.rule.pattern)
    def ingest(self, token):
        while True:
            target = self.rule.pattern[len(self.tokens)]
            if target[0]=='?':
                target = target[1:]
                optional = True
            else:
                optional = False
            if self.match(token, target):
                self.tokens.append(token)
                return self
            else:
                if optional:
                    self.tokens.append(EmptyTokenizedToken(target))
                    # check this is not the last target
                    if self.matched():
                        return self
                else:
                    return None
    '''def ingest_old_that_returns_lists(self, token):
        ret = []
        current_matcher = self
        while True:
            target = current_matcher.rule.pattern[len(current_matcher.tokens)]
            if not target.startswith('?'): break
            current_matcher = current_matcher.clone()
            current_matcher.tokens.append(EmptyTokenizedToken(target))
            ret.append(current_matcher)
        if self.match(token, target):
            self.tokens.append(token)
            ret.append(self)
        else:
            ret.append(None)
        return ret
    def ingest_old_w_o_support_for_optional(self, token):
        target = self.rule.pattern[len(self.tokens)]
        if self.match(token, target):
            self.tokens.append(token)
            return self
        else:
            return None'''
    def try_apply(self):
        if self.matched():
            self.apply()
            return None
        else:
            return self
    def apply(self, verbose=False):
        if verbose:
            print 'APPLYING RULE',self.rule
        if verbose:
            for token in self.tokens:
                if isinstance(token,Quote):
                    print token
        named = dict(zip(self.rule.pattern,self.tokens))
        named.update(dict(zip([i.replace('q~','Q').replace('q!','Q') for i in self.rule.pattern], self.tokens)))
        for action in self.rule.actions:
            #print self.rule
            if len(action)==3:
                # assignment using an object
                quote_name, p_set, target_name = action
                val = named[target_name]
            elif len(action)==4:
                # assignment using an object and a getter
                quote_name, p_set, target_name, p_get = action
                val = getattr(named[target_name], self.property_getters[p_get])()
            getattr(named[quote_name], self.property_setters[p_set])(val, str(self.rule))

def predict_quoted_speech(input_tuple, rules):
    output, quotes,mentions,verbs = input_tuple
    apply_matchers(output,rules)
    return output, quotes, mentions, verbs

def apply_matchers(tokenized_string,rules):
    matchers = []
    for token in tokenized_string:
        for rule in rules:
            if QuotedSpeechMatcher.match_first(rule, token):
                matchers.append(QuotedSpeechMatcher(rule))
        matchers = [matcher.ingest(token) for matcher in matchers]
        #matchers = util.flatten(matchers) # necessary to support ND-FSA
        matchers = filter(None, matchers) # kill dead matchers
        matchers = [matcher.try_apply() for matcher in matchers]
        matchers = filter(None, matchers)  # kill matchers that applied successfully

def load_rules_followup():
    rules = [
        'Q1 Q2 > Q1.l=Q2.s Q2.l=Q1.s',
        'Q2 Q1 > Q1.l=Q2.s Q2.l=Q1.s',
        'Q1 ?V ?P ?V ?P Q2 > Q1.l=Q2.s Q2.l=Q1.s',
        'Q2 ?V ?P ?V ?P Q1 > Q1.l=Q2.s Q2.l=Q1.s',
        'q~1 ?V ?P ?V ?P q!2 > Q2.s=Q1.s Q2.l=Q1.l',
        'q~2 ?V ?P ?V ?P q!1 > Q2.s=Q1.s Q2.l=Q1.l',
    ]
    return [QuotedSpeechPredictorRule.from_string(i,FOLLOWUP_RULE) for i in rules]

def load_weights_followup():
    data = '''_Q1 Q2 > Q1.l=Q2.s Q2.l=Q1.s	ACCURACY	0.00	0.59	2.93
_Q2 Q1 > Q1.l=Q2.s Q2.l=Q1.s	ACCURACY	0.00	0.59	2.93
_Q1 ?V ?P ?V ?P Q2 > Q1.l=Q2.s Q2.l=Q1.s	ACCURACY	0.00	0.43	3.33
_Q2 ?V ?P ?V ?P Q1 > Q1.l=Q2.s Q2.l=Q1.s	ACCURACY	0.00	0.43	3.33
_q~1 ?V ?P ?V ?P q!2 > Q2.s=Q1.s Q2.l=Q1.l	ACCURACY	0.68	0.07	0.38
_q~2 ?V ?P ?V ?P q!1 > Q2.s=Q1.s Q2.l=Q1.l	ACCURACY	0.68	0.10	0.39
'''.splitlines()
    data = [i.split('\t') for i in data]
    return dict([(i[0], (i[2], i[3])) for i in data])


def load_rules_manual():
    # example rules
    # Q S P > Q.s=P
    #   when Quote Said Person is found, assign Person to Quote.speaker
    # Q1 S P1 Q2 R P2> Q1.s=P1 Q1.l=P2 Q1.s=P1 Q1.l=P2:
    #   when Quote1 Said Person1 Quote2 Replied Person2 is found, assign Person1 to Quote1.speaker, Person2 to Quote1.listener...
    rules = [
        'Q S P > Q.s=P',
        'Q P S > Q.s=P',
        'P S Q > Q.s=P',
        'S P Q > Q.s=P',
        'S Q P > Q.s=P',
        'P Q S > Q.s=P',
        'Q1 S P1 Q2 R P2 > Q1.s=P1 Q1.l=P2 Q1.s=P1 Q1.l=P2',
        'Q E P > Q.s=P',
        'Q P E > Q.s=P',
        'P E Q > Q.s=P',
        'E P Q > Q.s=P',
        'E Q P > Q.s=P',
        'P Q E > Q.s=P',
        'Q T ?: P > Q.s=P Q.l=P',
        'Q P T ?. > Q.s=P Q.l=P',
        'P T ?: Q > Q.s=P Q.l=P',
        'T P ?: Q > Q.s=P Q.l=P',
        'T Q P ?. > Q.s=P Q.l=P',
        'P Q T ?. > Q.s=P Q.l=P',
        'Q1 E P1 Q2 R P2 > Q1.s=P1 Q1.l=P2 Q1.s=P1 Q1.l=P2',
        'Q S P > Q.s=P',
        'q~ S P > q~.s=P',
        'q~ S P q! > q~.s=P q!.s=P',
        #'q~1 S P q~2 q!> q~1.s=P q~2.s=P q!.s=P',
        'P S q! > q!.s=P',
        'P R q! > q!.s=P',
        #'P1 S q!1 P2 R q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1', doesn't work with punctuation
        'P1 S q!1 P2 R q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'q!1 q!2 P A > q!1.l=P q!2.s=P',
        'q!1 q~ S P q!2 > Q.s=P Q1.s=P Q2.s=P',
        'P E : Q > Q.s=P',
        'E P : Q > Q.s=P',
        'P S : Q > Q.s=P',
        'S P : Q > Q.s=P',
        'P A : Q > Q.s=P',
        #'A P : Q > Q.s=P',
        'P1 E P2 : Q > Q.s=P1 Q.l=P2',
        'P2 E P1 : Q > Q.s=P1 Q.l=P2',
        'P1 S P2 : Q > Q.s=P1 Q.l=P2',
        #'P2 S P1 : Q > Q.s=P1 Q.l=P2',
        'P1 A P2 : Q > Q.s=P1 Q.l=P2',
        'P2 A P1 : Q > Q.s=P1 Q.l=P2',
        'P1 E ?: q!1 P2 E ?: q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'P1 E ?: q!1 P2 R ?: q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'P1 A ?: q!1 P2 R ?: q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'P1 E ?: q!1 q!2 P2 E > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'P1 A ?: q!1 q!2 P2 R > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'P1 E P2 ?: Q > Q.s=P1 Q.l=P2',
        # new:
        # 'P1 V V P2 ?: Q > Q.s=P1 Q.l=P2', #Then the dragon began to implore Nikita:
    ]

    return [QuotedSpeechPredictorRule.from_string(i) for i in rules]

def extract_rules_window(output, quote, token_start, token_end):
    rules = []
    tokens = output[token_start:token_end]
    # check if there are valid mentions to construct the rule
    mentions = [i for i in tokens if isinstance(i,entitymanager.Mention)]
    speakers = [i for i in mentions if i.get_most_likely_symbol()==quote.annotations.speaker_annotation and quote.annotations.speaker_annotation]
    listeners = [i for i in mentions if i.get_most_likely_symbol()==quote.annotations.listener_annotation and quote.annotations.listener_annotation]
    quotes = [i for i in tokens if isinstance(i,Quote) and i!=quote]

    # compute rules for the given quote
    pattern = ['%s%d' % (token_to_string(token),i) for i,token in enumerate(tokens)]

    # TODO do it for all the quotes in the token list?
    token_to_string_dict = dict(zip(tokens,pattern))
    target = token_to_string_dict[quote]
    if speakers or listeners:
        actions = []
        for p in speakers:
            actions.append((target,'s',token_to_string_dict[p]))
        for p in listeners:
            actions.append((target,'l',token_to_string_dict[p]))
        rules.append(QuotedSpeechPredictorRule(pattern,actions))
    if quotes:
        actions = []
        for q in quotes:
            if quote.annotations.speaker_annotation==q.annotations.speaker_annotation:
                actions.append((target,'s',token_to_string_dict[q],'s'))
            if quote.annotations.speaker_annotation==q.annotations.listener_annotation:
                actions.append((target,'s',token_to_string_dict[q],'l'))
            if quote.annotations.listener_annotation==q.annotations.speaker_annotation:
                actions.append((target,'l',token_to_string_dict[q],'s'))
            if quote.annotations.listener_annotation==q.annotations.listener_annotation:
                actions.append((target,'l',token_to_string_dict[q],'l'))
        rules.append(QuotedSpeechPredictorRule(pattern, actions, FOLLOWUP_RULE))
    return rules

def extract_rules(input):
    rules = []
    missing = []
    output, quotes, mentions, verbs = input
    for token_i, token in enumerate(output):
        if isinstance(token,Quote):
            rules_ = []
            for before in range(1,6):
                for after in range(1,6):
                    start = token_i - before
                    end = token_i + after
                    if start<0 or end>len(output): continue
                    if not token.annotations: continue
                rules_ += extract_rules_window(output,token,start, end)
            if not rules_:
                missing += [token]
            rules += rules_
    return rules, missing


def clean_assignments(data_set):
    for output_tuple in data_set.values():
        output, quotes, mentions, verbs = output_tuple
        for quote in quotes:
            quote.speaker_mention = None
            quote.speaker_mentions = {}
            quote.listener_mention = None
            quote.listener_mentions = {}

def generalize_rules(rules):
    def generalize_options(i, actions):
        r = []
        if i[0] in ['S', 'A', 'R', 'T', 'E', 'q']:
            pass
            #r.append(generalize_token(i))
        if i not in [j[0] for j in actions] and i not in [j[2] for j in actions]:
            r.append('?' + i)
        if not r:
            r = [i]
        return r

    def generalize_token(i):
        return RULE_LANGUAGE.get(i[0:-1], i[0:-1]) + i[-1]

    def generalize_action(j):
        j = list(j)
        j[0] = generalize_token(j[0])
        return j

    rules2 = []
    for rule in rules:
        patterns = itertools.product(*[generalize_options(i, rule.actions) for i in rule.pattern])
        for pattern2 in patterns:
            actions2 = [j if (j[0] in pattern2) else generalize_action(j) for j in rule.actions]
            actions2 = [j for j in actions2 if j[2] in pattern2 and j[0] in pattern2]
            if not actions2: continue
            rules2.append(QuotedSpeechPredictorRule(pattern2, actions2, rule.rule_type))
    return rules2

def main_extract(verbose = True):
    logging.basicConfig(level=logging.ERROR)
    logger.setLevel(logging.ERROR)
    logging.root.setLevel(logging.ERROR)
    data_set = {}
    print 'LOADING DATA'
    for story_file in settings.STY_FILES:
        doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH + story_file)
        quotedspeechhelper.annotate_sentences(doc, settings.STORY_ALL_SENTENCES,single_sentences_file_story_id=doc.id)
        output_tuple = tokenize_document(doc)
        data_set[story_file] = output_tuple
    for story_file in settings.STY_FILES:
        print 'CROSS VALIDATION', story_file
        clean_assignments(data_set)
        rules = []
        uncovered_quotes = []
        training_tuples = []
        test_tuples = []
        for key,tuple in data_set.items():
            if key==story_file:
                test_tuples.append(tuple)
            else:
                training_tuples.append(tuple)

        print 'TRAINING/EXTRACTING RULES'
        for output_tuple in training_tuples:
            rules_,uncovered_quotes_ = extract_rules(output_tuple)
            rules += rules_
            uncovered_quotes += uncovered_quotes_

        print 'GENERALIZE/EXPAND RULES',len(rules)
        rules = generalize_rules(rules)

        print 'TRAINING WEIGHTS',len(rules)
        rules_eval = [str(i) for i in rules]
        rules_eval_counts = collections.Counter(rules_eval)
        rules_eval = set(rules_eval)
        rules_accum = dict([(i, [0] * 14) for i in rules_eval])
        weights = {}
        for rule_type in [None, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE]:
        #for rule_type in [None]:
            for output_tuple in training_tuples:
                rules_to_apply = [i for i in rules if i.rule_type==rule_type]
                output_tuple = predict_quoted_speech(output_tuple, rules_to_apply)
            for output_tuple in training_tuples:
                for rule in rules_eval:
                    e = eval_quoted_speech(output_tuple, rule, laplace=1)
                    rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
            for rule in rules_eval:
                weights[rule] = list(compute_eval_quoted_speech(rules_accum[rule])[1:])
            for output_tuple in training_tuples:
                weighted_assignment(output_tuple, weights)
        vozbase.serialize_to_file(weights,'weights.json',False)

        # TODO better way to prune out bad rules? takes too long otherwise
        # laplace=1 makes default accuracy 0.5
        rules = [i for i in rules if weights[str(i)][2]>0.5]

        print 'TEST ASSIGNMENT',len(rules)
        rules_accum['aggregated'] = [0] * 14
        for output_tuple in test_tuples:
            output_tuple = predict_quoted_speech(output_tuple, rules)
            weighted_assignment(output_tuple, weights)
            e = eval_quoted_speech(output_tuple, None)
            rule = 'aggregated'
            rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
            print 'Story-'+'\t'.join([str(i) for i in [rule] + list(compute_eval_quoted_speech(rules_accum[rule]))])
        break # first cross validation set complete here

    print 'OVERALL ALL STORIES'
    rule = 'aggregated'
    print '\t'.join([str(i) for i in [rule] + list(compute_eval_quoted_speech(rules_accum[rule]))])
    if verbose and False:
        for rule in rules_accum.keys():
            print '\t'.join([str(i) for i in [rule]+list(compute_eval_quoted_speech(rules_accum[rule]))])




def main_all(verbose = False):
    DO_MANUAL = False
    DO_FOLLOWUP = True
    logging.basicConfig(level=logging.ERROR)
    logging.basicConfig(level=logging.INFO)

    if DO_MANUAL:
        rules = load_rules_manual()
    else:
        #rules = load_auto_rules()
        rules = load_rules_from_weights_from_auto()
    rules_eval = [str(i) for i in rules]
    #rules_eval += ['baseline']

    rules_accum = dict([(i, [0] * 14) for i in rules_eval])
    rules_accum['aggregated'] = [0] * 14
    rules_accum['aggregated2'] = [0] * 14
    rules_accum['aggregated3'] = [0] * 14

    #for story_file in ["01 - Nikita the Tanner.sty"]:
    #for story_file in settings.STY_FILES[5:6]:
    for story_file in settings.STY_FILES:
        print story_file
        doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH+story_file)
        #styhelper.fix_sty_annotations(doc)
        quotedspeechhelper.annotate_sentences(doc, settings.STORY_ALL_SENTENCES, single_sentences_file_story_id = doc.id)
        if verbose:
            for sentence in doc.sentences:
                print sentence.format_quoted_annotations()

        output_tuple = tokenize_document(doc)
        if 'baseline' in rules_eval:
            # Informed Baseline
            output_tuple = predict_quoted_speech_informed(output_tuple)

        if rules:
            #Assignment
            output_tuple = predict_quoted_speech(output_tuple,rules)
        if verbose:
            print_quoted_speech_tuple(output_tuple)

        for rule in rules_eval:
            e = eval_quoted_speech(output_tuple, rule)
            rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
            if verbose:
                print_eval_quoted_speech(e)
        if 'aggregated' in rules_accum.keys():
            if DO_MANUAL:
                weights = load_weights()
            else:
                weights = load_weights_from_auto()
            weighted_assignment(output_tuple,weights)
            e = eval_quoted_speech(output_tuple, None)
            rule = 'aggregated'
            rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
        if DO_FOLLOWUP:
            # this relies on having aggreggated
            rules2 = load_rules_followup()
            for _ in range(20):
                output_tuple = predict_quoted_speech(output_tuple, rules2)
            rules_eval2 = [str(i) for i in rules2]
            for rule in rules_eval2:
                e = eval_quoted_speech(output_tuple, rule)
                if rule not in rules_accum:
                    rules_accum[rule] = [0] * 14
                rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
                if verbose:
                    print_eval_quoted_speech(e)
            if 'aggregated2' in rules_accum.keys():
                weights2 = load_weights_followup()
                weights.update(weights2)
                weighted_assignment(output_tuple,weights)
                e = eval_quoted_speech(output_tuple, None)
                rule = 'aggregated2'
                rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
        if 'aggregated3' in rules_accum.keys():
            rule = 'aggregated3'
            output_tuple = predict_quoted_speech_informed(output_tuple,False,rule)
            e = eval_quoted_speech(output_tuple, None)
            rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]

    if verbose:
            print_quoted_speech_tuple(output_tuple)

    print 'OVERALL ALL STORIES'
    rules_accum_ = [0.0 for _ in rules_accum.values()[0]]
    for rule in rules_accum.keys():
        if False and verbose:
            print rule
            print_eval_quoted_speech(rules_accum[rule])
        else:
            print '\t'.join([str(i) for i in [rule]+list(compute_eval_quoted_speech(rules_accum[rule]))])
        rules_accum_ = [(1.0*a/len(rules_eval) + b) for a, b in zip(rules_accum[rule], rules_accum_)]
    if verbose:
        print 'AVERAGE RULES'
        print_eval_quoted_speech(rules_accum_)

def main_minilang_parser():
    tokenized = [Punctuation('.'), Punctuation(':'), Punctuation('.'), Punctuation(':')]
    rules = [
        ':1 . :2 > :1.m=:2',
        ':1 ?. :2 > :1.m=:2',
        '?. ?. :1 ?. ?. :2 > :1.m=:2'
    ]
    rules = [QuotedSpeechPredictorRule.from_string(i) for i in rules]
    apply_matchers(tokenized,rules)
    for i in tokenized:
        print i.endp, len(i.matched_rules), i.matched_rules

def load_weights_from_auto():
    data = '''P1 E P2 ?: Q > Q.s=P1 Q.l=P2	ACCURACY	0.559322033898	0.655172413793	0.601694915254
Q1 Q2 E P ?. > Q2.s=P Q1.l=P	ACCURACY	0.603773584906	0.520833333333	0.537735849057
q!1 Q2 E P ?. > Q2.s=P Q1.l=P	ACCURACY	0.603773584906	0.520833333333	0.537735849057
P1 ?V V ?V P2 ?: Q > Q.s=P1 Q.l=P2	ACCURACY	0.5	0.475	0.47619047619
P1 ?V V ?V P2 ?: q! > Q.s=P1 Q.l=P2	ACCURACY	0.5	0.475	0.47619047619
Q1 Q2 S P ?. > Q2.s=P Q1.l=P	ACCURACY	0.512195121951	0.473684210526	0.475609756098
q!1 Q2 S P ?. > Q2.s=P Q1.l=P	ACCURACY	0.512195121951	0.473684210526	0.475609756098
Q1 q~2 E P ?. > Q2.s=P Q1.l=P	ACCURACY	0.5	0.428571428571	0.4375
q!1 q~2 E P ?. > Q2.s=P Q1.l=P	ACCURACY	0.5	0.428571428571	0.4375
Q1 Q2 E P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.475	0.296296296296	0.4375
q!1 Q2 E P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.475	0.296296296296	0.4375
Q1 Q2 E P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.447368421053	0.301886792453	0.434210526316
q!1 Q2 E P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.447368421053	0.301886792453	0.434210526316
P1 S P2 ?: Q > Q.s=P1 Q.l=P2	ACCURACY	0.361111111111	0.428571428571	0.388888888889
E P1 ?: Q1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.4	0.394736842105	0.3875
E P1 ?: q!1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.4	0.394736842105	0.3875
Q E P ?. > Q.s=P	ACCURACY	0.765306122449	0	0.382653061224
Q1 q~2 S P ?. > Q2.s=P Q1.l=P	ACCURACY	0.411764705882	0.354838709677	0.367647058824
q!1 q~2 S P ?. > Q2.s=P Q1.l=P	ACCURACY	0.411764705882	0.354838709677	0.367647058824
q~1 E P q!2 > Q1.s=P Q2.s=P	ACCURACY	0.72972972973	0	0.364864864865
Q S P ?. > Q.s=P	ACCURACY	0.728395061728	0	0.364197530864
P1 ?V P2 ?V . Q1 q!2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.363636363636	0.363636363636	0.363636363636
P1 ?V P2 ?V . q!1 q!2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.363636363636	0.363636363636	0.363636363636
Q1 Q2 S P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.393939393939	0.261904761905	0.363636363636
q!1 Q2 S P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.393939393939	0.261904761905	0.363636363636
q~ E P ?. > Q.s=P	ACCURACY	0.723684210526	0	0.361842105263
Q1 Q2 S P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.375	0.261904761905	0.359375
q!1 Q2 S P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.375	0.261904761905	0.359375
P1 ?V P2 ?V . Q1 Q2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.369565217391	0.347826086957	0.358695652174
P1 ?V P2 ?V . q!1 Q2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.369565217391	0.347826086957	0.358695652174
q~1 S P q!2 > Q1.s=P Q2.s=P	ACCURACY	0.705882352941	0	0.352941176471
P1 ?V P2 ?V . Q > Q.s=P1 Q.l=P2	ACCURACY	0.367088607595	0.342105263158	0.348101265823
q~ S P ?. > Q.s=P	ACCURACY	0.695652173913	0	0.347826086957
Q1 q!2 E P ?. > Q2.s=P Q1.l=P	ACCURACY	0.363636363636	0.30303030303	0.333333333333
q!1 q!2 E P ?. > Q2.s=P Q1.l=P	ACCURACY	0.363636363636	0.30303030303	0.333333333333
Q1 q~2 E P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.393939393939	0.225	0.333333333333
q!1 q~2 E P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.393939393939	0.225	0.333333333333
P1 ?V P2 ?V . q! > Q.s=P1 Q.l=P2	ACCURACY	0.388888888889	0.264150943396	0.324074074074
Q1 q~2 E P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.354838709677	0.225	0.322580645161
q!1 q~2 E P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.354838709677	0.225	0.322580645161
P1 A P2 ?: Q > Q.s=P1 Q.l=P2	ACCURACY	0.290322580645	0.354838709677	0.322580645161
E P1 Q1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.34375	0.3	0.3125
E P1 q!1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.34375	0.3	0.3125
E P1 ?: Q1 Q2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.3125	0.322580645161	0.3125
E P1 ?: q!1 Q2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.3125	0.322580645161	0.3125
E P1 ?: Q1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.323529411765	0.30303030303	0.308823529412
E P1 ?: q!1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.323529411765	0.30303030303	0.308823529412
P1 ?V E ?V P2 ?: Q > Q.s=P1 Q.l=P2	ACCURACY	0.322580645161	0.310344827586	0.306451612903
P1 ?V E ?V P2 ?: q! > Q.s=P1 Q.l=P2	ACCURACY	0.322580645161	0.310344827586	0.306451612903
P2 ?V P1 ?V . Q > Q.s=P1 Q.l=P2	ACCURACY	0.303797468354	0.302631578947	0.29746835443
Q1 q~2 S P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.333333333333	0.194444444444	0.283333333333
q!1 q~2 S P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.333333333333	0.194444444444	0.283333333333
S P1 ?: Q1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.3125	0.266666666667	0.28125
S P1 ?: q!1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.3125	0.266666666667	0.28125
Q1 Q2 P E ?. > Q2.s=P Q1.l=P	ACCURACY	0.3125	0.25	0.28125
q!1 Q2 P E ?. > Q2.s=P Q1.l=P	ACCURACY	0.3125	0.25	0.28125
P ?E ?V ?V ?V ?: q! > Q.s=P	ACCURACY	0.555172413793	0	0.277586206897
P ?E ?V ?V ?V ?: Q > Q.s=P	ACCURACY	0.554054054054	0	0.277027027027
Q1 q~2 S P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.310344827586	0.194444444444	0.275862068966
q!1 q~2 S P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.310344827586	0.194444444444	0.275862068966
Q P E ?. > Q.s=P	ACCURACY	0.545454545455	0	0.272727272727
q~ P E ?. > Q.s=P	ACCURACY	0.545454545455	0	0.272727272727
S P1 Q1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.3	0.25	0.266666666667
S P1 q!1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.3	0.25	0.266666666667
P E ?: Q ?. > Q.s=P	ACCURACY	0.525641025641	0	0.262820512821
P E ?: q! ?. > Q.s=P	ACCURACY	0.519480519481	0	0.25974025974
Q1 q!2 S P ?. > Q2.s=P Q1.l=P	ACCURACY	0.259259259259	0.259259259259	0.259259259259
q!1 q!2 S P ?. > Q2.s=P Q1.l=P	ACCURACY	0.259259259259	0.259259259259	0.259259259259
P2 ?V P1 ?V . q! > Q.s=P1 Q.l=P2	ACCURACY	0.259259259259	0.245283018868	0.25
P1 E ?: Q1 P2 E ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.266666666667	0.233333333333	0.25
P1 E ?: Q1 P2 E ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.266666666667	0.233333333333	0.25
P1 E ?: q!1 P2 E ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.266666666667	0.233333333333	0.25
P1 E ?: q!1 P2 E ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.266666666667	0.233333333333	0.25
Q P S ?. > Q.s=P	ACCURACY	0.489795918367	0	0.244897959184
P E Q ?. > Q.s=P	ACCURACY	0.487179487179	0	0.24358974359
Q1 q!2 E P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.222222222222	0.205882352941	0.240740740741
Q1 q!2 E P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.222222222222	0.212121212121	0.240740740741
q!1 q!2 E P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.222222222222	0.205882352941	0.240740740741
q!1 q!2 E P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.222222222222	0.212121212121	0.240740740741
q! E P ?. > Q.s=P	ACCURACY	0.47619047619	0	0.238095238095
P E q! ?. > Q.s=P	ACCURACY	0.473684210526	0	0.236842105263
P1 E ?: Q1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.266666666667	0.206896551724	0.233333333333
P1 E ?: q!1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.266666666667	0.206896551724	0.233333333333
P1 S ?: Q1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.266666666667	0.206896551724	0.233333333333
P1 S ?: q!1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.266666666667	0.206896551724	0.233333333333
E P1 Q1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.25	0.222222222222	0.232142857143
E P1 q!1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.25	0.222222222222	0.232142857143
E P1 ?: Q1 q~2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.25	0.222222222222	0.232142857143
E P1 ?: q!1 q~2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.25	0.222222222222	0.232142857143
S P1 ?: Q1 Q2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.25	0.222222222222	0.232142857143
S P1 ?: q!1 Q2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.25	0.222222222222	0.232142857143
S P Q ?. > Q.s=P	ACCURACY	0.462962962963	0	0.231481481481
S P q! ?. > Q.s=P	ACCURACY	0.462962962963	0	0.231481481481
q~ P S ?. > Q.s=P	ACCURACY	0.459459459459	0	0.22972972973
P1 ?V P2 ?V . q~ > Q.s=P1 Q.l=P2	ACCURACY	0.177777777778	0.279069767442	0.222222222222
P2 ?V P1 ?V . q~ > Q.s=P1 Q.l=P2	ACCURACY	0.222222222222	0.232558139535	0.222222222222
q~1 P E q!2 > Q1.s=P Q2.s=P	ACCURACY	0.444444444444	0	0.222222222222
P ?P2 ?V . q! > Q.s=P	ACCURACY	0.438461538462	0	0.219230769231
P ?V . q! > Q.s=P	ACCURACY	0.438461538462	0	0.219230769231
E P q! ?. > Q.s=P	ACCURACY	0.426470588235	0	0.213235294118
E P1 Q1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.230769230769	0.2	0.211538461538
E P1 q!1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.230769230769	0.2	0.211538461538
S P1 Q1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.230769230769	0.2	0.211538461538
S P1 q!1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.230769230769	0.2	0.211538461538
Q1 q~2 P E ?. > Q2.s=P Q1.l=P	ACCURACY	0.230769230769	0.192307692308	0.211538461538
q!1 q~2 P E ?. > Q2.s=P Q1.l=P	ACCURACY	0.230769230769	0.192307692308	0.211538461538
E P Q ?. > Q.s=P	ACCURACY	0.420289855072	0	0.210144927536
P ?P2 ?V . Q > Q.s=P	ACCURACY	0.412429378531	0	0.206214689266
P ?V . Q > Q.s=P	ACCURACY	0.412429378531	0	0.206214689266
S P1 ?: Q1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.214285714286	0.185185185185	0.196428571429
S P1 ?: q!1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.214285714286	0.185185185185	0.196428571429
Q1 P1 E ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.192307692308	0.192307692308
Q1 P1 E ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.192307692308	0.192307692308
Q1 P1 E ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.192307692308	0.192307692308
Q1 P1 E ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.192307692308	0.192307692308
P1 E ?: Q1 Q2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.192307692308	0.192307692308
P1 E ?: q!1 Q2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.192307692308	0.192307692308
P1 S ?: Q1 Q2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.192307692308	0.192307692308
P1 S ?: q!1 Q2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.192307692308	0.192307692308
E P1 ?: Q1 q!2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.2	0.192307692308
E P1 ?: q!1 q!2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.2	0.192307692308
Q1 Q2 R P ?. > Q2.s=P Q1.l=P	ACCURACY	0.230769230769	0.166666666667	0.192307692308
q!1 Q2 R P ?. > Q2.s=P Q1.l=P	ACCURACY	0.230769230769	0.166666666667	0.192307692308
q! P E ?. > Q.s=P	ACCURACY	0.375	0	0.1875
P1 S ?: Q1 P2 E ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.185185185185	0.185185185185	0.185185185185
P1 S ?: Q1 P2 E ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.185185185185	0.185185185185	0.185185185185
P1 S ?: q!1 P2 E ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.185185185185	0.185185185185	0.185185185185
P1 S ?: q!1 P2 E ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.185185185185	0.185185185185	0.185185185185
P S ?: Q ?. > Q.s=P	ACCURACY	0.363636363636	0	0.181818181818
Q1 Q2 P S ?. > Q2.s=P Q1.l=P	ACCURACY	0.2	0.16	0.18
Q1 q~2 P S ?. > Q2.s=P Q1.l=P	ACCURACY	0.2	0.16	0.18
q!1 Q2 P S ?. > Q2.s=P Q1.l=P	ACCURACY	0.2	0.16	0.18
q!1 q~2 P S ?. > Q2.s=P Q1.l=P	ACCURACY	0.2	0.16	0.18
S P ?: Q ?. > Q.s=P	ACCURACY	0.352112676056	0	0.176056338028
S P ?: q! ?. > Q.s=P	ACCURACY	0.352112676056	0	0.176056338028
P S ?: q! ?. > Q.s=P	ACCURACY	0.351851851852	0	0.175925925926
S P1 Q1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.16	0.173076923077
S P1 q!1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.16	0.173076923077
P1 E ?: Q1 q!2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.153846153846	0.173076923077
P1 E ?: q!1 q!2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.153846153846	0.173076923077
P1 S ?: Q1 q!2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.153846153846	0.173076923077
P1 S ?: q!1 q!2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.153846153846	0.173076923077
S P1 ?: Q1 q~2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.16	0.173076923077
S P1 ?: q!1 q~2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.192307692308	0.16	0.173076923077
q! S P ?. > Q.s=P	ACCURACY	0.34375	0	0.171875
E P1 Q1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.34375	0	0.171875
E P1 q!1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.34375	0	0.171875
P S Q ?. > Q.s=P	ACCURACY	0.333333333333	0	0.166666666667
P R ?: Q ?. > Q.s=P	ACCURACY	0.333333333333	0	0.166666666667
P R ?: q! ?. > Q.s=P	ACCURACY	0.333333333333	0	0.166666666667
q~1 P S q!2 > Q1.s=P Q2.s=P	ACCURACY	0.333333333333	0	0.166666666667
E P ?: q! ?. > Q.s=P	ACCURACY	0.326732673267	0	0.163366336634
P2 ?V P1 ?V . Q1 Q2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.173913043478	0.152173913043	0.163043478261
P2 ?V P1 ?V . q!1 Q2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.173913043478	0.152173913043	0.163043478261
E P ?: Q ?. > Q.s=P	ACCURACY	0.323529411765	0	0.161764705882
Q1 E P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.2	0.130434782609	0.16
Q1 E P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.2	0.130434782609	0.16
Q1 E P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.2	0.130434782609	0.16
Q1 E P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.2	0.130434782609	0.16
Q1 q~2 R P ?. > Q2.s=P Q1.l=P	ACCURACY	0.2	0.130434782609	0.16
q!1 q~2 R P ?. > Q2.s=P Q1.l=P	ACCURACY	0.2	0.130434782609	0.16
P2 ?V P1 ?V . Q1 q!2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.159090909091	0.159090909091	0.159090909091
P2 ?V P1 ?V . q!1 q!2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.159090909091	0.159090909091	0.159090909091
P S q! ?. > Q.s=P	ACCURACY	0.310344827586	0	0.155172413793
E P1 ?: Q1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.172413793103	0.142857142857	0.155172413793
E P1 ?: q!1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.172413793103	0.142857142857	0.155172413793
E P1 ?: Q1 Q2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.153846153846	0.153846153846
E P1 ?: Q1 q~2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.153846153846	0.153846153846
E P1 ?: q!1 Q2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.153846153846	0.153846153846
E P1 ?: q!1 q~2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.153846153846	0.153846153846
Q1 q!2 S P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.130434782609	0.153846153846	0.152173913043
Q1 q!2 S P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.130434782609	0.153846153846	0.152173913043
q!1 q!2 S P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.130434782609	0.153846153846	0.152173913043
q!1 q!2 S P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.130434782609	0.153846153846	0.152173913043
S P1 Q1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.3	0	0.15
S P1 q!1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.3	0	0.15
P1 E Q1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.125	0.145833333333
P1 E Q1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.125	0.145833333333
P1 E q!1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.125	0.145833333333
P1 E q!1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.125	0.145833333333
P1 S Q1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.125	0.145833333333
P1 S Q1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.125	0.145833333333
P1 S q!1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.125	0.145833333333
P1 S q!1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.125	0.145833333333
E P1 Q1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.130434782609	0.145833333333
E P1 Q1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.130434782609	0.145833333333
E P1 q!1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.130434782609	0.145833333333
E P1 q!1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.130434782609	0.145833333333
S P1 Q1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.130434782609	0.145833333333
S P1 Q1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.130434782609	0.145833333333
S P1 q!1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.130434782609	0.145833333333
S P1 q!1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.130434782609	0.145833333333
E P1 ?: Q1 q!2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.166666666667	0.145833333333
E P1 ?: q!1 q!2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.166666666667	0.145833333333
S P1 ?: Q1 q!2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.130434782609	0.145833333333
S P1 ?: q!1 q!2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.130434782609	0.145833333333
Q1 Q2 A P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.125	0.142857142857	0.145833333333
Q1 Q2 A P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.125	0.148148148148	0.145833333333
q!1 Q2 A P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.125	0.142857142857	0.145833333333
q!1 Q2 A P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.125	0.148148148148	0.145833333333
E P1 ?: Q1 P2 E ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.142857142857	0.142857142857	0.142857142857
E P1 ?: Q1 P2 E ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.142857142857	0.142857142857	0.142857142857
E P1 ?: q!1 P2 E ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.142857142857	0.142857142857	0.142857142857
E P1 ?: q!1 P2 E ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.142857142857	0.142857142857	0.142857142857
P1 E ?: Q1 P2 S ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.16	0.12	0.14
P1 E ?: Q1 P2 S ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.16	0.12	0.14
P1 E ?: q!1 P2 S ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.16	0.12	0.14
P1 E ?: q!1 P2 S ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.16	0.12	0.14
Q1 E P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.115384615385	0.134615384615
Q1 E P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.115384615385	0.134615384615
E P1 Q1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.12	0.134615384615
E P1 Q1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.12	0.134615384615
E P1 q!1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.12	0.134615384615
E P1 q!1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.12	0.134615384615
E P1 ?: Q1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.12	0.134615384615
E P1 ?: q!1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.153846153846	0.12	0.134615384615
Q1 q!2 P E ?. > Q2.s=P Q1.l=P	ACCURACY	0.153846153846	0.115384615385	0.134615384615
q!1 q!2 P E ?. > Q2.s=P Q1.l=P	ACCURACY	0.153846153846	0.115384615385	0.134615384615
Q P A ?. > Q.s=P	ACCURACY	0.25	0	0.125
q! P A ?. > Q.s=P	ACCURACY	0.25	0	0.125
E P1 Q1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.25	0	0.125
E P1 q!1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.25	0	0.125
Q1 P1 S ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
Q1 P1 S ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
Q1 P1 S ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
Q1 P1 S ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
q~1 P1 E ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
q~1 P1 E ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
q~1 P1 E ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
q~1 P1 E ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
q~1 P1 S ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
q~1 P1 S ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
q~1 P1 S ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
q~1 P1 S ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
q~1 E P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.0909090909091	0.125
q~1 E P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.0909090909091	0.125
q~1 E P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.0909090909091	0.125
q~1 E P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.166666666667	0.0909090909091	0.125
q!1 E P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
q!1 E P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
E P1 Q1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
E P1 Q1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
E P1 q!1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
E P1 q!1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 E ?: Q1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 E ?: Q1 q!2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 E ?: q!1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 E ?: q!1 q!2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 S ?: Q1 q!2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 S ?: q!1 q!2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
A P1 ?: Q1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
A P1 ?: Q1 Q2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
A P1 ?: Q1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
A P1 ?: Q1 q~2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
A P1 ?: q!1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
A P1 ?: q!1 Q2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
A P1 ?: q!1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
A P1 ?: q!1 q~2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 E ?: Q1 P2 R ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 E ?: Q1 P2 R ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 E ?: q!1 P2 R ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 E ?: q!1 P2 R ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 S ?: Q1 P2 R ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 S ?: Q1 P2 R ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 S ?: q!1 P2 R ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P1 S ?: q!1 P2 R ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.125	0.125
P ?P2 ?V . q~ > Q.s=P	ACCURACY	0.238805970149	0	0.119402985075
P ?V . q~ > Q.s=P	ACCURACY	0.238805970149	0	0.119402985075
Q R P ?. > Q.s=P	ACCURACY	0.230769230769	0	0.115384615385
E P1 Q1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.230769230769	0	0.115384615385
E P1 q!1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.230769230769	0	0.115384615385
S P1 Q1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.230769230769	0	0.115384615385
S P1 q!1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.230769230769	0	0.115384615385
Q A P ?. > Q.s=P	ACCURACY	0.222222222222	0	0.111111111111
q! P S ?. > Q.s=P	ACCURACY	0.21875	0	0.109375
P1 E Q1 ?. P2 E Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.130434782609	0.0869565217391	0.108695652174
P1 E Q1 ?. P2 E q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.130434782609	0.0869565217391	0.108695652174
P1 E Q1 ?. P2 S Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.130434782609	0.0869565217391	0.108695652174
P1 E Q1 ?. P2 S q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.130434782609	0.0869565217391	0.108695652174
P1 E q!1 ?. P2 E Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.130434782609	0.0869565217391	0.108695652174
P1 E q!1 ?. P2 E q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.130434782609	0.0869565217391	0.108695652174
P1 E q!1 ?. P2 S Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.130434782609	0.0869565217391	0.108695652174
P1 E q!1 ?. P2 S q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.130434782609	0.0869565217391	0.108695652174
Q1 q!2 A P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0869565217391	0.115384615385	0.108695652174
Q1 q!2 A P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0869565217391	0.12	0.108695652174
q!1 q!2 A P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0869565217391	0.115384615385	0.108695652174
q!1 q!2 A P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0869565217391	0.12	0.108695652174
P1 T P2 ?: Q > Q.s=P1 Q.l=P2	ACCURACY	0.0869565217391	0.130434782609	0.108695652174
E P1 Q1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
E P1 Q1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
E P1 q!1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
E P1 q!1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
P1 E ?: Q1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
P1 E ?: q!1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
P1 S ?: Q1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
P1 S ?: q!1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
E P1 ?: Q1 Q2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
E P1 ?: Q1 q~2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
E P1 ?: q!1 Q2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
E P1 ?: q!1 q~2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.125	0.0869565217391	0.104166666667
Q1 Q2 A P ?. > Q2.s=P Q1.l=P	ACCURACY	0.125	0.0833333333333	0.104166666667
q!1 Q2 A P ?. > Q2.s=P Q1.l=P	ACCURACY	0.125	0.0833333333333	0.104166666667
q~ R P ?. > Q.s=P	ACCURACY	0.2	0	0.1
Q1 E P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.2	0	0.1
Q1 E P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.2	0	0.1
Q1 E P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.2	0	0.1
Q1 E P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.2	0	0.1
Q1 P1 E ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.192307692308	0	0.0961538461538
Q1 P1 E ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.192307692308	0	0.0961538461538
Q1 P1 E ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.192307692308	0	0.0961538461538
Q1 P1 E ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.192307692308	0	0.0961538461538
S P1 Q1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.192307692308	0	0.0961538461538
S P1 q!1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.192307692308	0	0.0961538461538
q!1 P1 E ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
q!1 P1 E ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
q!1 P1 E ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
q!1 P1 E ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
Q1 E P1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
Q1 E P1 ?. q!2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
Q1 A P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
Q1 A P1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
Q1 A P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
Q1 A P1 ?. q!2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
q!1 E P1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
q!1 E P1 ?. q!2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
q!1 A P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
q!1 A P1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
q!1 A P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
q!1 A P1 ?. q!2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E Q1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E Q1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E q!1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E q!1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 S Q1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 S Q1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 S q!1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 S q!1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
E P1 Q1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
E P1 q!1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
A P1 Q1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
A P1 Q1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
A P1 Q1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
A P1 Q1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
A P1 q!1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
A P1 q!1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
A P1 q!1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
A P1 q!1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
S P1 Q1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
S P1 q!1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E ?: Q1 Q2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E ?: Q1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E ?: Q1 q~2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E ?: Q1 q~2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E ?: q!1 Q2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E ?: q!1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E ?: q!1 q~2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 E ?: q!1 q~2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 A ?: Q1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 A ?: Q1 Q2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 A ?: Q1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 A ?: Q1 q~2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 A ?: q!1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 A ?: q!1 Q2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 A ?: q!1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 A ?: q!1 q~2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 S ?: Q1 q~2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
P1 S ?: q!1 q~2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
R P1 ?: Q1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
R P1 ?: Q1 Q2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
R P1 ?: Q1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
R P1 ?: Q1 q~2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
R P1 ?: q!1 Q2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
R P1 ?: q!1 Q2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
R P1 ?: q!1 q~2 E P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
R P1 ?: q!1 q~2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
S P1 ?: Q1 q!2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
S P1 ?: q!1 q!2 S P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
Q1 Q2 P R ?. > Q2.s=P Q1.l=P	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
q!1 Q2 P R ?. > Q2.s=P Q1.l=P	ACCURACY	0.0909090909091	0.0909090909091	0.0909090909091
Q1 Q2 P E ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0869565217391	0.0909090909091
Q1 Q2 P E ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0869565217391	0.0909090909091
Q1 q~2 P E ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0869565217391	0.0909090909091
Q1 q~2 P E ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0869565217391	0.0909090909091
q!1 Q2 P E ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0869565217391	0.0909090909091
q!1 Q2 P E ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0869565217391	0.0909090909091
q!1 q~2 P E ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0869565217391	0.0909090909091
q!1 q~2 P E ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0869565217391	0.0909090909091
E Q P ?. > Q.s=P	ACCURACY	0.176470588235	0	0.0882352941176
P1 S ?: Q1 P2 S ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0869565217391	0.0869565217391	0.0869565217391
P1 S ?: Q1 P2 S ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0869565217391	0.0869565217391	0.0869565217391
P1 S ?: q!1 P2 S ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0869565217391	0.0869565217391	0.0869565217391
P1 S ?: q!1 P2 S ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0869565217391	0.0869565217391	0.0869565217391
P R Q ?. > Q.s=P	ACCURACY	0.166666666667	0	0.0833333333333
P R q! ?. > Q.s=P	ACCURACY	0.166666666667	0	0.0833333333333
q~1 E P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
q~1 E P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
q~1 E P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
q~1 E P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
P1 E Q1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
P1 E Q1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
P1 E q!1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
P1 E q!1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
P1 S Q1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
P1 S Q1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
P1 S q!1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
P1 S q!1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
E P1 Q1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
E P1 Q1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
E P1 q!1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
E P1 q!1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
S P1 Q1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
S P1 Q1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
S P1 q!1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
S P1 q!1 ?. q!2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.166666666667	0	0.0833333333333
E P1 ?: Q1 Q2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
E P1 ?: q!1 Q2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
A P1 ?: Q1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
A P1 ?: q!1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
E P1 ?: Q1 P2 R ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
E P1 ?: Q1 P2 R ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
E P1 ?: q!1 P2 R ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
E P1 ?: q!1 P2 R ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
A P1 ?: Q1 P2 E ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
A P1 ?: Q1 P2 E ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
A P1 ?: q!1 P2 E ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
A P1 ?: q!1 P2 E ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0833333333333	0.0833333333333	0.0833333333333
q~1 P T q!2 > Q1.s=P Q2.s=P	ACCURACY	0.166666666667	0	0.0833333333333
q~1 A P q!2 > Q1.s=P Q2.s=P	ACCURACY	0.166666666667	0	0.0833333333333
q! A P ?. > Q.s=P	ACCURACY	0.16	0	0.08
Q1 E P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.153846153846	0	0.0769230769231
Q1 E P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.153846153846	0	0.0769230769231
E P1 Q1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.153846153846	0	0.0769230769231
E P1 Q1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.153846153846	0	0.0769230769231
E P1 q!1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.153846153846	0	0.0769230769231
E P1 q!1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.153846153846	0	0.0769230769231
E q! P ?. > Q.s=P	ACCURACY	0.151515151515	0	0.0757575757576
S Q P ?. > Q.s=P	ACCURACY	0.148148148148	0	0.0740740740741
P2 ?V V ?V P1 ?: Q > Q.s=P1 Q.l=P2	ACCURACY	0.0714285714286	0.075	0.0714285714286
P2 ?V V ?V P1 ?: q! > Q.s=P1 Q.l=P2	ACCURACY	0.0714285714286	0.075	0.0714285714286
Q1 R P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
Q1 R P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
Q1 R P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
Q1 R P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
Q1 S P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
Q1 S P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
Q1 S P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
Q1 S P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
q~1 R P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
q~1 R P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
q~1 R P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
q~1 R P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
q~1 S P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
q~1 S P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
q~1 S P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
q~1 S P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
E P1 Q1 ?. Q2 A P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
E P1 Q1 ?. q!2 A P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
E P1 q!1 ?. Q2 A P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
E P1 q!1 ?. q!2 A P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 Q1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 Q1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 Q1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 Q1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 q!1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 q!1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 q!1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 q!1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
S P1 Q1 ?. Q2 A P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
S P1 Q1 ?. q!2 A P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
S P1 q!1 ?. Q2 A P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
S P1 q!1 ?. q!2 A P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
E P1 ?: Q1 Q2 A P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
E P1 ?: Q1 q!2 A P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
E P1 ?: q!1 Q2 A P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
E P1 ?: q!1 q!2 A P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 ?: Q1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 ?: Q1 Q2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 ?: Q1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 ?: Q1 q~2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 ?: q!1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 ?: q!1 Q2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 ?: q!1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
T P1 ?: q!1 q~2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
S P1 ?: Q1 Q2 A P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
S P1 ?: Q1 q!2 A P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
S P1 ?: q!1 Q2 A P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
S P1 ?: q!1 q!2 A P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0909090909091	0.047619047619	0.0681818181818
Q1 Q2 R P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0454545454545	0.0681818181818
Q1 q~2 R P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0454545454545	0.0681818181818
q!1 Q2 R P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0454545454545	0.0681818181818
q!1 q~2 R P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0.0454545454545	0.0681818181818
Q P T ?. > Q.s=P	ACCURACY	0.130434782609	0	0.0652173913043
q~ P T ?. > Q.s=P	ACCURACY	0.130434782609	0	0.0652173913043
P1 E Q1 ?. P2 E Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.130434782609	0	0.0652173913043
P1 E Q1 ?. P2 E q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.130434782609	0	0.0652173913043
P1 E Q1 ?. P2 S Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.130434782609	0	0.0652173913043
P1 E Q1 ?. P2 S q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.130434782609	0	0.0652173913043
P1 E q!1 ?. P2 E Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.130434782609	0	0.0652173913043
P1 E q!1 ?. P2 E q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.130434782609	0	0.0652173913043
P1 E q!1 ?. P2 S Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.130434782609	0	0.0652173913043
P1 E q!1 ?. P2 S q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.130434782609	0	0.0652173913043
E Q1 P1 ?. E Q2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0869565217391	0.0652173913043
E Q1 P1 ?. E q!2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0869565217391	0.0652173913043
E Q1 P1 ?. S Q2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0869565217391	0.0652173913043
E Q1 P1 ?. S q!2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0869565217391	0.0652173913043
E q!1 P1 ?. E Q2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0869565217391	0.0652173913043
E q!1 P1 ?. E q!2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0869565217391	0.0652173913043
E q!1 P1 ?. S Q2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0869565217391	0.0652173913043
E q!1 P1 ?. S q!2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0869565217391	0.0652173913043
T P ?: Q ?. > Q.s=P	ACCURACY	0.130434782609	0	0.0652173913043
T P ?: q! ?. > Q.s=P	ACCURACY	0.130434782609	0	0.0652173913043
Q1 q!2 A P ?. > Q2.s=P Q1.l=P	ACCURACY	0.0869565217391	0.0434782608696	0.0652173913043
q!1 q!2 A P ?. > Q2.s=P Q1.l=P	ACCURACY	0.0869565217391	0.0434782608696	0.0652173913043
Q P R ?. > Q.s=P	ACCURACY	0.129032258065	0	0.0645161290323
Q1 P1 S ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
Q1 P1 S ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
Q1 P1 S ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
Q1 P1 S ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
q~1 P1 E ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
q~1 P1 E ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
q~1 P1 E ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
q~1 P1 E ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
q~1 P1 S ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
q~1 P1 S ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
q~1 P1 S ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
q~1 P1 S ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
q!1 E P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
q!1 E P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
E P1 Q1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
E P1 Q1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
E P1 Q1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
E P1 Q1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
E P1 q!1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
E P1 q!1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
E P1 q!1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
E P1 q!1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.125	0	0.0625
S q! P ?. > Q.s=P	ACCURACY	0.115384615385	0	0.0576923076923
P ?E ?V ?V ?V ?: q~ > Q.s=P	ACCURACY	0.115384615385	0	0.0576923076923
Q1 A P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 A P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 A P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 A P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 E P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 E P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 E P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 E P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 A P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 A P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 A P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 A P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
P1 S Q1 ?. P2 E Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
P1 S Q1 ?. P2 E q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
P1 S Q1 ?. P2 S Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
P1 S Q1 ?. P2 S q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
P1 S q!1 ?. P2 E Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
P1 S q!1 ?. P2 E q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
P1 S q!1 ?. P2 S Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
P1 S q!1 ?. P2 S q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
S Q1 P1 ?. E Q2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
S Q1 P1 ?. E q!2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
S Q1 P1 ?. S Q2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
S Q1 P1 ?. S q!2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
S q!1 P1 ?. E Q2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
S q!1 P1 ?. E q!2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
S q!1 P1 ?. S Q2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
S q!1 P1 ?. S q!2 P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 Q2 T P ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 q~2 P R ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 q~2 A P ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 q!2 P R ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 q!2 R P ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 q!2 T P ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 Q2 T P ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 q~2 P R ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 q~2 A P ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 q!2 P R ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 q!2 R P ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 q!2 T P ?. > Q2.s=P Q1.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 Q2 P R ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
Q1 Q2 P R ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
Q1 Q2 P S ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 Q2 P S ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 Q2 R P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
Q1 q~2 P R ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
Q1 q~2 P R ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
Q1 q~2 P S ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 q~2 P S ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
Q1 q~2 A P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
Q1 q~2 A P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
Q1 q~2 R P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
q!1 Q2 P R ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
q!1 Q2 P R ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
q!1 Q2 P S ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 Q2 P S ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 Q2 R P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
q!1 q~2 P R ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
q!1 q~2 P R ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
q!1 q~2 P S ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 q~2 P S ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q!1 q~2 A P ?. Q3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
q!1 q~2 A P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
q!1 q~2 R P ?. q!3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0.0454545454545	0.047619047619
P1 R P2 ?: Q > Q.s=P1 Q.l=P2	ACCURACY	0.047619047619	0.047619047619	0.047619047619
q~ P R ?. > Q.s=P	ACCURACY	0.0909090909091	0	0.0454545454545
q~ A P ?. > Q.s=P	ACCURACY	0.0909090909091	0	0.0454545454545
P T Q ?. > Q.s=P	ACCURACY	0.0909090909091	0	0.0454545454545
P T q! ?. > Q.s=P	ACCURACY	0.0909090909091	0	0.0454545454545
T Q P ?. > Q.s=P	ACCURACY	0.0909090909091	0	0.0454545454545
T q! P ?. > Q.s=P	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 P1 E ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 P1 E ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 P1 E ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 P1 E ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 E P1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 E P1 ?. q!2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 A P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 A P1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 A P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 A P1 ?. q!2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 R P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 R P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 R P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 R P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 S P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 S P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 S P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 S P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q~1 R P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q~1 R P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q~1 R P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q~1 R P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q~1 S P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q~1 S P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q~1 S P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q~1 S P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 E P1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 E P1 ?. q!2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 A P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 A P1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 A P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 A P1 ?. q!2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
P1 E Q1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
P1 E Q1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
P1 E q!1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
P1 E q!1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
P1 S Q1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
P1 S Q1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
P1 S q!1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
P1 S q!1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
E P1 Q1 ?. Q2 A P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
E P1 Q1 ?. q!2 A P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
E P1 Q1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
E P1 q!1 ?. Q2 A P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
E P1 q!1 ?. q!2 A P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
E P1 q!1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
A P1 Q1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
A P1 Q1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
A P1 Q1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
A P1 Q1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
A P1 q!1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
A P1 q!1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
A P1 q!1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
A P1 q!1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
T P1 Q1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
T P1 Q1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
T P1 Q1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
T P1 Q1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
T P1 q!1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
T P1 q!1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
T P1 q!1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
T P1 q!1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
S P1 Q1 ?. Q2 A P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
S P1 Q1 ?. q!2 A P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
S P1 Q1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
S P1 q!1 ?. Q2 A P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
S P1 q!1 ?. q!2 A P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
S P1 q!1 ?. q!2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 P1 E ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
Q1 P1 E ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
Q1 P1 E ?. q!2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
Q1 P1 E ?. q!2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
Q1 P1 E ?. P2 Q2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
Q1 P1 E ?. P2 Q2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
Q1 P1 E ?. P2 q!2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
Q1 P1 E ?. P2 q!2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
q~1 P1 E ?. P2 Q2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
q~1 P1 E ?. P2 Q2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
q~1 P1 E ?. P2 q!2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
q~1 P1 E ?. P2 q!2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
q!1 P1 E ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
q!1 P1 E ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
q!1 P1 E ?. q!2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
q!1 P1 E ?. q!2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 Q1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 Q1 ?. q~2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 q!1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 q!1 ?. q~2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 Q1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 Q1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 Q1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 Q1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 q!1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 q!1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 q!1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 q!1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 Q1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 Q1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 Q1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 Q1 ?. q~2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 Q1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 Q1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 q!1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 q!1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 q!1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 q!1 ?. q~2 P2 R > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 q!1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 q!1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
P T ?: Q ?. > Q.s=P	ACCURACY	0.0909090909091	0	0.0454545454545
P T ?: q! ?. > Q.s=P	ACCURACY	0.0909090909091	0	0.0454545454545
P1 E ?: Q1 Q2 P2 A ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
P1 E ?: Q1 q!2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
P1 E ?: Q1 q!2 P2 A ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
P1 E ?: q!1 Q2 P2 A ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
P1 E ?: q!1 q!2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
P1 E ?: q!1 q!2 P2 A ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 ?: Q1 q~2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 ?: Q1 q!2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 ?: q!1 q~2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 ?: q!1 q!2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: Q1 Q2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: Q1 Q2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: Q1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: Q1 q~2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: Q1 q!2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: Q1 q!2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: q!1 Q2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: q!1 Q2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: q!1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: q!1 q~2 P2 S ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: q!1 q!2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: q!1 q!2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: Q1 Q2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: Q1 Q2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: Q1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: Q1 q~2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: Q1 q~2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: q!1 Q2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: q!1 Q2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: q!1 q~2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: q!1 q~2 P2 R ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: q!1 q~2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 ?: Q1 P2 S ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 ?: Q1 P2 S ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 ?: q!1 P2 S ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
E P1 ?: q!1 P2 S ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: Q1 P2 R ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: Q1 P2 R ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: Q1 P2 S ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: Q1 P2 S ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: q!1 P2 R ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: q!1 P2 R ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: q!1 P2 S ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
A P1 ?: q!1 P2 S ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: Q1 P2 E ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: Q1 P2 E ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: Q1 P2 R ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: Q1 P2 R ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: q!1 P2 E ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: q!1 P2 E ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: q!1 P2 R ?: Q2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
S P1 ?: q!1 P2 R ?: q!2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0.0454545454545	0.0454545454545
Q1 Q2 E P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0	0.0454545454545
Q1 q~2 E P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 Q2 E P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0	0.0454545454545
q!1 q~2 E P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.0909090909091	0	0.0454545454545
q~1 P R q!2 > Q1.s=P Q2.s=P	ACCURACY	0.0909090909091	0	0.0454545454545
q~1 R P q!2 > Q1.s=P Q2.s=P	ACCURACY	0.0909090909091	0	0.0454545454545
R P ?: Q ?. > Q.s=P	ACCURACY	0.0869565217391	0	0.0434782608696
R P ?: q! ?. > Q.s=P	ACCURACY	0.0869565217391	0	0.0434782608696
E P1 ?: Q1 q!2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0434782608696	0.0434782608696
E P1 ?: q!1 q!2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0434782608696	0.0434782608696
S P1 ?: Q1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0434782608696	0.0434782608696
S P1 ?: q!1 Q2 P2 E ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0434782608696	0.0434782608696	0.0434782608696
q! P R ?. > Q.s=P	ACCURACY	0.0689655172414	0	0.0344827586207
A P Q ?. > Q.s=P	ACCURACY	0.0689655172414	0	0.0344827586207
A P q! ?. > Q.s=P	ACCURACY	0.0689655172414	0	0.0344827586207
A P ?: Q ?. > Q.s=P	ACCURACY	0.0555555555556	0	0.0277777777778
A P ?: q! ?. > Q.s=P	ACCURACY	0.0555555555556	0	0.0277777777778
Q T P ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
q! R P ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
q! T P ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
P E q~ ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
P S q~ ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
E q~ P ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
S q~ P ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
R P Q ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
R P q! ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
T P Q ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
T P q! ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
Q1 A P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
Q1 A P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
Q1 A P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
Q1 A P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
q!1 E P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
q!1 E P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
q!1 E P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
q!1 E P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
q!1 A P1 ?. Q2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
q!1 A P1 ?. Q2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
q!1 A P1 ?. q~2 E P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
q!1 A P1 ?. q~2 S P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
P1 S Q1 ?. P2 E Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
P1 S Q1 ?. P2 E q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
P1 S Q1 ?. P2 S Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
P1 S Q1 ?. P2 S q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
P1 S q!1 ?. P2 E Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
P1 S q!1 ?. P2 E q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
P1 S q!1 ?. P2 S Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
P1 S q!1 ?. P2 S q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
S Q1 P1 ?. E Q2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
S Q1 P1 ?. E q!2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
S Q1 P1 ?. S Q2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
S Q1 P1 ?. S q!2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
S q!1 P1 ?. E Q2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
S q!1 P1 ?. E q!2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
S q!1 P1 ?. S Q2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
S q!1 P1 ?. S q!2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.047619047619	0	0.0238095238095
P E ?: q~ ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
P S ?: q~ ?. > Q.s=P	ACCURACY	0.047619047619	0	0.0238095238095
Q1 Q2 R P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0	0.0238095238095
Q1 Q2 S P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0	0.0238095238095
Q1 q~2 R P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0	0.0238095238095
Q1 q~2 S P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0	0.0238095238095
q!1 Q2 R P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0	0.0238095238095
q!1 Q2 S P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0	0.0238095238095
q!1 q~2 R P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0	0.0238095238095
q!1 q~2 S P ?. q~3 > Q2.s=P Q1.l=P Q3.l=P	ACCURACY	0.047619047619	0	0.0238095238095
Q1 P1 E ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 P1 E ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 P1 E ?. q!2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 P1 E ?. q!2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 P1 E ?. P2 Q2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 P1 E ?. P2 Q2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 P1 E ?. P2 q!2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 P1 E ?. P2 q!2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 P1 E ?. P2 Q2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 P1 E ?. P2 Q2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 P1 E ?. P2 q!2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 P1 E ?. P2 q!2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q!1 P1 E ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q!1 P1 E ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q!1 P1 E ?. q!2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q!1 P1 E ?. q!2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 E P1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 E P1 ?. q!2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 E P1 ?. E P2 Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 E P1 ?. E P2 q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. q!2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. E P2 Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. E P2 q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. q!2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. E P2 Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. E P2 q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. q!2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. E P2 Q2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. E P2 q!2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
E P1 Q1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
E P1 Q1 ?. q~2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
E P1 q!1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
E P1 q!1 ?. q~2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
A P1 Q1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
A P1 Q1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
A P1 Q1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
A P1 Q1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
A P1 q!1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
A P1 q!1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
A P1 q!1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
A P1 q!1 ?. q~2 P2 S > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 Q1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 Q1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 Q1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 Q1 ?. q~2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 Q1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 Q1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 q!1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 q!1 ?. Q2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 q!1 ?. q~2 P2 E > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 q!1 ?. q~2 P2 R > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 q!1 ?. Q2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
S P1 q!1 ?. q~2 R P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 E P1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 E P1 ?. q!2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 E P1 ?. E P2 Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 E P1 ?. E P2 q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. q!2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. E P2 Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 S P1 ?. E P2 q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. q!2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. E P2 Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 E P1 ?. E P2 q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. Q2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. Q2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. q!2 P2 E > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. q!2 P2 S > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. E P2 Q2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
q~1 S P1 ?. E P2 q!2 > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P1 E ?: Q1 Q2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P1 E ?: Q1 q~2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P1 E ?: q!1 Q2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P1 E ?: q!1 q~2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P1 S ?: Q1 Q2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P1 S ?: Q1 q~2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P1 S ?: q!1 Q2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P1 S ?: q!1 q~2 R P2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 Q2 P A ?. > Q2.s=P Q1.l=P	ACCURACY	0.0454545454545	0	0.0227272727273
Q1 q!2 P A ?. > Q2.s=P Q1.l=P	ACCURACY	0.0454545454545	0	0.0227272727273
q!1 Q2 P A ?. > Q2.s=P Q1.l=P	ACCURACY	0.0454545454545	0	0.0227272727273
q!1 q!2 P A ?. > Q2.s=P Q1.l=P	ACCURACY	0.0454545454545	0	0.0227272727273
P1 ?V P2 ?V . Q1 q~2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P1 ?V P2 ?V . q!1 q~2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P2 ?V P1 ?V . Q1 q~2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
P2 ?V P1 ?V . q!1 q~2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1	ACCURACY	0.0454545454545	0	0.0227272727273
E Q1 P1 ?. E Q2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0434782608696	0	0.0217391304348
E Q1 P1 ?. E q!2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0434782608696	0	0.0217391304348
E Q1 P1 ?. S Q2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0434782608696	0	0.0217391304348
E Q1 P1 ?. S q!2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0434782608696	0	0.0217391304348
E q!1 P1 ?. E Q2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0434782608696	0	0.0217391304348
E q!1 P1 ?. E q!2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0434782608696	0	0.0217391304348
E q!1 P1 ?. S Q2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0434782608696	0	0.0217391304348
E q!1 P1 ?. S q!2 P2 > Q1.s=P1 Q2.s=P2	ACCURACY	0.0434782608696	0	0.0217391304348
P A ?: Q ?. > Q.s=P	ACCURACY	0.0434782608696	0	0.0217391304348
P A ?: q! ?. > Q.s=P	ACCURACY	0.0434782608696	0	0.0217391304348
'''.splitlines()
    data = [i.split('\t') for i in data]
    return dict([(i[0],(i[2],i[3])) for i in data])

def load_weights():
    data = '''P E Q > Q.s=P	ACCURACY	1.0	1.0	0.75641025641	COVERAGE	0.0543175487465	0.0294550810015	0.042233357194	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	12	9	0	0	0	MATCHES	39	20
S P Q > Q.s=P	ACCURACY	0.833333333333	1.0	0.601851851852	COVERAGE	0.075208913649	0.0294550810015	0.0529706513958	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	22	3	0	0	0	MATCHES	45	20
E P : Q > Q.s=P	ACCURACY	0.444444444444	1.0	0.407407407407	COVERAGE	0.075208913649	0.0294550810015	0.0529706513958	SKIPPED	0.0	0.0543175487465	CONFLICTS	2	13	19	0	0	0	MATCHES	24	20
q!1 q~ S P q!2 > Q.s=P Q1.s=P Q2.s=P	ACCURACY	0.818181818182	1.0	0.636363636364	COVERAGE	0.0612813370474	0.0294550810015	0.0458124552613	SKIPPED	0.0	0.0543175487465	CONFLICTS	5	15	2	0	0	0	MATCHES	36	20
q!1 q!2 P A > q!1.l=P q!2.s=P	ACCURACY	0.954545454545	0.909090909091	0.931818181818	COVERAGE	0.0306406685237	0.0324005891016	0.0314960629921	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	4	0	0	-3	4	MATCHES	21	20
E P Q > Q.s=P	ACCURACY	0.710144927536	1.0	0.5	COVERAGE	0.0961002785515	0.0294550810015	0.0637079455977	SKIPPED	0.0	0.0543175487465	CONFLICTS	5	22	7	0	0	0	MATCHES	49	20
Q P S > Q.s=P	ACCURACY	0.897959183673	1.0	0.65306122449	COVERAGE	0.0682451253482	0.0294550810015	0.0493915533286	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	14	5	0	0	0	MATCHES	44	20
P1 A P2 : Q > Q.s=P1 Q.l=P2	ACCURACY	0.962962962963	1.0	0.981481481481	COVERAGE	0.0376044568245	0.039764359352	0.0386542591267	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	8	13	0	8	13	MATCHES	26	27
T P ?: Q > Q.s=P Q.l=P	ACCURACY	1.0	1.0	1.0	COVERAGE	0.0320334261838	0.0338733431517	0.032927702219	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	3	3	0	3	3	MATCHES	23	23
P Q S > Q.s=P	ACCURACY	0.909090909091	1.0	0.909090909091	COVERAGE	0.0306406685237	0.0294550810015	0.0300644237652	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	3	4	0	0	0	MATCHES	20	20
q~ S P q! > q~.s=P q!.s=P	ACCURACY	1.0	1.0	0.647058823529	COVERAGE	0.0947075208914	0.0294550810015	0.0629921259843	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	22	0	0	0	0	MATCHES	68	20
T Q P ?. > Q.s=P Q.l=P	ACCURACY	1.0	0.909090909091	0.954545454545	COVERAGE	0.0306406685237	0.0324005891016	0.0314960629921	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	2	2	0	2	2	MATCHES	22	20
P2 E P1 : Q > Q.s=P1 Q.l=P2	ACCURACY	0.5	0.558139534884	0.522727272727	COVERAGE	0.0612813370474	0.0633284241532	0.0622763063708	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	12	19	0	12	19	MATCHES	22	24
Q P E > Q.s=P	ACCURACY	0.772727272727	1.0	0.5	COVERAGE	0.122562674095	0.0294550810015	0.0773085182534	SKIPPED	0.0	0.0543175487465	CONFLICTS	25	14	8	0	0	0	MATCHES	68	20
P S Q > Q.s=P	ACCURACY	1.0	1.0	0.833333333333	COVERAGE	0.041782729805	0.0294550810015	0.0357909806729	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	11	6	0	0	0	MATCHES	30	20
P E : Q > Q.s=P	ACCURACY	0.71186440678	1.0	0.525423728814	COVERAGE	0.0821727019499	0.0294550810015	0.0565497494631	SKIPPED	0.0	0.0543175487465	CONFLICTS	6	9	1	0	0	0	MATCHES	42	20
P1 E ?: q!1 P2 E ?: q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1	ACCURACY	0.933333333333	0.9	0.916666666667	COVERAGE	0.041782729805	0.0441826215022	0.0429491768074	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	8	2	0	8	2	MATCHES	28	27
P1 E P2 ?: Q > Q.s=P1 Q.l=P2	ACCURACY	0.898305084746	1.0	0.940677966102	COVERAGE	0.0821727019499	0.0854197349043	0.0837508947745	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	12	28	0	12	28	MATCHES	53	58
P1 E ?: q!1 P2 R ?: q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1	ACCURACY	0.958333333333	0.958333333333	0.958333333333	COVERAGE	0.033426183844	0.0353460972018	0.034359341446	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	4	0	0	4	0	MATCHES	23	23
S P : Q > Q.s=P	ACCURACY	0.526315789474	1.0	0.526315789474	COVERAGE	0.0529247910864	0.0294550810015	0.0415175375805	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	9	12	0	0	0	MATCHES	20	20
Q T ?: P > Q.s=P Q.l=P	ACCURACY	1.0	1.0	1.0	COVERAGE	0.0292479108635	0.0309278350515	0.0300644237652	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	1	0	0	1	0	MATCHES	21	21
P2 A P1 : Q > Q.s=P1 Q.l=P2	ACCURACY	0.740740740741	0.777777777778	0.759259259259	COVERAGE	0.0376044568245	0.039764359352	0.0386542591267	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	8	13	0	8	13	MATCHES	20	21
P T ?: Q > Q.s=P Q.l=P	ACCURACY	1.0	0.954545454545	0.977272727273	COVERAGE	0.0306406685237	0.0324005891016	0.0314960629921	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	2	2	0	2	2	MATCHES	22	21
Q E P > Q.s=P	ACCURACY	0.969387755102	1.0	0.586734693878	COVERAGE	0.136490250696	0.0294550810015	0.084466714388	SKIPPED	0.0	0.0543175487465	CONFLICTS	17	32	0	0	0	0	MATCHES	95	20
P Q E > Q.s=P	ACCURACY	0.909090909091	1.0	0.909090909091	COVERAGE	0.0306406685237	0.0294550810015	0.0300644237652	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	3	4	0	0	0	MATCHES	20	20
q~ S P > q~.s=P	ACCURACY	0.985507246377	1.0	0.63768115942	COVERAGE	0.0961002785515	0.0294550810015	0.0637079455977	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	30	0	0	0	0	MATCHES	68	20
Q1 E P1 Q2 R P2 > Q1.s=P1 Q1.l=P2 Q1.s=P1 Q1.l=P2	ACCURACY	1.0	1.0	1.0	COVERAGE	0.0278551532033	0.0294550810015	0.0286327845383	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	0	0	0	0	0	MATCHES	20	20
P1 E P2 : Q > Q.s=P1 Q.l=P2	ACCURACY	0.886363636364	1.0	0.931818181818	COVERAGE	0.0612813370474	0.0633284241532	0.0622763063708	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	12	19	0	12	19	MATCHES	39	43
Q P T ?. > Q.s=P Q.l=P	ACCURACY	1.0	0.95652173913	0.978260869565	COVERAGE	0.0320334261838	0.0338733431517	0.032927702219	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	3	0	0	3	0	MATCHES	23	22
P R q! > q!.s=P	ACCURACY	1.0	1.0	0.916666666667	COVERAGE	0.033426183844	0.0294550810015	0.0314960629921	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	4	2	0	0	0	MATCHES	24	20
P1 A ?: q!1 P2 R ?: q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1	ACCURACY	1.0	1.0	1.0	COVERAGE	0.0278551532033	0.0294550810015	0.0286327845383	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	0	0	0	0	0	MATCHES	20	20
S Q P > Q.s=P	ACCURACY	0.888888888889	1.0	0.814814814815	COVERAGE	0.0376044568245	0.0294550810015	0.0336435218325	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	4	12	0	0	0	MATCHES	24	20
P Q T ?. > Q.s=P Q.l=P	ACCURACY	1.0	1.0	1.0	COVERAGE	0.0278551532033	0.0294550810015	0.0286327845383	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	0	0	0	0	0	MATCHES	20	20
P S q! > q!.s=P	ACCURACY	1.0	1.0	0.844827586207	COVERAGE	0.0403899721448	0.0294550810015	0.0350751610594	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	13	6	0	0	0	MATCHES	29	20
E Q P > Q.s=P	ACCURACY	0.764705882353	1.0	0.676470588235	COVERAGE	0.0473537604457	0.0294550810015	0.0386542591267	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	5	19	0	0	0	MATCHES	26	20
P1 E ?: q!1 q!2 P2 E > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1	ACCURACY	0.954545454545	0.954545454545	0.954545454545	COVERAGE	0.0306406685237	0.0324005891016	0.0314960629921	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	2	0	0	2	0	MATCHES	21	21
Q S P > Q.s=P	ACCURACY	0.975308641975	1.0	0.611111111111	COVERAGE	0.112813370474	0.0294550810015	0.0722977809592	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	64	0	0	0	0	MATCHES	158	40
P S : Q > Q.s=P	ACCURACY	0.666666666667	1.0	0.555555555556	COVERAGE	0.0626740947075	0.0294550810015	0.0465282748747	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	7	0	0	0	0	MATCHES	30	20
P1 A ?: q!1 q!2 P2 R > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1	ACCURACY	1.0	1.0	1.0	COVERAGE	0.0278551532033	0.0294550810015	0.0286327845383	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	0	0	0	0	0	MATCHES	20	20
P A : Q > Q.s=P	ACCURACY	0.913043478261	1.0	0.891304347826	COVERAGE	0.0320334261838	0.0294550810015	0.0307802433787	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	3	1	0	0	0	MATCHES	21	20
P1 S P2 : Q > Q.s=P1 Q.l=P2	ACCURACY	0.903225806452	1.0	0.935483870968	COVERAGE	0.0431754874652	0.0441826215022	0.0436649964209	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	8	12	0	8	12	MATCHES	28	30
P1 S q!1 P2 R q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1	ACCURACY	1.0	1.0	1.0	COVERAGE	0.0278551532033	0.0294550810015	0.0286327845383	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	0	0	0	0	0	MATCHES	20	20
Q1 S P1 Q2 R P2 > Q1.s=P1 Q1.l=P2 Q1.s=P1 Q1.l=P2	ACCURACY	1.0	1.0	1.0	COVERAGE	0.0278551532033	0.0294550810015	0.0286327845383	SKIPPED	0.0	0.0543175487465	CONFLICTS	0	0	0	0	0	0	MATCHES	20	20
'''.splitlines()
    data = [i.split('\t') for i in data]
    return dict([(i[0],(i[2],i[3])) for i in data])

def load_auto_rules():
    d = {'Q': ['Q','q~','q!'],
         'E': ['E','A','R','T','S'],
         }
    g = [
        minilang_permutations(['{Q}', 'P', '{E}']) + ' ?. > Q.s=P',
        minilang_permutations(['{Q}1', 'P1', '{E}']) + ' ?. ' + minilang_permutations(['{Q}2', 'P2', '{E}']) + ' > Q1.s=P1 Q2.s=P2',
        minilang_permutations(['{Q}1', 'P1', '{E}']) + ' ?. ' + minilang_permutations(['{Q}2', 'P2', '{E}']) + ' > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1',
        '(P {E}|{E} P) ?: {Q} ?. > Q.s=P',
        '(P1 {E}|{E} P1) ?: {Q}1 {Q}2 (P2 {E}|{E} P2) ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1',
        '(P1 {E}|{E} P1) ?: {Q}1 (P2 {E}|{E} P2) ?: {Q}2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1',
        '{Q}1 {Q}2 (P {E}|{E} P) ?. > Q2.s=P Q1.l=P',
        '{Q}1 {Q}2 (P {E}|{E} P) ?. {Q}3 > Q2.s=P Q1.l=P Q3.l=P',
        'P1 {E} P2 ?: Q > Q.s=P1 Q.l=P2',
        'q~1 (P {E}|{E} P) q!2 > Q1.s=P Q2.s=P',
        'P ?P2 ?V . {Q} > Q.s=P',
        'P ?V . {Q} > Q.s=P',
        'P1 ?V P2 ?V . {Q} > Q.s=P1 Q.l.P2',
        'P2 ?V P1 ?V . {Q} > Q.s=P1 Q.l.P2',
        'P1 ?V P2 ?V . {Q}1 {Q}2 > Q1.s=P1 Q1.l.P2 Q2.s=P2 Q2.l.P1',
        'P2 ?V P1 ?V . {Q}1 {Q}2 > Q1.s=P1 Q1.l.P2 Q2.s=P2 Q2.l.P1',
        'P ?E ?V ?V ?V ?: {Q} > Q.s=P',
        'P1 ?V (V|E) ?V P2 ?: {Q} > Q.s=P1 Q.l=P2',
        'P2 ?V (V|E) ?V P1 ?: {Q} > Q.s=P1 Q.l=P2',
    ]
    rules = utterancegenerator.UtteranceGenerator().generate(g, d, verbose=False)
    return [QuotedSpeechPredictorRule.from_string(i) for i in rules]

def load_rules_from_weights_from_auto():
    rules = load_weights_from_auto().keys()
    return [QuotedSpeechPredictorRule.from_string(i) for i in rules]

import re
import utterancegenerator
import itertools

def minilang_permutations(lst):
    return '('+'|'.join([' '.join(list(i)) for i in itertools.permutations(lst)])+')'


def main_gene_minilang():

    d = {'Q': ['Q','q~','q!'],
         'E': ['E','A','R','T','S'],
         }
    g = [
        minilang_permutations(['{Q}', 'P', '{E}']) + ' (?.|)',
        minilang_permutations(['{Q}', 'P', '{E}']) + ' ' + minilang_permutations(['{Q}', 'P', '{E}']),
        '(P {E}|{E} P) (?:|:|) {Q} (?.|)',
        '(P {E}|{E} P) (?:|:|) {Q} {Q} (P {E}|{E} P) (?.|)',
        '(P {E}|{E} P) (?:|:|) {Q} (P {E}|{E} P) (?:|:|) {Q} (?.|)',
        '{Q} {Q} (P {E}|{E} P) (?.|)',
        '{Q} {Q} (P {E}|{E} P) (?.|) {Q}',
        'P {E} P (?:|:|) Q',
        'q~ (P {E}|{E} P) q!',
    ]

    results = utterancegenerator.UtteranceGenerator().generate(g, d, verbose=False)
    results = [i.strip() for i in results]
    print 'generated rules',len(results)
    rules = [re.sub('\d','',i.split('>')[0].strip()) for i in load_weights().keys()]
    print 'generated',len(results),len(set(rules)&set(results))
    print 'missing',len(set(rules)-set(results))
    for i in set(rules)-set(results):
        print i


def main_print_stuff():
    len_quotes = 0
    for story_file in settings.STY_FILES:
        print story_file
        doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH+story_file)
        #styhelper.fix_sty_annotations(doc)
        quotedspeechhelper.annotate_sentences(doc, settings.STORY_ALL_SENTENCES, single_sentences_file_story_id = doc.id)
        output_tuple = tokenize_document(doc)
        output, quotes, mentions, verbs = output_tuple
        print tokenized_string_to_string(output, 1)
        len_quotes += len(quotes)
    print 'TOTAL NUM QUOTES ', len_quotes

if __name__=='__main__':
    main_extract()
    #main_all()
    #main_print_stuff()
    #main_minilang_parser()
    #main_gene_minilang()