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
from quotedbase import *
import itertools

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

def weighted_assignment(input_tuple,weights,top_weight_mechanism=0):
    output, quotes, mentions, verbs = input_tuple
    def top(items,slot):
        return sorted(items,key=lambda i:weights[i[0]][slot])[-1][1]
    def weight(items,slot):
        cumm_weights = {}
        total = 0.0
        for rule_name,mention in items:
            cumm_weights[mention] = cumm_weights.get(mention,0.0) + weights[rule_name][slot]
        return sorted(cumm_weights.items(),key=lambda i:i[1])[-1][0]
    def bayes(items,slot):
        mentions = [i[1] for i in items]
        cumm_weights = dict([(i,0.0) for i in mentions])
        for rule_name,mention in items:
            cumm_weights[mention] += weights[rule_name][slot]
            for other in mentions:
                if other==mention: continue
                cumm_weights[other] += (1.0-weights[rule_name][slot])/(len(mentions)-1)
        return sorted(cumm_weights.items(), key=lambda i: i[1])[-1][0]
    def counter(items,slot):
        return collections.Counter([i[1] for i in items]).most_common(1)[0][0]

    if top_weight_mechanism==0:
        selector = top
    elif top_weight_mechanism==1:
        selector = weight
    elif top_weight_mechanism==2:
        selector = bayes
    elif top_weight_mechanism==3:
        selector = counter

    for quote in quotes:
        quote.speaker_mention = None
        quote.listener_mention = None
        if quote.speaker_mentions:
            quote.speaker_mention = selector(quote.speaker_mentions.items(),0)
        if quote.listener_mentions:
            quote.listener_mention = selector(quote.listener_mentions.items(),1)

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
    def __eq__(self,other):
        return hash(self)==hash(other) and str(self)==str(other)
    def __ne__(self,other):
        return not self == other
    def __hash__(self):
        return hash(str(self))

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
    if quote not in token_to_string_dict:
        pass
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

def extract_rules(input, windows_size=8):
    rules = []
    missing = []
    output, quotes, mentions, verbs = input
    for token_i, token in enumerate(output):
        if isinstance(token,Quote):
            rules_ = []
            for before in range(0,windows_size):
                for after in range(1,windows_size):
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

def main_minilang_parser_test():
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

def minilang_permutations(lst):
    return '('+'|'.join([' '.join(list(i)) for i in itertools.permutations(lst)])+')'


def load_rules_manual():
    # example rules
    # Q S P > Q.s=P
    #   when Quote Said Person is found, assign Person to Quote.speaker
    # Q1 S P1 Q2 R P2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1:
    #   when Quote1 Said Person1 Quote2 Replied Person2 is found, assign Person1 to Quote1.speaker, Person2 to Quote1.listener...
    rules = [
        'Q S P > Q.s=P',
        'Q P S > Q.s=P',
        'P S Q > Q.s=P',
        'S P Q > Q.s=P',
        'S Q P > Q.s=P',
        'P Q S > Q.s=P',
        'Q1 S P1 Q2 R P2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1',
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
        'Q1 E P1 Q2 R P2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1',
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
    rules_ = [QuotedSpeechPredictorRule.from_string(i) for i in rules]
    rules = [
        'Q1 Q2 > Q1.l=Q2.s Q2.l=Q1.s',
        'Q2 Q1 > Q1.l=Q2.s Q2.l=Q1.s',
        'Q1 ?V ?P ?V ?P Q2 > Q1.l=Q2.s Q2.l=Q1.s',
        'Q2 ?V ?P ?V ?P Q1 > Q1.l=Q2.s Q2.l=Q1.s',
        'q~1 ?V ?P ?V ?P q!2 > Q2.s=Q1.s Q2.l=Q1.l',
        'q~2 ?V ?P ?V ?P q!1 > Q2.s=Q1.s Q2.l=Q1.l',
    ]
    return rules_ + [QuotedSpeechPredictorRule.from_string(i,FOLLOWUP_RULE) for i in rules]


def load_rules_from_permutation_minilang(use_qsa_subset=False):
    d = {'Q': ['Q','q~','q!'],
         'E': ['E','A','R','T','S'],
         }
    if not use_qsa_subset:
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
    else:
        g = [
        minilang_permutations(['{Q}', 'P', '{E}']) + ' ?V ?. ?: > Q.s=P',
        minilang_permutations(['{Q}1', 'P1', '{E}']) + ' ?V ?. ?: ' + minilang_permutations(
            ['{Q}2', 'P2', '{E}']) + ' > Q1.s=P1 Q2.s=P2',
        minilang_permutations(['{Q}1', 'P1', '{E}']) + ' ?V ?. ?: ' + minilang_permutations(
            ['{Q}2', 'P2', '{E}']) + ' > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1',
        ]
    rules = utterancegenerator.UtteranceGenerator().generate(g, d, verbose=False)
    return [QuotedSpeechPredictorRule.from_string(i) for i in rules]


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
    print 'generated rules',len(results),results


def main_print_stats():
    len_quotes = 0
    len_sentences = 0
    len_verbs = 0
    len_mentions = 0
    len_pp = 0
    len_pn = 0
    len_tokens = 0
    len_tokens_in_quotes = 0
    for story_file in settings.STY_FILES:
        print story_file
        doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH+story_file)
        #styhelper.fix_sty_annotations(doc)
        quotedspeechhelper.annotate_sentences(doc, settings.STORY_ALL_SENTENCES, single_sentences_file_story_id = doc.id)
        output_tuple = tokenize_document(doc)
        output, quotes, mentions, verbs = output_tuple
        print tokenized_string_to_string(output, 1)
        len_quotes += len(quotes)
        len_verbs += len(verbs)
        len_mentions += len(mentions)
        len_sentences += len(doc.sentences)
        len_pp += len([i for i in mentions if [j for j in i.tokens if j.pos == 'PRP']])
        len_pn += len([i for i in mentions if [j for j in i.tokens if j.pos == 'NNP']])
        len_tokens += len(doc.get_text())
        len_tokens_in_quotes += sum([q.offset_end-q.offset for q in quotes])
    print 'TOTAL NUM QUOTES\t', len_quotes
    print 'TOTAL NUM SENT\t', len_sentences
    print 'TOTAL NUM VERBS\t', len_verbs
    print 'TOTAL NUM MENT\t', len_mentions
    print 'TOTAL NUM PP\t', len_pp
    print 'TOTAL NUM PN\t', len_pn
    print 'TOTAL NUM chars\t', len_tokens
    print 'TOTAL NUM chars in quotes\t', len_tokens_in_quotes

if __name__=='__main__':
    main_minilang_parser_test()
    main_gene_minilang()
    main_print_stats()