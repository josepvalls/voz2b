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