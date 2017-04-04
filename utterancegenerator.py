import random
class UtteranceGenerator(object):
    # @TODO unfolod recursive calls using a stack and convert all this to a generator
    class ListOptions(list):
        def __repr__(self):
            return 'OP' + list.__repr__(self)

    class ListSequence(list):
        def __repr__(self):
            return 'SEQ<' + '-'.join([str(i) for i in self]) + '>'
    @classmethod
    def consume(cls, lst, until_token=None):
        ret = ''
        while lst:
            i = lst.pop(0)
            if i == until_token:
                break
            else:
                ret += i
        return ret
    @classmethod
    def parse(cls, lst, dictionary):
        options = UtteranceGenerator.ListOptions()
        current = UtteranceGenerator.ListSequence([''])
        while lst:
            i = lst.pop(0)
            if i == '(':
                current.append(UtteranceGenerator.parse(lst, dictionary))
                current.append('')
            elif i == ')':
                options.append(current)
                return options
            elif i == '|':
                options.append(current)
                current = UtteranceGenerator.ListSequence([''])
            elif i == '{':
                options_ = UtteranceGenerator.ListOptions()
                dictionary_name = UtteranceGenerator.consume(lst, '}')
                if dictionary_name in dictionary:
                    for j in dictionary.get(dictionary_name, []):
                        options_.append(UtteranceGenerator.ListSequence([j]))
                else:
                    options_.append(UtteranceGenerator.ListSequence(['{%s}' % dictionary_name]))
                current.append(options_)
                current.append('')
            elif i == '}':
                pass  # this will be consumed and not in the string
            else:
                current[-1] += i
        options.append(current)
        return options

    @classmethod
    def _generate(cls, seq, current=None, lst=[], trim_whitespace=True):
        ret = []
        current_ = current or ['']

        if not seq:
            return current_
        else:
            for current_i in current_:
                car = seq[0]
                cdr = seq[1:]
                if isinstance(car, str):
                    if trim_whitespace and car and current_i and car.startswith(' ') and current_i.endswith(' '):
                        car = car[1:]
                    ret.append(current_i + car)
                elif isinstance(car, UtteranceGenerator.ListOptions):
                    for option in car:
                        ret_ = UtteranceGenerator._generate(option, trim_whitespace=trim_whitespace)
                        for i in ret_:
                            if trim_whitespace and i and current_i and i.startswith(' ') and current_i.endswith(
                                    ' '):
                                i = i[1:]
                            ret.append(current_i + i)
            return UtteranceGenerator._generate(cdr, ret, trim_whitespace=trim_whitespace)
    @classmethod
    def generate(cls, grammar, dictionary={}, trim_whitespace=True, verbose=False):
        results = []
        for line in grammar:
            p = UtteranceGenerator.ListSequence([UtteranceGenerator.parse(list(line), dictionary)])
            if verbose:
                print p
            for i in UtteranceGenerator._generate(p,trim_whitespace=trim_whitespace):
                if verbose:
                    print ' ', i
                results.append(i)
        return results
    @classmethod
    def _sample(cls, item, r, trim_whitespace=True):
        if isinstance(item, UtteranceGenerator.ListSequence):
            if trim_whitespace:
                return ' '.join(filter(None,[UtteranceGenerator._sample(i, r, trim_whitespace=trim_whitespace).strip() for i in item]))
            else:
                return ''.join([UtteranceGenerator._sample(i, r, trim_whitespace=trim_whitespace) for i in item])


        elif isinstance(item, str):
            return item
        elif isinstance(item, UtteranceGenerator.ListOptions):
            return UtteranceGenerator._sample(r.choice(item), r, trim_whitespace=trim_whitespace)
    @classmethod
    def sample(cls, grammar, dictionary={}, trim_whitespace=True, verbose=False, r=None):
        if not r: r = random.Random()
        p = UtteranceGenerator.ListSequence([UtteranceGenerator.parse(list(r.choice(grammar)), dictionary)])
        return UtteranceGenerator._sample(p,  r, trim_whitespace=trim_whitespace)

import re
def fix(e):
    e = re.sub(r'\s([\.\,\?\!])', r'\1', e.strip())
    e = e[0].upper() + e[1:]
    e = re.sub(r'([\.\?\!] )(\w)',lambda match: match.group(1).upper()+match.group(2).upper() ,e)
    return e

def main():
    d = {
        'name': ['battleship', 'battle ship', 'naval battle'],
    }
    g = [
        'play {name}',
        'start ((|a) (|new) (game|board) (|of {name})|{name})',
        'restart ( |the|the game) {name}',
        '{a} (and|or) {b}',
        "((hello|hi).|) (tell me|) how are you (feeling|) (today|)?"
        ]
    results = UtteranceGenerator().generate(g, d, verbose=True)
    print 'EXHAUSTIVE LIST'
    for i in results:
        print fix(i)
    print 'SAMPLING'
    for _ in range(10):
        print UtteranceGenerator().sample(g, d, verbose=False, r=None)
    print 'NOT RANDOM'
    class NotRandom(random.Random):
        def choice(self, seq):
            return seq[-1]
    for _ in range(3):
        print UtteranceGenerator().sample(g, d, verbose=False, r=NotRandom())



if __name__=='__main__':
    main()