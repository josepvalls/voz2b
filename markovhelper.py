import random

class Markov(object):
    def __init__(self, corpus, n_grams, min_length):
        """
        corpus = list of string text ["speech1", "speech2", ..., "speechn"]
        n_grams = max sequence length
        min_length = minimum number of next words required for back-off scheme
        """
        self.grams = {}
        self.n_grams = n_grams
        self.corpus = corpus
        self.min_length = min_length
        self.sequences()

    def tokenizer(self, gram):
        """tokenize speeches in corpus, i.e. split speeches into words"""

        if len(self.corpus) < gram:
            pass
        else:
            for i in range(len(self.corpus) - gram):
                yield (self.corpus[i:i + (gram + 1)])

    def sequences(self):
        """
        create all sequences of length up to n_grams
        along with the pool of next words.
        """
        for gram in range(1, self.n_grams + 1):
            dictionary = {}
            for sequence in self.tokenizer(gram):
                key_id = tuple(sequence[0:-1])

                if key_id in dictionary:
                    dictionary[key_id].append(sequence[gram])
                else:
                    dictionary[key_id] = [sequence[gram]]
            self.grams[gram] = dictionary

    def next_word(self, key_id):
        """returns the next word for an input sequence
        but backs off to shorter sequence if length
        requirement is not met.
        """
        for i in range(len(key_id)):
            try:
                if len(self.grams[len(key_id)][key_id]) >= self.min_length:
                    return random.choice(self.grams[len(key_id)][key_id])
            except KeyError:
                pass
        # if the length requirement isn't met, we shrink the key_id
            if len(key_id) > 1:
                key_id = key_id[1:]
        # when we're down to only a one-word sequence,
        #ignore the requirement
        try:
            return random.choice(self.grams[len(key_id)][key_id])
        except KeyError:
            # key does not exist: should only happen when user enters
            # a sequence whose last word was not in the corpus
            # choose next word at random
            return random.choice(' '.join(self.corpus).split())

    def next_key(self, key_id, res):
        return tuple(key_id[1:]) + tuple([res])

    def generate_markov_text(self, start, size=25):
        """"start is a sentence of at least n_grams words"""
        key_id = tuple(start)[ - self.n_grams:]
        gen_words = []
        i = 0
        while i <= size:
            result = self.next_word(key_id)
            key_id = self.next_key(key_id, result)
            gen_words.append(result)
            i += 1
        return gen_words


def do_markov_main():
    train = []
    test = []
    train_lens = []
    words_to_generate = 100
    for idx in range(100):
        #train += [i.strip() for i in open("training stories/training_story_%d_full.txt" % (idx + 1), 'r').readlines()]
        train += [i.strip() for i in open("training stories/training_story_%d_%d.txt" % (idx + 1, 2), 'r').readlines()]
        train_lens.append(len(open("training stories/training_story_%d_%d.txt" % (idx + 1, 2), 'r').readlines()))
        test.append([i.strip() for i in open("training stories/training_story_%d_%d.txt" % (idx + 1, 1), 'r').readlines()])
    print 1.0*sum(train_lens)/len(train_lens)
    return
    for seed in test:
        #content_model = ngram.BaseNgramModel(3, train)
        #content = content_model.generate(words_to_generate, seed)
        mark = Markov(train, 5, 3)
        content = mark.generate_markov_text(seed, size=words_to_generate)
        #' '.join(gen_words).replace(' .', '.').replace(' ,', ',')
        print 'START'
        print ' '.join(seed)
        print 'CONTINUE'
        print ' '.join(content)


def main():
    do_markov_main()

if __name__=='__main__':
    main()