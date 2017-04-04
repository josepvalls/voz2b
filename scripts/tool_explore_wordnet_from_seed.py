import nltkhelper as nltkhelper
import util

def explore(seed):
    closed = set([])
    open = nltkhelper.nltk_wordnet.query(seed)
    while open:
        s = open.pop()
        if s not in closed:
            open += s.hyponyms()
            closed.add(s)
    lemmas = [j.name() for j in set(util.flatten([i.lemmas() for i in closed])) if '_' not in j.name()]
    lemmas = set(lemmas)
    print seed,len(lemmas),lemmas
#sss.hyponyms()
#sss.hypernyms()

for s in ['communicate','talk','say','ask','think','reply']:
    explore(s)

