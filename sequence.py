import logging
import settings
import stanfordhelper
import voz
import verbmanager
import collections
import sys
import os
import re
import verbmanager


def print_verb_sequence(doc=None,include_coref=True,filter_characters=False,include_na=True,print_labels=True,verb_glue=';',field_separator='\t'):
    assert isinstance(doc,voz.Document)
    all_coref_mentions = set()
    nodes = []
    for i in doc.get_all_mentions(filter_only_independent=True):
        if not i.predictions.coref:
            i.predictions.coref = i.id
        if i.predictions.coref not in all_coref_mentions and (not filter_characters or i.predictions.is_character()):
            all_coref_mentions.add(i.predictions.coref)
            nodes.append(i)

    for i_i,i in enumerate(nodes):
        print i_i, i.predictions.coref, i.predictions.type, i.predictions.role,
        if include_coref and doc:
            coref = [k for k in doc.get_all_mentions(filter_only_independent=True) if k.predictions.coref==i.predictions.coref]
            for j in coref:
                print '\t',j, j.tokens[0]._parent_sentence.idx, j.tokens[0].idx
        else:
            print i, i.tokens[0]._parent_sentence.idx, i.tokens[0].idx

    verbs = []
    for sentence in doc.sentences:
        verbs += sentence.verbs

    def filter_mentions(mentions):
        mm = []
        verbmanager.add_children_mentions_to_list(mentions,mm)
        mm = [i.predictions.coref for i in mm]
        mm = [i for i in mm if i in all_coref_mentions]
        return sorted(set(mm)) or [-1]

    tokens = doc.get_all_tokens()
    for verb in verbs:
        assert isinstance(verb,verbmanager.Verb)
        subjects = filter_mentions(verb.get_subjects())
        objects = filter_mentions(verb.get_objects())
        print verb.get_text(), 1.0 * tokens.index(verb.token) / len(tokens), ';'.join([str(i) for i in subjects]), ';'.join([str(i) for i in objects])

def clear_coref(doc):
    for i in doc.get_all_mentions(filter_only_independent=True):
        i.predictions.coref = i.id


def main():
    NO_COREF = False
    filename = None
    if len(sys.argv)>=2:
        filename = sys.argv[1]
        if not os.path.isfile(filename):
            print "provide a path to a txt file"
            sys.exit()
    else:
        print "provide a path to a txt file"
        sys.exit()

    text = re.sub(r"\".*?\"","QUOTE",open(filename).read())
    doc = stanfordhelper.create_document_from_raw_text(text,{'cache':False})
    doc.compute_predictions()
    if NO_COREF: clear_coref(doc)
    print_verb_sequence(doc)

if __name__ == '__main__':
    main()