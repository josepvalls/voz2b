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
import featuremanager
import stanfordhelper
import classificationhelper
import util
import collections
import coreferencehelper
import verbhelper

logger = logging.getLogger(__name__)
import sys

SKIP_PYPY = hasattr(sys, "pypy_translation_info")


def main():
    # coref_details()
    #load_documents()
    do_init_loop()
    # get_stats_docs_verbs()


DO_FORCE_DOCS_RELOAD = False  # required for SRL since verbs are not loaded otherwise
DO_FORCE_FEATURES_REWRITE = False
classificationhelper.DO_FORCE_LABEL_COMPUTE = True
featuremanager.DO_FORCE_FEATURE_RELOAD = False  # this is slow!
coreferencehelper.DO_FORCE_COREF_RELOAD = False


def coref_details():
    docs = load_documents()
    fname = generate_tsv_file(docs, classificationhelper.TASK_CHARACTER, 0)
    preds = classificationhelper.get_labels(fname, docs)
    apply_predictions(docs, classificationhelper.TASK_CHARACTER, preds, apply_ground_truth=True)
    coreferencehelper.do_improve_coref_details(docs)


def do_init_loop():
    logging.root.setLevel(logging.ERROR)
    docs = load_documents() if DO_FORCE_DOCS_RELOAD else load_documents_placeholders()
    generate_tsv_file(docs, classificationhelper.TASK_COREF, 0)
    do_loop(docs)


def do_loop(docs):
    for iteration in xrange(1):
        if True:
            # this is used to load the CH/Non-CH annotations in order to filter coref
            fname = generate_tsv_file(docs, classificationhelper.TASK_CHARACTER, 0)
            preds = classificationhelper.get_labels(fname, docs)
            apply_predictions(docs, classificationhelper.TASK_CHARACTER, preds, apply_ground_truth=True)
        print 'IX ITERATION', iteration
        print 'IA BEFOREVERBS', verbhelper.get_srl_stats_prfa(docs)
        verbhelper.do_fix_srl(docs)
        print 'IA AFTERVERBS', verbhelper.get_srl_stats_prfa(docs)
        print 'IB BEFORECOREF', coreferencehelper.get_coref_stats(docs, stats=True, only_stats=True)
        if not SKIP_PYPY:
            coreferencehelper.do_improve_coref(docs, iteration, use_ground_truth_instead_of_predictions=True)
        print 'IB AFTERCOREF', coreferencehelper.get_coref_stats(docs, stats=True, only_stats=True)
        for task in [classificationhelper.TASK_CHARACTER, classificationhelper.TASK_TYPE,
                     classificationhelper.TASK_ROLE]:
            _ = generate_tsv_file(docs, classificationhelper.TASK_MYCOREF, iteration)
            fname = generate_tsv_file(docs, task, iteration)
            preds = classificationhelper.get_labels(fname, docs)
            apply_predictions(docs, task, preds)
            print task, 'BEFOREVOTING', get_confusion_prfa(docs, task)
            classificationhelper.do_voting(docs, task, use_coref_annotations=False)
            print task, 'AFTERVOTING', get_confusion_prfa(docs, task)


def load_documents():
    docs = []
    for sty_file in settings.STY_FILES:
        doc = stanfordhelper.create_document_using_stanford_from_filtered_sty_file(settings.STY_FILE_PATH + sty_file)
        docs.append(doc)
    return docs


def load_documents_placeholders():
    fname = classificationhelper.get_filename(classificationhelper.TASK_COREF, 0)
    if os.path.isfile(fname):
        story_id_ = -1
        docs = []
        for line in open(fname).readlines():
            story_id, mention_id, coref_a, coref_p = [int(i) for i in line.split('\t')]
            if story_id != story_id_:
                story_id_ = story_id
                docs.append(voz.Document('', [voz.Sentence(-1, -1, -1, [])], {}, story_id))
            mention = entitymanager.Mention(mention_id, [], is_independent=True)
            mention.annotations.coref = coref_a
            mention.predictions.coref = coref_p
            docs[-1].sentences[0].mentions.append(mention)
        return docs
    else:
        return load_documents()


def generate_tsv_file(docs, what_file='', iteration=0):
    stats_skipped_not_independent = 0
    fname = classificationhelper.get_filename(what_file, iteration)
    if DO_FORCE_FEATURES_REWRITE or not os.path.isfile(fname):
        if not docs:
            docs = load_documents()
        f = open(fname, 'w')
        for doc in docs:
            fm = featuremanager.FeatureContainer(doc).init_features()
            for mention in doc.get_all_mentions(filter_only_independent=True):
                if what_file == classificationhelper.TASK_ROLE and not mention.annotations.is_character():
                    continue
                if what_file == classificationhelper.TASK_COREF or what_file == classificationhelper.TASK_MYCOREF:
                    data = [doc.id, mention.id, mention.annotations.coref or -1, mention.predictions.coref or -1]
                else:
                    data = [doc.id] + fm.get_features(mention)
                if what_file == classificationhelper.TASK_CHARACTER:
                    data += [mention.annotations.is_character()]
                elif what_file == classificationhelper.TASK_TYPE:
                    data += [mention.annotations.type or 'NA']
                elif what_file == classificationhelper.TASK_ROLE:
                    data += [mention.annotations.role or 'NA']
                line = '\t'.join([str(i) for i in data])
                f.write(line + '\n')
        f.close()
        # print stats_skipped_not_independent
    return fname


def apply_predictions(docs, what_file, predictions, apply_ground_truth=True):
    mentions = []
    for doc in docs:
        for mention in doc.get_all_mentions(filter_only_independent=True):
            if what_file == classificationhelper.TASK_ROLE and not mention.annotations.is_character():
                continue
            mentions.append(mention)
    if not len(mentions) == len(predictions):
        logger.error("Length of predictions doesn't match mentions")
    for mention, prediction in zip(mentions, predictions):
        if what_file == classificationhelper.TASK_TYPE:
            mention.predictions.type = prediction[-1]
            if apply_ground_truth:
                mention.annotations.type = prediction[-2]
        elif what_file == classificationhelper.TASK_ROLE:
            mention.predictions.role = prediction[-1]
            if apply_ground_truth:
                mention.annotations.role = prediction[-2]
        elif what_file == classificationhelper.TASK_CHARACTER:
            mention.predictions.character = util.bool_str(prediction[-1])
            if apply_ground_truth:
                mention.annotations.character = util.bool_str(prediction[-2])


def get_confusion_prfa(docs, what_file, verbose=False):
    if what_file == classificationhelper.TASK_CHARACTER:
        labels = [True, False]
        property = 'character'
    elif what_file == classificationhelper.TASK_TYPE:
        labels = entitymanager.taxonomy_dict[entitymanager.TaxonomyContainer.TAXONOMY_ENTITY_TYPES].labels
        property = 'type'
    elif what_file == classificationhelper.TASK_ROLE:
        labels = entitymanager.taxonomy_dict[entitymanager.TaxonomyContainer.TAXONOMY_CHARACTER_6ROLES].labels
        property = 'role'
    conf_matrix = {}
    pri = {}
    total = 0
    correct = 0
    prf = []
    for i in labels:
        pri[('p', i)] = 0
        pri[('r', i)] = 0
        pri[('i', i)] = 0
        for j in labels:
            conf_matrix[(i, j)] = 0
    for doc in docs:
        for mention in doc.get_all_mentions(filter_only_independent=True):
            if what_file == classificationhelper.TASK_ROLE and not mention.annotations.is_character(): continue
            total += 1
            l_pred = getattr(mention.predictions, property)
            if l_pred is None:
                l_pred = 'NA'
            l_anno = getattr(mention.annotations, property)
            conf_matrix[(l_pred, l_anno)] += 1
            pri[('p', l_pred)] += 1
            pri[('r', l_anno)] += 1
            if l_pred == l_anno:
                pri[('i', l_pred)] += 1
                correct += 1
    if verbose:
        print '\t',
        for i in sorted(labels):
            print i, '\t',
        print 'n\tp\tr\tf'
    for i in sorted(labels):
        p = (1.0 * pri[('i', i)] / pri[('p', i)]) if pri[('p', i)] else 0.0
        r = (1.0 * pri[('i', i)] / pri[('r', i)]) if pri[('r', i)] else 0.0
        f = (2 * p * r / (p + r)) if (p + r) else 0.0
        prf.append((p * pri[('r', i)] / total, r * pri[('r', i)] / total, f * pri[('r', i)] / total))
        if verbose:
            print i, '\t',
            for j in sorted(labels):
                print conf_matrix[(i, j)], '\t',
            print pri[('r', i)], '\t', p, '\t', r, '\t', f
    p, r, f = 0.0, 0.0, 0.0
    for i in prf:
        p += i[0]
        r += i[1]
        f += i[2]
    a = 1.0 * correct / total
    if verbose:
        print total, '\t', p, '\t', r, '\t', f, '\t', a
    return total, p, r, f, a


def get_stats_docs_verbs():
    docs = []
    for sty_file in settings.STY_FILES:
        doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH + sty_file)
        quotedspeechhelper.annotate_sentences(doc, settings.STORY_ALL_SENTENCES, format='tsv',
                                              single_sentences_file_story_id=doc.id)
        docs.append(doc)
    print sum([len(i.get_all_verbs()) for i in docs])
    print sum([sum([len([k for k in j._objects if k and k.is_independent]) + len(
        [k for k in j._subjects if k and k.is_independent]) for j in i.get_all_verbs()]) for i in docs])


if __name__ == '__main__':
    main()
