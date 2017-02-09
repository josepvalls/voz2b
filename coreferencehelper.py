import numpy as np
import jsonpickle
import jsonpickle.ext.numpy as jsonpickle_numpy
jsonpickle_numpy.register_handlers()
import settings
import os
import featuremanager
import collections
import util
import logging

logger = logging.getLogger(__name__)

DO_FORCE_COREF_RELOAD = False

def do_improve_coref(docs, iteration, use_ground_truth_instead_of_predictions=False):
    m1 = get_coref_m1(docs) # stanford
    m2 = get_coref_m2(docs) # names
    m3 = get_coref_m3(docs)  # features
    m4 = get_coref_m4(docs)  # restrictions
    matrices = [m1,m2,m3,m4]
    weights = [1.0, 1.1, 0.9, 10.0]
    if iteration>0:
        m5,m6 = get_coref_m56(docs, use_ground_truth_instead_of_predictions)
        matrices += [m5, m6]
        weights += [0.9, 10.0]
    m = do_merge_matrices(docs, matrices, weights)
    make_consistent_and_apply_coref(docs, m)

def make_consistent_and_apply_coref(docs, m, container='predictions'):
    for doc in docs:
        mentions = doc.get_all_mentions(filter_only_independent=True)
        clear_coreference(mentions, container)
        mentions_done = set()
        for i in xrange(len(mentions)):
            if i in mentions_done: continue
            group = _get_mentions_in_group(m[doc.id],i,len(mentions),threshold=1.0)
            mentions_done.update(group)
            for j in group:
                coref = getattr(mentions[j], container).coref
                if not coref==None:
                    logger.warning("mention %d already has coref group %d when setting it to %d" % (mentions[j].id, coref, i))
                else:
                    getattr(mentions[j], container).coref = mentions[i].id
        for mention in mentions:
            coref = getattr(mention, container).coref
            getattr(mention, container).coref = coref or mention.id

def _get_mentions_in_group(m,start,end, threshold):
    open = set([start])
    closed = set([start])
    while open:
        i = open.pop()
        #for i in xrange(start,end):
        for j in xrange(start,end):
            if m[i,j]>threshold:
                if j not in closed:
                    closed.add(j)
                    open.add(j)
    return closed


def clear_coreference(mentions, container='predictions'):
    for mention in mentions:
        getattr(mention,container).coref=None




def get_coref_m1(docs):
    m1 = {}
    fname = settings.COREFERENCE_JSON + 'm1-' + str(docs[0].id) + '.npy'
    if not DO_FORCE_COREF_RELOAD and os.path.isfile(fname):
        for doc in docs:
            fname = settings.COREFERENCE_JSON + 'm1-'+str(doc.id)+'.npy'
            if not DO_FORCE_COREF_RELOAD and os.path.isfile(fname):
                m1[doc.id] = np.load(fname)
    else:
        fin = settings.FEATURE_TSV_FILES + 'all_coreferenced-entities-aaai-%s-%d.tsv' % ('COREF', 0)
        story_id_ = -1
        m1_ = {}
        for line in open(fin).readlines():
            story_id,mention_id,coref_a,coref_p = [int(i) for i in line.split('\t')]
            if story_id != story_id_:
                m1_[story_id] = []
                story_id_ = story_id
            m1_[story_id].append((mention_id,coref_p))
        for story,mentions in m1_.items():
            m1[story] = np.ones((len(mentions),len(mentions)))*-1.0
            for i,i_m in enumerate(mentions):
                for j,j_m in enumerate(mentions):
                    if i_m[1]==j_m[1]:
                        m1[story][i,j]=1.0
            fname = settings.COREFERENCE_JSON + 'm1-' + str(story) + '.npy'
            np.save(fname,m1[story])
    return  m1

def get_coref_m2(docs):
    m2 = {}
    for doc in docs:
        fname = settings.COREFERENCE_JSON + 'm2-' + str(doc.id) + '.npy'
        if not DO_FORCE_COREF_RELOAD and os.path.isfile(fname):
            m2[doc.id] = np.load(fname)
        else:
            mentions = doc.get_all_mentions(filter_only_independent=True)
            m2[doc.id] = np.zeros((len(mentions),len(mentions)))
            for i,i_m in enumerate(mentions):
                for j,j_m in enumerate(mentions):
                    intersect = set([k.lemma for k in i_m.tokens]) & set([k.lemma for k in j_m.tokens])
                    if intersect:
                        m2[doc.id][i,j] = 1.0
            np.save(fname,m2[doc.id])
    return m2

def get_coref_m3(docs):
    m3 = {}
    for doc in docs:
        fname = settings.COREFERENCE_JSON + 'm3-' + str(doc.id) + '.npy'
        if not DO_FORCE_COREF_RELOAD and os.path.isfile(fname):
            m3[doc.id] = np.load(fname)
        else:
            dist_cache = {}
            fm = featuremanager.FeatureContainer(doc).init_features()
            mentions = doc.get_all_mentions(filter_only_independent=True)
            m3[doc.id] = np.zeros((len(mentions),len(mentions)))
            for i,i_m in enumerate(mentions):
                for j,j_m in enumerate(mentions):
                    if i==j:
                        dist = 0.0
                    else:
                        cache_key = list(sorted([hash(tuple(fm.get_features(i_m))),hash(tuple(fm.get_features(j_m)))]))
                        cache_key = cache_key[0]+cache_key[1]
                        if cache_key in dist_cache:
                            dist = dist_cache[cache_key]
                        else:
                            dist = _m3_distance(fm.get_features(i_m),fm.get_features(j_m))
                            dist_cache[cache_key] = dist
                    m3[doc.id][i,j] = dist
            np.save(fname, m3[doc.id])
    return m3

def _m3_distance(a_,b_):
    t_inter = 0.0
    t_union = 0.0
    for a,b in zip(a_,b_):
        t_inter += min(a,b)
        t_union += max(a,b)
    if t_union:
        return t_inter/t_union
    else:
        return 0.0

def get_coref_m4(docs):
    labels = {
        'P':['hasPluralFirstPerson','hasPluralSecondPerson','hasPluralThirdPerson','hasPluralThirdPersonGeneric'],
        'M':['hasSingularThirdPersonMale','ProperNamesMale','CommonNamesMale'],
        'F':['hasSingularThirdPersonFemale','ProperNamesFemale','CommonNamesFemale']
    }
    m4 = {}
    for doc in docs:
        fname = settings.COREFERENCE_JSON + 'm4-'+str(doc.id)+'.npy'
        if not DO_FORCE_COREF_RELOAD and os.path.isfile(fname):
            m4[doc.id] = np.load(fname)
        else:
            fm = featuremanager.FeatureContainer(doc).init_features()
            mentions = doc.get_all_mentions(filter_only_independent=True)
            m4[doc.id] = np.zeros((len(mentions),len(mentions)))
            mention_labels = []
            for i in mentions:
                f = fm.get_features(i)
                i_l = 'N'
                i_l_v = 0.0
                for l,f_list in labels.items():
                    v = 0.0
                    for f_ in f_list:
                        v+=f[featuremanager.MentionFeatures.get_feature_names().index(f_)]
                    if v>i_l_v:
                        i_l_v=v
                        i_l = l
                mention_labels.append(i_l)
            for i,i_m in enumerate(mentions):
                for j,j_m in enumerate(mentions):
                    if not mention_labels[i]== mention_labels[j]:
                        m4[doc.id][i,j] = -1.0
            np.save(fname,m4[doc.id])
    return m4

def get_coref_m56(docs, use_ground_truth_instead_of_predictions):
    m5 = {}
    m6 = {}
    for doc in docs:
        mentions = doc.get_all_mentions(filter_only_independent=True)
        m5[doc.id] = np.zeros((len(mentions), len(mentions)))
        m6[doc.id] = np.zeros((len(mentions), len(mentions)))
        for i, i_m in enumerate(mentions):
            for j, j_m in enumerate(mentions):
                if use_ground_truth_instead_of_predictions:
                    if i_m.annotations.role==j_m.annotations.role:
                        m5[doc.id][i, j] = 1.0
                    if i_m.annotations.character != j_m.annotations.character:
                        m6[doc.id][i, j] = -1.0
                else:
                    if i_m.predictions.role==j_m.predictions.role:
                        m5[doc.id][i, j] = 1.0
                    if i_m.predictions.character != j_m.predictions.character:
                        m6[doc.id][i, j] = -1.0
    return m5,m6

def debug_cpm(m):
    for k,v in m.items():
        print k,np.sum(v)


def main():
    get_coref_m1(None)

if __name__=='__main__':
    main()



def do_merge_matrices(docs, matrices, weights):
    agg_m = {}
    for doc in docs:
        mentions = doc.get_all_mentions(filter_only_independent=True)
        agg_m[doc.id] = np.identity(len(mentions))
        #agg_m[doc.id] = np.ones((len(mentions),len(mentions)))
        #continue
        for m,w in zip(matrices,weights):
            b = m[doc.id]
            agg_m[doc.id] = np.add(agg_m[doc.id],b*w)
    return agg_m

def get_coref_stats(docs, filter_character_field='annotations', stats = True, verbose=False, only_stats=False):
    anno_to_mention = collections.defaultdict(list)
    pred_to_mention = collections.defaultdict(list)
    for doc in docs:
        for mention in doc.get_all_mentions(filter_only_independent=True):
            if filter_character_field is not None and not getattr(mention,filter_character_field).is_character(): continue
            anno_to_mention[(doc.id,mention.annotations.coref)].append(mention)
            pred_to_mention[(doc.id,mention.predictions.coref)].append(mention)
    if stats:
        c_g = []
        g_c = []
        for g in pred_to_mention.values():
            c_g_ = len(set([i.annotations.coref for i in g]))
            c_g.append(c_g_)
        for g in anno_to_mention.values():
            g_c_ = len(set([i.predictions.coref for i in g]))
            g_c.append(g_c_)
        c_g = util.average(c_g)
        g_c = util.average(g_c)
    else:
        c_g = 0.0
        g_c = 0.0
    if verbose:
        print 'COREF', len(anno_to_mention),len(pred_to_mention),c_g,g_c
    if only_stats:
        return len(anno_to_mention),len(pred_to_mention),c_g,g_c
    else:
        return dict(pred_to_mention),dict(anno_to_mention),len(anno_to_mention),len(pred_to_mention),c_g,g_c
