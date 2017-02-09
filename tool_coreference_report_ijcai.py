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
import util
import numpy as np
import collections
import itertools
logger = logging.getLogger(__name__)

def main():
    logging.root.setLevel(logging.ERROR)
    documents = make_coreferences()

def aggregate(values,weights,p0):
    pf = [1.0]*len(p0)
    pf_ = [1.0]*len(p0)
    for i in xrange(len(pf)):
        for k in xrange(len(values)):
            pf[i] *= (values[k][i]/p0[i])**weights[k]*p0[i]
            pf_[i] *= ((1.0-values[k][i])/(1.0-p0[i]))**weights[k]*(1.0-p0[i])
        pf[i] = pf[i]/(pf[i]+pf_[i])
    #print pf
    return pf


def merge_matrices(mentions,tables,table_gt=None):
    data = mentions
    p0 = [0.5,0.5]
    merged = np.zeros((len(data),len(data)))

    cell_weights = []
    for table,individual in tables:
        assert isinstance(table,np.ndarray)
        assert table.shape[0]==len(data)
        assert table.shape[1]==len(data)
        f1 = individual[2]/individual[7]
        f0 = individual[5]/individual[7]
        f_eval = individual[15]/individual[7]
        #cell_weights.append(util.average([f1,f0]))
        #cell_weights.append(f_eval)
        cell_weights.append(f1)

    #cell_weights = [0.6,0.2,0.1,0.1]
    #cell_weights = [0.9,0.1,0.1,0.1]

    for x in range(len(data)):
        for y in range(len(data)):
            cell_values = []
            for table,individual in tables:
                f1 = individual[2]/individual[7]
                f0 = individual[5]/individual[7]
                if table[x,y]==1:
                    #cell_values.append([1-f1,f1])
                    cell_values.append([0.1,0.9])
                else:
                    #cell_values.append([f1,1-f1])
                    #cell_values.append([f0,1-f0])
                    cell_values.append([0.9,0.1])

            pf = aggregate(cell_values,cell_weights,p0)

            merged[x,y]=1 if pf[1]>=pf[0] else 0

    # get the GT table to compare against

    if table_gt is None:
        table_gt = np.zeros((len(data),len(data)))
    groups_gt = collections.defaultdict(set)
    mention_ids = []
    for mention in data:
        mention_id = mention.id
        coref_group_gt = tuple(mention.get_tag(voz.entitymanager.TaggableContainer.TAG_CHARACTER_SYMBOL))
        mention_ids.append(mention_id)
        groups_gt[coref_group_gt].add(mention_id)
    if table_gt is None:
        for group_id,group in groups_gt.items():
            for pair in itertools.combinations(group,2):
                if pair[0]==pair[1]: continue
                table_gt[mention_ids.index(pair[0]),mention_ids.index(pair[1])]=1
                table_gt[mention_ids.index(pair[1]),mention_ids.index(pair[0])]=1

    table = merged
    p1 = np.sum(table)
    r1 = np.sum(table_gt)
    count1 = r1
    intersect1 = np.sum(np.multiply(table,table_gt))
    p1 = 1.0 * intersect1/p1
    r1 = 1.0 * intersect1/r1
    f1 = 2.0 * p1 * r1 / (p1+r1)
    p0 = np.sum(np.logical_not(table))
    r0 = np.sum(np.logical_not(table_gt))
    count0 = r0
    intersect0 = np.sum(np.multiply(np.logical_not(table),np.logical_not(table_gt)))
    p0 = 1.0 * intersect0/p0
    r0 = 1.0 * intersect0/r0
    f0 = 2.0 * p0 * r0 / (p0+r0)
    print(util.string_as_print("BEFORE CONSISTENT","PRF1",p1,r1,f1,"PRF0",p0,r0,f0))
    # MAKE CONSISTENT
    new_coreference_groups_map = {}
    new_coreference_groups_ids = 0
    for x in range(len(data)):
        for y in range(len(data)):
            if table[x,y]==1:
                if mention_ids[x] not in new_coreference_groups_map and mention_ids[y] not in new_coreference_groups_map:
                    # new coref group
                    new_coreference_groups_map[mention_ids[x]] = new_coreference_groups_ids
                    new_coreference_groups_map[mention_ids[y]] = new_coreference_groups_ids
                    new_coreference_groups_ids+=1
                elif mention_ids[x] in new_coreference_groups_map and mention_ids[y] in new_coreference_groups_map and new_coreference_groups_map[mention_ids[x]]==new_coreference_groups_map[mention_ids[y]]:
                    # existing coref
                    pass
                elif mention_ids[x] in new_coreference_groups_map and mention_ids[y] not in new_coreference_groups_map:
                    new_coreference_groups_map[mention_ids[y]]=new_coreference_groups_map[mention_ids[x]]
                elif mention_ids[y] in new_coreference_groups_map and mention_ids[x] not in new_coreference_groups_map:
                    new_coreference_groups_map[mention_ids[x]]=new_coreference_groups_map[mention_ids[y]]
                elif mention_ids[x] in new_coreference_groups_map and mention_ids[y] in new_coreference_groups_map and not new_coreference_groups_map[mention_ids[x]]==new_coreference_groups_map[mention_ids[y]]:
                    new_value = new_coreference_groups_map[mention_ids[x]]
                    old_value = new_coreference_groups_map[mention_ids[y]]
                    for i in new_coreference_groups_map.keys():
                        if new_coreference_groups_map[i]==old_value:
                            new_coreference_groups_map[i] = new_value
                else:
                    logger.error("THIS SHOULD NEVER HAPPEN")
    for mention in mentions:
        if mention.id in new_coreference_groups_map:
            mention.add_tag('AGGREGATED',new_coreference_groups_map[mention.id])
        else:
            mention.add_tag('AGGREGATED',new_coreference_groups_ids)
            new_coreference_groups_ids+=1

def make_coreferences():
    documents = []

    file_path = settings.STY_FILE_PATH

    vars_names = ['p1','r1','f1','p0','r0','f0','length','length**2','count1','count0','mentions_characters','char_uniq','coref_groups','c/gr','gr/c','eval']
    num_vars = len(vars_names)

    matrices_to_compute = ['OLD_STANFORD_COREF','OLD_NAME_COREF']+['OLD_RESTRICTION','OLD_TYPE']+['OLD_IDX']
    matrices_to_merge = ['OLD_STANFORD_COREF','OLD_NAME_COREF']+['OLD_RESTRICTION','OLD_TYPE']
    # OLD_ROLE_PRED1
    # OLD_ROLE_GT
    #matrices_to_print = matrices_to_compute+["AGGREGATED"]
    matrices_to_print = ["AGGREGATED"]
    cumulative = dict([(i,[0.0]*num_vars) for i in matrices_to_compute+["AGGREGATED"]])

    for sty_file in settings.STY_FILES:
        logger.info("Processing %s" % sty_file)
        quoted_speech_file = sty_file.split()[0]+"/sentences.csv"
        doc = styhelper.create_document_from_sty_file(file_path+sty_file)
        assert isinstance(doc,voz.Document)
        quotedspeechhelper.annotate_sentences(doc, file_path + quoted_speech_file)
        quotedspeechhelper.clean_quoted_speech_from_document(doc)
        doc.coreference_aux[voz.entitymanager.TaggableContainer.TAG_CHARACTER_SYMBOL] = doc.coreference
        mentions = doc.get_all_mentions()
        mentions = [i for i in mentions if i.is_independent]
        mentions = [i for i in mentions if 'CH' in i.get_taxonomy(voz.entitymanager.TaxonomyContainer.TAXONOMY_NONCHARACTER)]
        '''# create stanford, name, roles coref
        for coref_key in matrices_to_eval:
            coref_ids =  sorted([i for i in set(util.flatten([mention.get_tag(coref_key) for mention in mentions]))])
            print "mentions, coref_ids",len(mentions),len(coref_ids),coref_ids
            doc.coreference_aux[coref_key] = voz.entitymanager.Coreference(doc)
            for coref_id in coref_ids:
                mentions_coref = [i for i in mentions if coref_id in i.get_tag(coref_key)]
                doc.coreference_aux[coref_key].create_coref_group_and_entity_from_mentions(doc.get_symbol_id(coref_id,'COREF_SYMBOL'),coref_id,mentions_coref)

        # eval coref
        print voz.Document.format_stats(doc.get_stats())'''

        # eval the individual matrices and compute their table for aggregation later
        tables_to_merge = []
        table_gt_temp = None
        for coref_key in matrices_to_compute:
            print coref_key
            table,individual = voz.entitymanager.Coreference.eval_prf(coref_key,mentions)
            if table_gt_temp is None:
                table_gt_temp = table
            if coref_key in matrices_to_merge:
                tables_to_merge.append((table,individual))
            for i in xrange(num_vars):
                cumulative[coref_key][i]+=individual[i]
        # aggregate the tables and evaluate aggregation
        coref_key = "AGGREGATED"
        merge_matrices(mentions,tables_to_merge,table_gt_temp)
        table,individual = voz.entitymanager.Coreference.eval_prf(coref_key,mentions)
        for i in xrange(num_vars):
            cumulative[coref_key][i]+=individual[i]

        #break # sty_file

    for j in matrices_to_print:
        for i in xrange(num_vars):
            cumulative[j][i]=cumulative[j][i]/cumulative[j][7]
    print 'CUMMULATIVE OVER STORIES'
    for j in matrices_to_print:
        print j
        for i in xrange(num_vars):
            print "%s\t%f" % (vars_names[i],cumulative[j][i])
        for i in xrange(num_vars-3,num_vars):
            print "%s\t%f" % (vars_names[i],cumulative[j][i]/15.0)
        avg = 1.0 * (cumulative[j][2]*cumulative[j][8]+cumulative[j][3]*cumulative[j][9])/(cumulative[j][8]+cumulative[j][9])
        print "error\t%f\t%f" % (avg,1-avg)





if __name__=='__main__':
    main()

'''
                    mention.add_tag('OLD_SYMBOL',row[DATA_SYMBOL])
                    mention.add_tag('OLD_TYPE',row[DATA_TYPE])
                    mention.add_tag('OLD_IDX',labels[1])
                    mention.add_tag('OLD_STANFORD_COREF',labels[2])
                    mention.add_tag('OLD_NAME_COREF',labels[3])
                    mention.add_tag('OLD_ROLE_GT',labels[13])
                    mention.add_tag('OLD_ROLE_PRED1',labels[16])
                    mention.add_tag('OLD_ROLE_PRED2',labels[17])
                    mention.add_tag('OLD_ROLE_PRED3',labels[18])'''








