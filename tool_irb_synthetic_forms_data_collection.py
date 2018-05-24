import tool_irb_synthetic_forms_httpserver
import util
def flatten_form(d):
    for i in d:
        if isinstance(d[i],list) and d[i]:
            d[i] = d[i][0]
    return d


import os
import collections
import json
import datetime

data = collections.defaultdict(list)
ret = []
for f in os.listdir(tool_irb_synthetic_forms_httpserver.DATA_PATH):
    if '_' not in f or '.' in f: continue
    uid,evt = f.split('_',1)
    data[uid].append(evt)
fields_consent = ['name','timestamp','age','edu','dustudent','native','competent']
fields = ['status','total_time','datapoints']+fields_consent+['reading_check', 'uid']
statuses = {'DONE':0,'PEND':0,'BAD':0,'MISS':0}
issue_scale = {'y':1,'n':-1,'a':0}
issue_labels = ['Reasonable plot', 'Character match', 'Character coref', 'Grammar']
continuation_labels = ['Original','Voz+Riu','Anno.+Riu','LSTM']
continuation_labels = ['Original','Anno.+Riu','Voz+Riu','LSTM']
# good voz syn nn
NUM_STORIES = 9
NUM_CONTINUATIONS = len(continuation_labels)
NUM_STORIES_SHOWN = 5
NUM_ISSUES = len(issue_labels)
results_by_story = dict([('s%d'%i,dict([('c%d'%j,[]) for j in range(NUM_CONTINUATIONS)])) for i in range(NUM_STORIES)])
results_by_presentation = [[[] for j in range(NUM_CONTINUATIONS)] for i in range(NUM_STORIES)]
results_by_continuation = dict([('c%d'%j,[]) for j in range(NUM_CONTINUATIONS)])
results_missing = []
import itertools
results_by_continuation_issues = dict([('c%d-%d'%j,[]) for j in itertools.product(range(NUM_CONTINUATIONS),range(1,NUM_ISSUES+1))])
results_by_continuation_comments = dict([('c%d'%j,[]) for j in range(NUM_CONTINUATIONS)])



for uid in data.keys():
    e = {}
    if not 'form1' in data[uid]: continue
    e['uid'] = uid
    c = json.load(open(tool_irb_synthetic_forms_httpserver.DATA_PATH+'/'+uid+'_form1'))
    c = flatten_form(c)
    for i in fields_consent:
        e[i]=c.get(i,'?')
    if not e['name']: e['name'] = "NO NAME"
    e['reading_check'] = 0
    if c.get('c1','?') == 'false': e['reading_check'] += 1
    if c.get('c2','?') == 'true': e['reading_check'] += 1
    if c.get('c3','?') == 'false': e['reading_check'] += 1
    if c.get('c4','?') == 'false': e['reading_check'] += 1
    if c.get('c5','?') == 'true': e['reading_check'] += 1
    if c.get('c6','?') == 'true': e['reading_check'] += 1
    if 'form2' in data[uid]:
        e['status'] = 'DONE'
        c = json.load(open(tool_irb_synthetic_forms_httpserver.DATA_PATH + '/' + uid + '_form2'))
        c = flatten_form(c)

        t_s = datetime.datetime.strptime(e['timestamp'], tool_irb_synthetic_forms_httpserver.TIMESTAMP_FORMAT)
        t_e = datetime.datetime.strptime(c['timestamp'], tool_irb_synthetic_forms_httpserver.TIMESTAMP_FORMAT)

        e['total_time'] = 1.0 * (t_e-t_s).seconds / 60.0
        if e['reading_check']<5 or e['total_time']<5.0:
            e['status'] = 'BAD'
        e['total_time'] = "%.2f" % e['total_time']
    else:
        e['status'] = 'PEND'
        e['total_time'] = 'PEND'
        c = {}
        for trace in data[uid]:
            if trace in ['form1','form2']: continue
            c_ = json.load(open(tool_irb_synthetic_forms_httpserver.DATA_PATH + '/' + uid + '_'+trace))
            if len(c_)> len(c):
                c = c_
    #if c and e['status'] in ['DONE','PEND']:
    if c and e['status'] in ['DONE']:
        e['sources'] = c['sources'].split()
        source_idx = 0
        for i in range(NUM_STORIES_SHOWN):
            for j in range(NUM_CONTINUATIONS):
                rating_field = 'r%d-%d' % (i+1,j+1)
                rating = c.get(rating_field,None)
                source_id = e['sources'][source_idx]
                source_idx+=1
                if rating is None:
                    e['status'] = 'MISS'
                    results_missing.append((uid,rating_field))
                    continue
                rating = float(rating)
                results_by_presentation[i][j].append(rating)
                results_by_story[source_id[0:2]][source_id[2:4]].append(rating)
                results_by_continuation[source_id[2:4]].append(rating)
                for k in range(NUM_ISSUES):
                    issue = c.get('q%d-%d-%d' % (i + 1, j + 1, k+1), None)
                    if issue and issue in issue_scale:
                        results_by_continuation_issues[source_id[2:4]+'-%d'%(k+1)].append(issue_scale[issue] * (1 if k in [0,1] else -1))
                comment = c.get('o%d-%d' % (i+1,j+1),None)
                if comment and comment.strip():
                    results_by_continuation_comments[source_id[2:4]].append((source_id[0:2],uid,comment))
    statuses[e['status']]+=1
    e['datapoints'] = len(data[uid])
    ret.append(e)
for j in fields:
        print j,'\t',
print
for i in ret:
    for j in fields:
        print i.get(j,'?'),'\t',
    print


for c,c_lst in sorted(results_by_continuation.items()):
    print c,util.average(c_lst),len(c_lst), c_lst
print sum([len(i) for i in results_by_continuation.values()])

import scipy.stats as stats
print stats.ttest_ind(a= results_by_continuation['c0'], b= results_by_continuation['c1'], equal_var=False)
print stats.ttest_ind(a= results_by_continuation['c1'], b= results_by_continuation['c2'], equal_var=False)
print stats.ttest_ind(a= results_by_continuation['c2'], b= results_by_continuation['c3'], equal_var=False)
if True:
    from pandas import DataFrame, concat
    from matplotlib import pyplot
    import pickle
    import seaborn as sns
    if True:
        results = [DataFrame({lbl:d}) for lbl,d in results_by_continuation.items()]
        results = concat([
            DataFrame({continuation_labels[0]: results_by_continuation['c0']}),
            DataFrame({continuation_labels[2]: results_by_continuation['c2']}),
            DataFrame({continuation_labels[1]: results_by_continuation['c1']}),
            DataFrame({continuation_labels[3]: results_by_continuation['c3']})], axis=1)
        results.columns=continuation_labels
        print(results.describe())
        #results.boxplot()
        #pyplot.show()        
        ax = sns.violinplot(data=results, inner="box")
        pyplot.yticks([1, 2, 3, 4, 5, 6])
        pyplot.ylabel("Score")
        if False:
            pyplot.show()

    if False:
        d.columns=['Plot','Char.','Coref.','Gramm.']
        results = concat([DataFrame({lbl: d}) for lbl, d in results_by_continuation_issues.items()])
        pickle.dump(results,open('tool_irb_results.pickle','wb'))
    else:
        results = pickle.load(open('tool_irb_results.pickle','rb'))
    if True:
        print(results.describe())
        #results.boxplot()
        #pyplot.show()        
        #ax = sns.violinplot(data=results, inner="box")
        #pyplot.show()
        fig, (ax1, ax2, ax3, ax4) = pyplot.subplots(ncols=4, sharey=True)
        d = results[['c0-1', 'c0-2', 'c0-3', 'c0-4']]
        d.columns=['Plot','Char.','Coref.','Gramm.']
        sns.violinplot(data=d, inner="box", ax=ax1)
        d=results[['c1-1', 'c1-2', 'c1-3', 'c1-4']]
        d.columns = ['Plot', 'Char.', 'Coref.', 'Gramm.']
        sns.violinplot(data=d, inner="box", ax=ax3)
        d=results[['c2-1', 'c2-2', 'c2-3', 'c2-4']]
        d.columns = ['Plot', 'Char.', 'Coref.', 'Gramm.']
        sns.violinplot(data=d, inner="box", ax=ax2)
        d=results[['c3-1', 'c3-2', 'c3-3', 'c3-4']]
        d.columns = ['Plot', 'Char.', 'Coref.', 'Gramm.']
        sns.violinplot(data=d, inner="box", ax=ax4)
        ax1.set_title(continuation_labels[0])
        ax2.set_title(continuation_labels[1])
        ax3.set_title(continuation_labels[2])
        ax4.set_title(continuation_labels[3])
        pyplot.yticks([-1, 0, 1])
        #ax1.ylabel("Score")
        ax1.set_ylabel("Score")
        if False:
            pyplot.show()

print "Totals"
print statuses
print "Missing"
print results_missing
print "Comments"
for k,v in sorted(results_by_continuation_comments.items()):
    for s,u,i in v:
        print k,'\t',u,'\t',s,'\t',i.replace('\n','; ').replace('\r','')
