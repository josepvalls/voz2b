import tool_irb_synthetic_forms_httpserver

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
fields = ['status','total_time','datapoints']+fields_consent+['reading_check']
statuses = {'DONE':0,'PEND':0}
for uid in data.keys():
    e = {}
    if not 'form1' in data[uid]: continue
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
        e['total_time'] = "%.2f" % e['total_time']
    else:
        e['status'] = 'PEND'
        e['total_time'] = 'PEND'
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
print "Totals"
print statuses

