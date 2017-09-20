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
import styhelper
import util
from riuhelper import *

export_path = '/Users/josepvalls/voz2/sam-clisp/'

def main():
    script = ''
    stories_in_use = settings.STY_FILES
    for suffix_g in ['sty']: #sty voz
        for suffix_v in ['levinverb']: # noverb, 'basicverb' levinverb
            for suffix_f in ['nofunc']:#,'functs']:
                for suffix_r in ['roleexp']: # 'norole','roleent','roleexp'
                    for suffix_s in ['prep']:

                        suffix = suffix_v + '_' + suffix_f + '_' + suffix_r + '_' + suffix_s + '_' + suffix_g
                        # write the scripts
                        for k in ['eval','full']:
                            for i,s in enumerate(get_riu_runner(len(stories_in_use),suffix,k)):
                                fname = 'voz-'+k+'-'+str(i+1)+'-'+suffix+'.lisp'
                                with open(export_path+fname,'w') as f:
                                    f.write(s)
                                script += 'clisp ' + fname + ' > ' + fname + '.txt &\n'
                                    #break

                        #continue

                        # write the stories
                        for sty_file in stories_in_use:
                            if suffix_g == 'sty':
                                doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH + sty_file)
                                styhelper.fix_sty_annotations(doc)
                            else:
                                doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH + sty_file)
                                #doc = stanfordhelper.create_document_using_stanford_from_filtered_sty_file(settings.STY_FILE_PATH + sty_file)
                                #doc = stanfordhelper.create_document_from_raw_text(text,{'cache':False})
                                doc.compute_predictions()
                            out = doc_to_sam(doc,suffix_g,suffix_v,suffix_f,suffix_r,suffix_s,limit=None)
                            with open(export_path+('voz/story%d' %doc.id)+'-full-%s.lisp'%suffix,'w') as f:
                                f.write(out)
                            #continue
                            out = doc_to_sam(doc,suffix_g,suffix_v,suffix_f,suffix_r,suffix_s,limit=2)
                            with open(export_path+('voz/story%d' %doc.id)+'-complete-%s.lisp'%suffix,'w') as f:
                                f.write(out)
                            out = doc_to_sam(doc,suffix_g,suffix_v,suffix_f,suffix_r,suffix_s,limit=1)
                            with open(export_path+('voz/story%d' %doc.id)+'-partial-%s.lisp'%suffix,'w') as f:
                                f.write(out)

                        #break
    with open(export_path+'main.sh','w') as f:
        f.write(script)

def sent_stats(doc,sentences):
    assert isinstance(doc,voz.Document)
    s = []
    mentions = doc.get_all('mentions',sentences)
    mentions = [i for i in mentions if i.is_independent]
    s.append(len(mentions))
    s.append(len([i for i in mentions if i.annotations.is_character()]))
    s.append(len(set([i.get_most_likely_symbol() for i in mentions if i.annotations.is_character()])))
    s.append(len(mentions))
    verbs = doc.get_all('verbs', sentences)
    s.append(len(verbs))
    vargs = util.flatten([i.get_subjects('annotations')+i.get_objects('annotations') for i in verbs])
    vargs = [i for i in vargs if i in mentions]
    s.append(len(vargs))
    return s


def main_get_stats():
    for sty_file in settings.STY_FILES:
        stats = []
        doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH + sty_file)
        styhelper.fix_sty_annotations(doc)
        stats += sent_stats(doc,doc.sentences)+[ len(doc.narrative.functions(filter_non_actual=False))]
        phases = segment_doc(doc)
        # (sentences, functions)
        try:
            stats += sent_stats(doc,phases[0][0])+[ len(phases[0][1])]
        except:
            pass
        try:
            stats += sent_stats(doc,phases[1][0])+[ len(phases[1][1])]
        except:
            pass
        print '\t'.join([str(i) for i in stats])


if __name__ == '__main__':
    main_get_stats()
    #main_get_levin_language()
    #sys.exit()
    #main()
