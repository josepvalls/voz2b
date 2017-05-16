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

export_path = '/Users/josepvalls/voz2/sam-clisp/'

def _get_sam_phases_():
    return '''
         (phase1 (:s t1 t2 t3 t4))
         (phase2 (:s tb1))
    '''

def _get_sam_templates_():
    return '''
         (t1 (E0  (Julian "Julian") " hasn't come across a " (dolphin "Tiger Dolphin") ) " in twenty-three years,")
         (t2 "and " (Julian "he") " has never been " (E1  (Julian "this") " close to " (dolphin "one") ) ".")
         (t3 (Julian "Julian") " couldn?t " ( "tell") " if " (E6  (Julian "his") " sudden shortness of " (tired "breath") ) " was " (E2  (Julian "from") " " (excited "excitement") ) " or " (E4  (equipment "tank") " damage from the " (dolphin "dolphin") ) "?s nudging.")
         (t4 (E7  (Julian "He") " considers " (surface "resurfacing") ) " to check " (E8  (Julian "his") " " (equipment "tank") ) ".")
         (t5 (E5  (dolphin "Dolphin") " push " (equipment "tank") ) ".")
         (t6 (E9  (Julian "Julian") " needs " (air "air") ) ".")
         (tb1 "And " (EB1 (Julian "Julian") " lived happily ever after"))
    '''

def get_sam_common():
    return '''
       (common
         (:entities
           (human :type animate)
           (MA :type human)
           (FE :type human)
           (anthropomorphized :type animate)
           (AA :type anthropomorphized)
           (AO :type anthropomorphized)
           (othera :type animate)
           (GR :type othera)
           (MB :type anthropomorphized)
           (PA :type entity)
           (AN :type othera)
           (HA :type entity)
           (OB :type inanimate)
           (SC :type inanimate)
           (PO :type inanimate)
           (setting :type entity)
           (SS :type setting)
           (ST :type setting)
           (NC :type entity)
           (NA :type entity)
           (m-1 :type NA)
         )
         (:expressions
         )
       )
    '''

def get_sam_phases(phases,all_coref_mentions,do_verbs,do_funcs):
    ret = ''
    ret_ent = ''
    ret_exp = ''
    phase_i = 0
    sentence_i = 0
    sentence_i_bak = 0
    expression_extra_i = 0
    expression_i = 0
    discourse = ''
    templates_comments = ''
    templates_templates = ''

    mentions = []
    discourse = ''
    for sentences, functions in phases:
        ret_tmp = ''
        substitutions = {}
        substitutions_expressions = {}
        phase_i+=1
        discourse_sentence = ''
        sentence_i_bak = sentence_i
        for sentence in sentences:
            sentence_i+=1
            discourse_sentence += ' t%d' % sentence_i
            templates_comments += '         ;; %s\n' % sentence.get_text()
            #templates_templates += '         (t%d "%s"\n' % (sentence_i, sentence.get_text().replace('"', "'"))
        discourse += '         (phase%d (:s %s))\n' % (phase_i,discourse_sentence)

        for sentence in sentences:
            for verb in sentence.verbs:
                assert isinstance(verb, verbmanager.Verb)
                subjects = filter_mentions(verb.get_subjects(), all_coref_mentions)
                objects = filter_mentions(verb.get_objects(), all_coref_mentions)
                #mentions += subjects
                #mentions += objects
                if subjects or objects:
                    if subjects:
                        mentions.append(subjects[0])
                        subject = subjects[0].annotations.coref
                        substitutions[subjects[0]] = subject
                    else:
                        subject = -1
                    if objects:
                        mentions.append(objects[0])
                        object_ = objects[0].annotations.coref
                        substitutions[objects[0]] = object_
                    else:
                        object_ = -1
                    if do_verbs =='basicverb':
                        expression_i +=1
                        ret_tmp += '           ((verb-%s m%s m%s) :name E%d)\n' % (verb.get_text(), subject, object_, expression_i)
        entities = {}
        for i in mentions:
            entities[i.annotations.coref] = i

        ret += '       (phase%d\n' % phase_i
        ret += '         (:entities\n'
        for key in entities.keys():
            ret += '           (m%s :type %s)\n' % (key,entities[key].annotations.type)
        ret += '         )\n'
        ret += '         (:expressions\n'
        ret += ret_tmp
        if do_funcs == 'funcs':
            for key in entities.keys():
                role = entities[key].annotations.role
                if role and ',' not in role:
                    expression_extra_i +=1
                    ret += '           ((%s m%s) :name EXTRA%d)\n' % (role,key,expression_extra_i)

        ret += '         )\n'
        ret += '       )\n'
        # end of phase
        sentence_i = sentence_i_bak
        for sentence in sentences:
            sentence_i += 1
            #templates_templates += '         (t%d "%s"\n' % (sentence_i, sentence.get_text().replace('"', "'"))
            templates_templates += '         (t%d "%s"\n' % (sentence_i, substitute_text(sentence,substitutions,substitutions_expressions))


    return discourse,templates_comments,templates_templates,ret

def substitute_text(sentence,substitutions,expressions):
    ret = ''
    subs_tokens = dict(util.flatten([[(j,i) for j in i.tokens] for i in substitutions.keys()]))
    sent_tokens = list(sentence.tokens)
    while sent_tokens:
        token = sent_tokens.pop(0)
        if token in subs_tokens:
            mention = subs_tokens[token]
            for _ in mention.tokens[1:]:
                if sent_tokens: sent_tokens.pop(0)
            ret += "\" (%s \"%s\") \"" % (substitutions[mention],mention.get_text())
        else:
            ret += sentence._parent_document.text[token.offset:token.offset + token.len].replace('\n', '').replace('"',"'") + ' '
    return ret


def get_sam_template(story_id, phases, templates_comment, templates_text, structure_common, structure_phases):
    return '''
;; --------------------------------------------------------------------
;; This file has been automatically generated by Voz
;; Josep Valls-Vargas
;; Jichen Zhu
;; Santiago Ontanon
;; --------------------------------------------------------------------

(setf *story*
  '(STORY-%s
     (:discourse
       (:clauses
%s
       )
       (:templates
%s
%s
       )
     )
     (:structure
       %s  
%s
     )
   )
)
    ''' % (story_id, phases, templates_comment, templates_text, structure_common, structure_phases)

def get_riu_runner(num, suffix):
    template = '''
;; Riu

(defvar *sme-loaded* nil)
(defvar *riu-debug* '())
(setf *riu-debug* '())
;; (setf *riu-debug* '(analogy discourse retrieval prediction intentionality))
;; (setf *riu-debug* '(prediction))

(unless *sme-loaded* (load "sme-load.lisp"))

(load "riu-utils.lisp")
(load "riu-retrieval.lisp")
(load "riu-analogy.lisp")
(load "riu-discourse.lisp")
(load "riu-scene-instantiation.lisp")
(load "riu-bdi.lisp")

(defun generate-story ()
	{}
	(setf complete-stories (list {}))

	(load "voz/story{}-partial-{}.lisp")
	(setf partial-story *story*)

	(setf retrieved-stories (retrieve-K-memories partial-story complete-stories 3 nil))
	(format t "~a~%" (length retrieved-stories))

	(setf *riu-debug* '(show-final-mapping))
	(dolist (source retrieved-stories)
		(generate-analogical-text-general partial-story source)
	)

)

(generate-story)
    '''
    template_load = '''
	(load "voz/story%d-complete-%s.lisp")
	(setf complete-story%d *story*)
    '''

    ret = []
    stories = [i+1 for i in range(num)]
    for i in stories:
        to_load = ''
        to_load_lst = []
        for j in stories:
            if i==j: continue
            to_load += template_load % (j,suffix,j)
            to_load_lst.append('complete-story%d' % j)
        out = template.format(to_load,' '.join(to_load_lst),i,suffix)
        ret.append(out)
    return ret


def filter_mentions(mentions,all_coref_mentions):
    mm = []
    verbmanager.add_children_mentions_to_list(mentions,mm)
    #mm = [i.annotations.coref for i in mm]
    mm = [i for i in mm if i.annotations.coref in all_coref_mentions]
    return mm

def doc_to_sam(doc,do_verbs,do_funcs,do_segment,partial=False,filter_characters=True):
    assert isinstance(doc,voz.Document)
    all_coref_mentions = set()
    for i in doc.get_all_mentions(filter_only_independent=True):
        if not i.annotations.coref:
            i.annotations.coref = i.id
        if i.annotations.coref not in all_coref_mentions and (not filter_characters or i.annotations.is_character()):
            all_coref_mentions.add(i.annotations.coref)

    phases = []

    def process_sentences_to_phase(sentences,functions):
        return (sentences,functions)

    if do_segment=='prep':
        cutoff_0 = None
        cutoff_1 = None
        functions_0 = []
        functions_1 = []
        functions_2 = []
        phase = 0
        for function in doc.narrative.functions():
            if phase==0 and function.function_group in 'alpha,beta,gamma,delta,epsilon,zeta,eta,theta,lambda,A,a,depart'.split(','):
                functions_0.append(function)
                continue
            elif phase==0:
                functions_1.append(function)
                # transition out of preparation, cut here
                if function.locations:
                    cutoff_0 = doc.get_token_by_id(function.locations[0].token_ids[0])
                    phase=1
            elif phase==1 and function.function_group not in 'return,Pr,Rs,o,L,M,N,Q,Ex,T,U,W'.split(','):
                functions_1.append(function)
            elif phase == 1 and function.function_group in 'return,Pr,Rs,o,L,M,N,Q,Ex,T,U,W'.split(','):
                if function.locations:
                    cutoff_1 = doc.get_token_by_id(function.locations[0].token_ids[0])
                    phase=2
            elif phase==2:
                functions_2.append(function)
                pass
        sentences = []
        cutoff = cutoff_0
        for sentence in doc.sentences:
            if not cutoff or cutoff and sentence.offset < cutoff.offset:
                sentences.append(sentence)
            else:
                if cutoff==cutoff_0:
                    phases.append(process_sentences_to_phase(sentences,functions_0))
                    sentences = []
                    cutoff = cutoff_1
                elif cutoff==cutoff_1:
                    phases.append(process_sentences_to_phase(sentences,functions_1))
                    sentences = []
                    cutoff = None
        phases.append(process_sentences_to_phase(sentences, functions_2))

    if partial:
        phases = [phases[0]]

    discourse,templates_comments,phases_text,phases_struct = get_sam_phases(phases,all_coref_mentions,do_verbs,do_funcs)

    return get_sam_template(doc.id,discourse,templates_comments,phases_text,get_sam_common(),phases_struct)


def main():
    stories_in_use = settings.STY_FILES
    for suffix_v in ['noverb','basicverb']:
        for suffix_f in ['nofunc','func']:
            for suffix_s in ['prep']:
                for suffix_g in ['sty']:
                    suffix = suffix_v + '_' + suffix_f + '_' + suffix_s + '_' + suffix_g
                    # write the scripts
                    for i,s in enumerate(get_riu_runner(len(stories_in_use),suffix)):
                        with open(export_path+'voz-eval-'+str(i+1)+'-'+suffix+'.lisp','w') as f:
                            f.write(s)
                        #break
                    # write the stories
                    for sty_file in stories_in_use:
                        if suffix_g == 'sty':
                            doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH + sty_file)
                            styhelper.fix_sty_annotations(doc)
                        else:
                            pass
                            #doc = stanfordhelper.create_document_using_stanford_from_filtered_sty_file(settings.STY_FILE_PATH + sty_file)
                            #doc = stanfordhelper.create_document_from_raw_text(text,{'cache':False})
                            #doc.compute_predictions()
                        out = doc_to_sam(doc,suffix_v,suffix_f,suffix_s,partial=False)
                        with open(export_path+('voz/story%d' %doc.id)+'-complete-%s.lisp'%suffix,'w') as f:
                            f.write(out)
                        out = doc_to_sam(doc,suffix_v,suffix_f,suffix_s,partial=True)
                        with open(export_path+('voz/story%d' %doc.id)+'-partial-%s.lisp'%suffix,'w') as f:
                            f.write(out)

                        #break



if __name__ == '__main__':
    main()