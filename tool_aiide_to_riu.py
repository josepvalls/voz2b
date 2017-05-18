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

def get_sam_common(roles_as_entities=True):
    ret = '''
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
%s
         )
         (:expressions
         )
       )
    '''
    if roles_as_entities:
        ret = ret % '''
           (role :type entity)
           (Hero :type role)
           (Villain :type role)
           (Tester :type role)
           (Prize :type role)
           (FalseHero :type role)
           (Other :type role)'''
    else:
        ret = ret % ''
    return ret

verbmapper = verbmanager.VerbMapper()

def get_sam_verb(verb,do_verb):
    if do_verb=='basicverb':
        return 'verb'
    elif do_verb=='levinverb':
        ret = verbmapper.map(verb.token.lemma,verbmanager.VerbMapper.MODE_LEVIN_TEXT,fallback=False)
        if not ret: return None
        ret = 'levin-'+ret.replace('.','-')
        return ret

def get_start_end(tokens_lsts):
    start_i = float('inf')
    end_i = float('-inf')
    start = None
    end=None
    for lst in tokens_lsts:
        if lst[0].offset<start_i:
            start = lst[0]
            start_i = start.offset
        if lst[-1].offset>end_i:
            end = lst[-1]
            end_i = end.offset
    return start,end


def remove_overlaps(substitutions_expressions):
    substitutions_expressions.sort(key=lambda i:i[0][0].offset)
    last = float('-inf')
    ret = []
    for start_end, verb_string in substitutions_expressions:
        start,end = start_end
        if start.offset>last:
            ret.append((start_end, verb_string))
            last = end.offset
    return ret


def get_sam_phases(phases,all_coref_mentions,do_verbs,do_funcs,do_roles):
    roles_as_expressions = True
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
    template_i = 1000
    expressions_extra_func = 0

    mentions = []
    discourse = ''
    for sentences, functions in phases:
        expressions_verbs = ''
        expressions_roles = ''
        entities_roles = ''
        templates_funcs = ''
        expressions_funcs = ''
        templates_roles = ''
        substitutions = {}
        substitutions_expressions = []
        phase_i+=1
        discourse_sentence = ''
        sentence_i_bak = sentence_i
        added_expressions_remove_redundant = set()
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
                expression_tokens = []
                if subjects or objects:
                    if subjects:
                        mentions.append(subjects[0])
                    if objects:
                        mentions.append(objects[0])
                if subjects and objects:
                    if subjects:
                        expression_tokens.append(subjects[0].tokens)
                        subject_id = subjects[0].annotations.coref
                        substitutions[subjects[0]] = subject_id
                    else:
                        subject_id = -1
                    if objects:
                        expression_tokens.append(objects[0].tokens)
                        object_id = objects[0].annotations.coref
                        substitutions[objects[0]] = object_id
                    else:
                        object_id = -1
                    if do_verbs:
                        verb_string = get_sam_verb(verb,do_verbs)
                        expression_tokens.append([verb.token])
                        if not verb_string: continue
                        verb_string = '%s m%s m%s' % (verb_string, subject_id, object_id)
                        if verb_string not in added_expressions_remove_redundant:
                            substitutions_expressions.append((get_start_end(expression_tokens),verb_string))
                            added_expressions_remove_redundant.add(verb_string)
        entities = {}
        for i in mentions:
            entities[i.annotations.coref] = i

        current_hero = None
        if roles_as_expressions:
            for key in entities.keys():
                role = entities[key].annotations.role
                if role=='Hero':
                    current_hero = key
                if role and ',' not in role:
                    template_i +=1
                    expressions_roles += '           ((role%s m%s) :name EXTRA%d)\n' % (role,key,expression_extra_i)
                    if do_roles=='roleent':
                        pass
                        #templates_roles += '         (t%d (m%s "%s") " is the " (%s "%s") ".")\n' % (template_i, key, entities[key].get_text(), role, role)
                    if do_roles=='roleexp':
                        expression_extra_i += 1
                        templates_roles += '         (t%d (EXTRA%d  (m%s "%s") " is the " (%s "%s") ) ".")\n' % (template_i,expression_extra_i,key,entities[key].get_text(),role,role)
        add_func_tuple = None
        if do_funcs == 'functs':
            if functions and current_hero:
                add_func_tuple = ('funcFiller-'+functions[0].function_group,'m%d'%current_hero)

        substitutions_expressions_=[]
        for start_end, verb_string in remove_overlaps(substitutions_expressions):
            expression_i += 1
            expression_name = 'E%d' % expression_i
            expressions_verbs += '           ((%s) :name %s)\n' % (verb_string, expression_name)
            substitutions_expressions_.append((expression_name,start_end, verb_string ))
        substitutions_expressions = substitutions_expressions_
        if add_func_tuple:
            expressions_extra_func += 1
            template_i +=1
            expressions_funcs = '           ((%s %s) :name EXTRAF%d)\n' % (add_func_tuple[0], add_func_tuple[1],expressions_extra_func)
            templates_funcs += '         (t%d (EXTRAF%d  (m%s "%s") " fulfills %s.")\n' % (template_i, expressions_extra_func, add_func_tuple[1], entities[int(add_func_tuple[1].strip('m'))].get_text(), add_func_tuple[0])
            add_func_tuple = [add_func_tuple[0], add_func_tuple[1],expressions_extra_func]


        ret += '       (phase%d\n' % phase_i
        ret += '         (:entities\n'
        for key in entities.keys():
            ret += '           (m%s :type %s)\n' % (key,entities[key].annotations.type)
            if do_roles=='roleent':
                role = entities[key].annotations.role
                if role and ',' not in role:
                    ret += '           (m%s :type %s)\n' % (key, role)

        ret += '         )\n'
        ret += '         (:expressions\n'
        ret += expressions_verbs
        if do_roles=='roleexp':
            ret += expressions_roles
        ret += expressions_funcs


        ret += '         )\n'
        ret += '       )\n'
        # end of phase
        sentence_i = sentence_i_bak
        for sentence in sentences:
            sentence_i += 1
            #templates_templates += '         (t%d "%s"\n' % (sentence_i, sentence.get_text().replace('"', "'"))
            templates_templates += '         (t%d "%s")\n' % (sentence_i, substitute_text(sentence,substitutions,substitutions_expressions,add_func_tuple))
        templates_templates += templates_roles
        templates_templates += templates_funcs


    return discourse,templates_comments,templates_templates,ret

def substitute_text(sentence,substitutions,substitutions_expressions,add_func_tuple):
    ret = ''
    subs_tokens = dict(util.flatten([[(j,i) for j in i.tokens] for i in substitutions.keys()]))
    sent_tokens = list(sentence.tokens)

    #for expression_name, start_end, verb_string in substitutions_expressions:
    current_exp = substitutions_expressions.pop(0) if substitutions_expressions else None
    exp_start = False

    add_func_tuple_do = False
    if add_func_tuple:
        funct,filler,expr = add_func_tuple
        if int(filler.strip('m')) in substitutions.values():
            add_func_tuple.pop()
            add_func_tuple.pop()
            add_func_tuple.pop()
            add_func_tuple_do = True

            ret += '" (EXTRAF%d "' % expr


    while sent_tokens:
        token = sent_tokens.pop(0)
        if current_exp and not exp_start and token.offset>= current_exp[1][0].offset:
            ret += '" (%s "' % current_exp[0]
            exp_start = True
        if exp_start and token.offset>current_exp[1][1].offset:
            ret += '" ) "'
            exp_start = False
            current_exp = substitutions_expressions.pop(0) if substitutions_expressions else None

        if token in subs_tokens:
            mention = subs_tokens[token]
            for _ in mention.tokens[1:]:
                if sent_tokens: sent_tokens.pop(0)
            ret += "\" (m%s \"%s\") \" " % (substitutions[mention],mention.get_text().replace('\n', '').replace('"',"'"))
        else:
            ret += sentence._parent_document.text[token.offset:token.offset + token.len].replace('\n', '').replace('"',"'") + ' '
    if exp_start:
        ret += '" ) "'

    if add_func_tuple_do:
        ret += '" ) "'

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
(proclaim '(optimize (debug 1)))

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

def doc_to_sam(doc,do_verbs,do_funcs,do_roles,do_segment,limit=None,filter_characters=True):
    assert isinstance(doc,voz.Document)
    all_coref_mentions = set()
    for i in doc.get_all_mentions(filter_only_independent=True):
        if not i.annotations.coref:
            i.annotations.coref = i.id
        if i.annotations.coref not in all_coref_mentions and (not filter_characters or i.annotations.is_character()):
            all_coref_mentions.add(i.annotations.coref)

    phases = []

    if do_segment=='prep':
        cutoffs = []
        functions_ = []
        functions = []
        phase = 0
        for function in doc.narrative.functions():
            if phase==0 and function.function_group in 'alpha,beta,gamma,delta,epsilon,zeta,eta,theta,lambda'.split(','):
                functions.append(function)
            elif phase==0 and function.function_group not in 'alpha,beta,gamma,delta,epsilon,zeta,eta,theta,lambda'.split(','):
                if function.locations:
                    cutoffs.append(doc.get_token_by_id(function.locations[0].token_ids[0]))
                    functions_.append(functions)
                    functions = []
                    phase=1
                    functions.append(function)
                else:
                    pass
            elif phase==1 and function.function_group in 'A,a,depart'.split(','):
                functions.append(function)
            elif phase == 1 and function.function_group not in 'A,a,depart'.split(','):
                if function.locations:
                    cutoffs.append(doc.get_token_by_id(function.locations[0].token_ids[0]))
                    functions_.append(functions)
                    functions = []
                    phase=2
                    functions.append(function)
                else:
                    pass
            elif phase==2 and function.function_group in 'B,C,D,E,F,G,H,I,J,K'.split(','):
                functions.append(function)
            elif phase == 2 and function.function_group not in 'B,C,D,E,F,G,H,I,J,K'.split(','):
                if function.locations:
                    cutoffs.append(doc.get_token_by_id(function.locations[0].token_ids[0]))
                    functions_.append(functions)
                    functions = []
                    phase=3
                    functions.append(function)
                else:
                    pass
            elif phase==3:
                functions.append(function) #return,Pr,Rs,o,L,M,N,Q,Ex,T,U,W
        sentences = []
        cutoffs.append(None)
        functions_.append(functions)
        cutoff = cutoffs.pop(0)
        functions = functions_.pop(0)
        for sentence in doc.sentences:
            if not cutoff or cutoff and sentence.offset < cutoff.offset:
                sentences.append(sentence)
            else:
                cutoff = cutoffs.pop(0)
                functions = functions_.pop(0)
                phases.append((sentences,functions))
                sentences = []
        phases.append((sentences, functions))

    if limit:
        phases = phases[0:limit]

    discourse,templates_comments,phases_text,phases_struct = get_sam_phases(phases,all_coref_mentions,do_verbs,do_funcs,do_roles)

    return get_sam_template(doc.id,discourse,templates_comments,phases_text,get_sam_common(do_roles=='roleent'),phases_struct)


def main():
    script = ''
    stories_in_use = settings.STY_FILES#[0:3]
    for suffix_v in ['levinverb']: # noverb, 'basicverb' levinverb
        for suffix_f in ['nofunc']:#,'functs']:
            for suffix_r in ['roleexp']: # 'norole','roleent','roleexp'
                for suffix_s in ['prep']:
                    for suffix_g in ['sty']:
                        suffix = suffix_v + '_' + suffix_f + '_' + suffix_r + '_' + suffix_s + '_' + suffix_g
                        # write the scripts
                        for i,s in enumerate(get_riu_runner(len(stories_in_use),suffix)):
                            fname = 'voz-eval-'+str(i+1)+'-'+suffix+'.lisp'
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
                                pass
                                #doc = stanfordhelper.create_document_using_stanford_from_filtered_sty_file(settings.STY_FILE_PATH + sty_file)
                                #doc = stanfordhelper.create_document_from_raw_text(text,{'cache':False})
                                #doc.compute_predictions()
                            out = doc_to_sam(doc,suffix_v,suffix_f,suffix_r,suffix_s,limit=2)
                            with open(export_path+('voz/story%d' %doc.id)+'-complete-%s.lisp'%suffix,'w') as f:
                                f.write(out)
                            out = doc_to_sam(doc,suffix_v,suffix_f,suffix_r,suffix_s,limit=1)
                            with open(export_path+('voz/story%d' %doc.id)+'-partial-%s.lisp'%suffix,'w') as f:
                                f.write(out)

                        #break
    with open(export_path+'main.sh','w') as f:
        f.write(script)


def main_get_levin_language():
    verbmapper.map('walk', verbmanager.VerbMapper.MODE_LEVIN_TEXT)
    for i in sorted(set(verbmapper._verb_mapping_cache[verbmanager.VerbMapper.MODE_LEVIN_TEXT].values())):
        print '(sme:defPredicate levin-%s (entity entity) relation :expression-type action)' % i.replace('.', '-')


if __name__ == '__main__':
    #main_get_levin_language()
    #sys.exit()
    main()