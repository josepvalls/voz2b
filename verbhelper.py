import voz
import sys

def mention_contains_mention(container, mention):
    container_ = set([i.idx for i in container.tokens])
    mention_ = set([i.idx for i in mention.tokens])
    return container_ & mention_

def get_srl_stats_prfa(docs):
    verbs = 0
    v_p = 0
    v_r = 0
    v_i = 0
    v_t = 0
    v_a = 0
    va_p = 0
    va_r = 0
    va_i = 0
    va_t = 0
    va_a = 0
    for doc in docs:
        for sentence in doc.sentences:
            v_p += len(sentence.verbs)
            for verb_ref in sentence.annotations.verbs:
                v_r +=1
                v_t +=1
                verb = [i for i in sentence.verbs if i.token.idx == verb_ref.token.idx]
                if verb:
                    verb = verb[0]
                    v_a +=1
                    v_i +=1
                    verb_args = verb.get_subjects()
                    va_p += len(verb_args)
                    for arg_ref in verb_ref.get_subjects():
                        va_r += 1
                        va_t += 1
                        for arg in verb_args:
                            if mention_contains_mention(arg_ref,arg):
                                verb_args.remove(arg)
                                va_i +=1
                                va_a +=1
                                break
                    verb_args = verb.get_objects()
                    va_p += len(verb_args)
                    for arg_ref in verb_ref.get_objects():
                        va_r += 1
                        va_t += 1
                        for arg in verb_args:
                            if mention_contains_mention(arg_ref,arg):
                                verb_args.remove(arg)
                                va_i +=1
                                va_a +=1
                                break
                else:
                    args_ref = verb_ref.get_subjects() + verb_ref.get_objects()
                    va_r += len(args_ref)
                    va_t += len(args_ref)
    v_p = 1.0 * v_i / v_p if v_p else 0.0
    v_r = 1.0 * v_i / v_r if v_r else 0.0
    v_f = 2.0 * v_p * v_r / (v_p+v_r) if (v_p+v_r) else 0.0
    v_a = 1.0 * v_a / v_t if v_t else 0.0
    va_p = 1.0 * va_i / va_p if va_p else 0.0
    va_r = 1.0 * va_i / va_r if va_r else 0.0
    va_f = 2.0 * va_p * va_r / (va_p+va_r) if (va_p+va_r) else 0.0
    va_a = 1.0 * va_a / va_t if va_t else 0.0
    return v_p,v_r,v_f,v_a, va_p,va_r,va_f,va_a

def do_fix_srl(docs):
    pass