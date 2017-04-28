import logging
logger = logging.getLogger(__name__)
from quotedspeechpredictor import *

def main(DO_MANUAL = False, DO_FOLLOWUP = True, DO_FALLBACK_BASELINE= True, verbose = False):
    logging.basicConfig(level=logging.ERROR)
    #logging.basicConfig(level=logging.INFO)
    for VOTING_METHOD in [0,1,2,3]:
        rules_accum = {}
        if DO_MANUAL:
            rules = load_rules_manual()
        else:
            rules = load_rules_from_permutation_minilang()
        rules_eval = [str(i) for i in rules]
        rules_accum['aggregated'] = [0] * 14
        rules_accum['aggregated_with_baseline'] = [0] * 14

        for story_file in settings.STY_FILES:
            #print story_file
            doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH+story_file)
            #styhelper.fix_sty_annotations(doc)
            quotedspeechhelper.annotate_sentences(doc, settings.STORY_ALL_SENTENCES, single_sentences_file_story_id = doc.id)
            if verbose:
                for sentence in doc.sentences:
                    print sentence.format_quoted_annotations()

            output_tuple = tokenize_document(doc)

            if rules:
                #Assignment
                output_tuple = predict_quoted_speech(output_tuple,rules)
            if verbose:
                print_quoted_speech_tuple(output_tuple)

            logger.warn('TRAINING WEIGHTS ' + str(len(rules)))
            rules_eval = [str(i) for i in rules]
            rules_eval = set(rules_eval)
            for i in rules_eval:
                rules_accum[i] = [0] * 14
            weights = dict([(str(i),0.25) for i in rules])
            for rule_type in [None, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE]:
                rules_to_apply = [i for i in rules if i.rule_type == rule_type]
                output_tuple = predict_quoted_speech(output_tuple, rules_to_apply)
                for rule in [str(i) for i in rules_to_apply]:
                    e = eval_quoted_speech(output_tuple, rule, laplace=1)
                    rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
                for rule in [str(i) for i in rules_to_apply]:
                    weights[rule] = list(compute_eval_quoted_speech(rules_accum[rule])[1:])

            weighted_assignment(output_tuple,weights,VOTING_METHOD)
            e = eval_quoted_speech(output_tuple, None)
            rule = 'aggregated'
            rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
            if DO_FALLBACK_BASELINE:
                rule = 'aggregated_with_baseline'
                output_tuple = predict_quoted_speech_informed(output_tuple,False,rule)
                e = eval_quoted_speech(output_tuple, None)
                rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]

        if verbose:
                print_quoted_speech_tuple(output_tuple)

        #print 'OVERALL ALL STORIES'
        rule = 'aggregated'
        print  '\t'.join([str(i) for i in [VOTING_METHOD,rule]+list(compute_eval_quoted_speech(rules_accum[rule]))])
        rule = 'aggregated_with_baseline'
        print  '\t'.join([str(i) for i in [VOTING_METHOD,rule]+list(compute_eval_quoted_speech(rules_accum[rule]))])

if __name__=='__main__':
    main()