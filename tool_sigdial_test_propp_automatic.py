import logging
logger = logging.getLogger(__name__)
from quotedspeechpredictor import *

def main(verbose = False):
    logging.basicConfig(level=logging.WARN)
    logger.setLevel(logging.WARN)
    logging.root.setLevel(logging.WARN)
    data_set = {}
    rules_accum = {}
    rules_accum['aggregated'] = [0] * 14
    print 'LOADING DATA'
    for story_file in settings.STY_FILES:
        doc = styhelper.create_document_from_sty_file(settings.STY_FILE_PATH + story_file)
        quotedspeechhelper.annotate_sentences(doc, settings.STORY_ALL_SENTENCES,single_sentences_file_story_id=doc.id)
        output_tuple = tokenize_document(doc)
        data_set[story_file] = output_tuple
    for VOTING_METHOD in [0, 1, 2, 3]:
        for story_file in settings.STY_FILES:
            logger.warn('CROSS VALIDATION '+ story_file)
            clean_assignments(data_set)
            rules = []
            uncovered_quotes = []
            training_tuples = []
            test_tuples = []
            for key,tuple in data_set.items():
                if key==story_file:
                    test_tuples.append(tuple)
                else:
                    training_tuples.append(tuple)

            logger.warn('TRAINING/EXTRACTING RULES')
            for output_tuple in training_tuples:
                rules_,uncovered_quotes_ = extract_rules(output_tuple)
                rules += rules_
                uncovered_quotes += uncovered_quotes_

            logger.warn('GENERALIZE/EXPAND RULES %d' %len(rules))
            rules = generalize_rules(rules)

            logger.warn('TRAINING WEIGHTS %d' %len(rules))
            rules_eval = [str(i) for i in rules]
            rules_eval = set(rules_eval)
            for i in rules_eval:
                rules_accum[i] = [0] * 14
            weights = dict([(str(i), [0.25, 0.25]) for i in rules])
            for rule_type in [None, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE]:
            #for rule_type in [None]:
                for output_tuple in training_tuples:
                    rules_to_apply = [i for i in rules if i.rule_type==rule_type]
                    output_tuple = predict_quoted_speech(output_tuple, rules_to_apply)
                for output_tuple in training_tuples:
                    for rule in [str(i) for i in rules_to_apply]:
                        e = eval_quoted_speech(output_tuple, rule, laplace=1)
                        rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
                for rule in [str(i) for i in rules_to_apply]:
                    weights[rule] = list(compute_eval_quoted_speech(rules_accum[rule])[1:])
                for output_tuple in training_tuples:
                    weighted_assignment(output_tuple, weights, VOTING_METHOD)
            vozbase.serialize_to_file(weights,'weights.json',False)

            # better way to prune out bad rules? takes too long otherwise
            # laplace=1 makes default accuracy 0.5
            #rules = [i for i in rules if weights[str(i)][2]>0.5]

            logger.warn('TEST ASSIGNMENT %d' %len(rules))
            #rules_accum['aggregated'] = [0] * 14
            for output_tuple in test_tuples:
                output_tuple = predict_quoted_speech(output_tuple, rules)
                weighted_assignment(output_tuple, weights)
                e = eval_quoted_speech(output_tuple, None)
                rule = 'aggregated'
                rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
                out = 'Story-'+'\t'.join([str(i) for i in [rule] + list(compute_eval_quoted_speech(e))])
                #print out
                logger.warn(out)
            #break # first cross validation set complete here

        print 'OVERALL ALL STORIES'
        rule = 'aggregated'
        print '\t'.join([str(i) for i in [VOTING_METHOD,rule] + list(compute_eval_quoted_speech(rules_accum[rule]))])

if __name__=='__main__':
    main()