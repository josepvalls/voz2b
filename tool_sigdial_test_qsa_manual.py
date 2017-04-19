import logging
logger = logging.getLogger(__name__)
from quotedspeechpredictor import *
import qsahelper

def main(verbose = False, DO_MANUAL=False, DO_CROSS_VALIDATION=True):
    logging.basicConfig(level=logging.ERROR)
    logger.setLevel(logging.ERROR)
    logging.root.setLevel(logging.ERROR)

    data_set = {}
    rules_accum = {}
    rules_accum['aggregated'] = [0] * 14
    print 'LOADING DATA'
    files_in_use = settings.QSA_FILES[0:2]
    for story_file in files_in_use:
        output_tuple = qsahelper.tokenized_string_from_qsa_file(settings.QSA_FILE_PATH + story_file)
        data_set[story_file] = output_tuple
    for story_file in (files_in_use if DO_CROSS_VALIDATION else ['TRAINING SET = TEST SET']):
        if DO_CROSS_VALIDATION:
            print 'CROSS VALIDATION', story_file
            clean_assignments(data_set)
            rules = []
            uncovered_quotes = []
            training_tuples = []
            test_tuples = []
            for key, tuple in data_set.items():
                if key == story_file:
                    test_tuples.append(tuple)
                else:
                    training_tuples.append(tuple)
        else:
            training_tuples = test_tuples = data_set.values()

        if DO_MANUAL:
            print 'LOADING MANUAL RULES'
            rules = load_rules_manual()
        else:
            print 'TRAINING/EXTRACTING RULES'
            for output_tuple in training_tuples:
                rules_, uncovered_quotes_ = extract_rules(output_tuple)
                rules += rules_
                uncovered_quotes += uncovered_quotes_

            print 'GENERALIZE/EXPAND RULES', len(rules)
            rules = generalize_rules(rules)

        print 'TRAINING WEIGHTS', len(rules)
        rules_eval = [str(i) for i in rules]
        rules_eval_counts = collections.Counter(rules_eval)
        rules_eval = set(rules_eval)
        for i in rules_eval:
            rules_accum[i] = [0] * 14
        weights = {}
        for rule_type in [None, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE, FOLLOWUP_RULE,
                          FOLLOWUP_RULE, FOLLOWUP_RULE]:
            # for rule_type in [None]:
            for output_tuple in training_tuples:
                rules_to_apply = [i for i in rules if i.rule_type == rule_type]
                output_tuple = predict_quoted_speech(output_tuple, rules_to_apply)
            for output_tuple in training_tuples:
                for rule in rules_eval:
                    e = eval_quoted_speech(output_tuple, rule, laplace=1)
                    rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
            for rule in rules_eval:
                weights[rule] = list(compute_eval_quoted_speech(rules_accum[rule])[1:])
            for output_tuple in training_tuples:
                weighted_assignment(output_tuple, weights)
        vozbase.serialize_to_file(weights, 'weights.json', False)

        if not DO_MANUAL:
            # TODO better way to prune out bad rules? takes too long otherwise
            # laplace=1 makes default accuracy 0.5
            rules = [i for i in rules if weights[str(i)][2] > 0.5]

        print 'TEST ASSIGNMENT', len(rules)
        # rules_accum['aggregated'] = [0] * 14
        for output_tuple in test_tuples:
            output_tuple = predict_quoted_speech(output_tuple, rules)
            weighted_assignment(output_tuple, weights)
            e = eval_quoted_speech(output_tuple, None)
            rule = 'aggregated'
            rules_accum[rule] = [a + b for a, b in zip(rules_accum[rule], e)]
            print 'Story - ' + '\t'.join([str(i) for i in [rule] + list(compute_eval_quoted_speech(e))])
            # break # first cross validation set complete here

    print 'OVERALL ALL STORIES'
    rule = 'aggregated'
    print '\t'.join([str(i) for i in [rule] + list(compute_eval_quoted_speech(rules_accum[rule]))])
    if verbose:
        print_quoted_speech_tuple(output_tuple)
        for rule in rules_accum.keys():
            print '\t'.join([str(i) for i in [rule] + list(compute_eval_quoted_speech(rules_accum[rule]))])


main()