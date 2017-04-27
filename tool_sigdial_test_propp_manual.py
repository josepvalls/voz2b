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
            weights = {}
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

def load_rules_followup():
    rules = [
        'Q1 Q2 > Q1.l=Q2.s Q2.l=Q1.s',
        'Q2 Q1 > Q1.l=Q2.s Q2.l=Q1.s',
        'Q1 ?V ?P ?V ?P Q2 > Q1.l=Q2.s Q2.l=Q1.s',
        'Q2 ?V ?P ?V ?P Q1 > Q1.l=Q2.s Q2.l=Q1.s',
        'q~1 ?V ?P ?V ?P q!2 > Q2.s=Q1.s Q2.l=Q1.l',
        'q~2 ?V ?P ?V ?P q!1 > Q2.s=Q1.s Q2.l=Q1.l',
    ]
    return [QuotedSpeechPredictorRule.from_string(i,FOLLOWUP_RULE) for i in rules]

def load_rules_manual():
    # example rules
    # Q S P > Q.s=P
    #   when Quote Said Person is found, assign Person to Quote.speaker
    # Q1 S P1 Q2 R P2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1:
    #   when Quote1 Said Person1 Quote2 Replied Person2 is found, assign Person1 to Quote1.speaker, Person2 to Quote1.listener...
    rules = [
        'Q S P > Q.s=P',
        'Q P S > Q.s=P',
        'P S Q > Q.s=P',
        'S P Q > Q.s=P',
        'S Q P > Q.s=P',
        'P Q S > Q.s=P',
        'Q1 S P1 Q2 R P2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1',
        'Q E P > Q.s=P',
        'Q P E > Q.s=P',
        'P E Q > Q.s=P',
        'E P Q > Q.s=P',
        'E Q P > Q.s=P',
        'P Q E > Q.s=P',
        'Q T ?: P > Q.s=P Q.l=P',
        'Q P T ?. > Q.s=P Q.l=P',
        'P T ?: Q > Q.s=P Q.l=P',
        'T P ?: Q > Q.s=P Q.l=P',
        'T Q P ?. > Q.s=P Q.l=P',
        'P Q T ?. > Q.s=P Q.l=P',
        'Q1 E P1 Q2 R P2 > Q1.s=P1 Q1.l=P2 Q2.s=P2 Q2.l=P1',
        'Q S P > Q.s=P',
        'q~ S P > q~.s=P',
        'q~ S P q! > q~.s=P q!.s=P',
        #'q~1 S P q~2 q!> q~1.s=P q~2.s=P q!.s=P',
        'P S q! > q!.s=P',
        'P R q! > q!.s=P',
        #'P1 S q!1 P2 R q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1', doesn't work with punctuation
        'P1 S q!1 P2 R q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'q!1 q!2 P A > q!1.l=P q!2.s=P',
        'q!1 q~ S P q!2 > Q.s=P Q1.s=P Q2.s=P',
        'P E : Q > Q.s=P',
        'E P : Q > Q.s=P',
        'P S : Q > Q.s=P',
        'S P : Q > Q.s=P',
        'P A : Q > Q.s=P',
        #'A P : Q > Q.s=P',
        'P1 E P2 : Q > Q.s=P1 Q.l=P2',
        'P2 E P1 : Q > Q.s=P1 Q.l=P2',
        'P1 S P2 : Q > Q.s=P1 Q.l=P2',
        #'P2 S P1 : Q > Q.s=P1 Q.l=P2',
        'P1 A P2 : Q > Q.s=P1 Q.l=P2',
        'P2 A P1 : Q > Q.s=P1 Q.l=P2',
        'P1 E ?: q!1 P2 E ?: q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'P1 E ?: q!1 P2 R ?: q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'P1 A ?: q!1 P2 R ?: q!2 > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'P1 E ?: q!1 q!2 P2 E > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'P1 A ?: q!1 q!2 P2 R > q!1.s=P1 q!1.l=P2 q!2.s=P2 q!2.l=P1',
        'P1 E P2 ?: Q > Q.s=P1 Q.l=P2',
        # new:
        # 'P1 V V P2 ?: Q > Q.s=P1 Q.l=P2', #Then the dragon began to implore Nikita:
    ]
    return [QuotedSpeechPredictorRule.from_string(i) for i in rules]

def load_rules_from_permutation_minilang(use_qsa_subset=False):
    d = {'Q': ['Q','q~','q!'],
         'E': ['E','A','R','T','S'],
         }
    if not use_qsa_subset:
        g = [
        minilang_permutations(['{Q}', 'P', '{E}']) + ' ?. > Q.s=P',
        minilang_permutations(['{Q}1', 'P1', '{E}']) + ' ?. ' + minilang_permutations(['{Q}2', 'P2', '{E}']) + ' > Q1.s=P1 Q2.s=P2',
        minilang_permutations(['{Q}1', 'P1', '{E}']) + ' ?. ' + minilang_permutations(['{Q}2', 'P2', '{E}']) + ' > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1',
        '(P {E}|{E} P) ?: {Q} ?. > Q.s=P',
        '(P1 {E}|{E} P1) ?: {Q}1 {Q}2 (P2 {E}|{E} P2) ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1',
        '(P1 {E}|{E} P1) ?: {Q}1 (P2 {E}|{E} P2) ?: {Q}2 ?. > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1',
        '{Q}1 {Q}2 (P {E}|{E} P) ?. > Q2.s=P Q1.l=P',
        '{Q}1 {Q}2 (P {E}|{E} P) ?. {Q}3 > Q2.s=P Q1.l=P Q3.l=P',
        'P1 {E} P2 ?: Q > Q.s=P1 Q.l=P2',
        'q~1 (P {E}|{E} P) q!2 > Q1.s=P Q2.s=P',
        'P ?P2 ?V . {Q} > Q.s=P',
        'P ?V . {Q} > Q.s=P',
        'P1 ?V P2 ?V . {Q} > Q.s=P1 Q.l.P2',
        'P2 ?V P1 ?V . {Q} > Q.s=P1 Q.l.P2',
        'P1 ?V P2 ?V . {Q}1 {Q}2 > Q1.s=P1 Q1.l.P2 Q2.s=P2 Q2.l.P1',
        'P2 ?V P1 ?V . {Q}1 {Q}2 > Q1.s=P1 Q1.l.P2 Q2.s=P2 Q2.l.P1',
        'P ?E ?V ?V ?V ?: {Q} > Q.s=P',
        'P1 ?V (V|E) ?V P2 ?: {Q} > Q.s=P1 Q.l=P2',
        'P2 ?V (V|E) ?V P1 ?: {Q} > Q.s=P1 Q.l=P2',
        ]
    else:
        g = [
        minilang_permutations(['{Q}', 'P', '{E}']) + ' ?V ?. ?: > Q.s=P',
        minilang_permutations(['{Q}1', 'P1', '{E}']) + ' ?V ?. ?: ' + minilang_permutations(
            ['{Q}2', 'P2', '{E}']) + ' > Q1.s=P1 Q2.s=P2',
        minilang_permutations(['{Q}1', 'P1', '{E}']) + ' ?V ?. ?: ' + minilang_permutations(
            ['{Q}2', 'P2', '{E}']) + ' > Q1.s=P1 Q2.s=P2 Q1.l=P2 Q2.l=P1',
        ]
    rules = utterancegenerator.UtteranceGenerator().generate(g, d, verbose=False)
    return [QuotedSpeechPredictorRule.from_string(i) for i in rules]


if __name__=='__main__':
    main()