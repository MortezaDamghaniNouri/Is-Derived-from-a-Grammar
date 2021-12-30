#lang racket
(provide (all-defined-out))

(struct rule (left_nonterminal terminal right_nonterminal) #:transparent)

(define (append lst string)(
cond [(null? lst) (cons string null)] [(null? (cdr lst)) (cons (car lst) (cons string null))]
     [#t (cons [car lst] [append (cdr lst) string] )]
                            ))


(define (nonterminal_list_extractor gr nonterminal lst)(
cond [(null? gr) lst]
[(equal? (rule-left_nonterminal (car gr)) nonterminal ) (nonterminal_list_extractor (cdr gr) nonterminal (append lst (string-append (rule-terminal (car gr)) (rule-right_nonterminal (car gr)) ) ))]
[#t (nonterminal_list_extractor (cdr gr) nonterminal lst    )]

                                                    ))

(define (last_char_replacer first_string second_string )(
string-append (substring first_string 0 (- (string-length first_string) 1)) second_string
                                                         ))


(define (list_appender first_list second_list) (
cond [(null? second_list) first_list]
[#t (list_appender (append first_list (car second_list)) (cdr second_list) ) ]
                                                ))

(define (middle_list_generator string lst output_lst) (

cond [(null? lst) output_lst]
[#t (middle_list_generator string (cdr lst) (append output_lst (last_char_replacer string (car lst)) ) )]
                                            ))





(define (last_nonterminal_replacer A_list B_list C_list D_list S_list main_strings_list output_list) (
cond [(null? main_strings_list) output_list]
[(equal? (substring (car main_strings_list) (- (string-length (car main_strings_list)) 1 ) (string-length (car main_strings_list)) ) "A") (last_nonterminal_replacer A_list B_list C_list D_list S_list (cdr main_strings_list) (list_appender output_list (middle_list_generator (car main_strings_list) A_list '())  )    )]
[(equal? (substring (car main_strings_list) (- (string-length (car main_strings_list)) 1 ) (string-length (car main_strings_list)) ) "B") (last_nonterminal_replacer A_list B_list C_list D_list S_list (cdr main_strings_list) (list_appender output_list (middle_list_generator (car main_strings_list) B_list '())  )    )]
[(equal? (substring (car main_strings_list) (- (string-length (car main_strings_list)) 1 ) (string-length (car main_strings_list)) ) "C") (last_nonterminal_replacer A_list B_list C_list D_list S_list (cdr main_strings_list) (list_appender output_list (middle_list_generator (car main_strings_list) C_list '())  )    )]
[(equal? (substring (car main_strings_list) (- (string-length (car main_strings_list)) 1 ) (string-length (car main_strings_list)) ) "D") (last_nonterminal_replacer A_list B_list C_list D_list S_list (cdr main_strings_list) (list_appender output_list (middle_list_generator (car main_strings_list) D_list '())  )    )]
[(equal? (substring (car main_strings_list) (- (string-length (car main_strings_list)) 1 ) (string-length (car main_strings_list)) ) "S") (last_nonterminal_replacer A_list B_list C_list D_list S_list (cdr main_strings_list) (list_appender output_list (middle_list_generator (car main_strings_list) S_list '())  )    )]


                                                                                          ))



(define (main_strings_list_generator A_list B_list C_list D_list S_list string_length main_strings_list) (

cond [(zero? (- string_length 1)) main_strings_list]
[#t (main_strings_list_generator A_list B_list C_list D_list S_list (- string_length 1) (last_nonterminal_replacer A_list B_list C_list D_list S_list main_strings_list '()))]





                                                                                        ))

(define (last_character_remover lst output_list)(
cond [(null? lst) output_list]
[#t (last_character_remover (cdr lst) (append output_list (substring (car lst) 0 (- (string-length (car lst)) 1) ) )) ]

                                            ))


(define (search lst string)(
cond [(null? lst) #f]
     [(equal? (car lst) string) #t]
     [#t (search (cdr lst) string)]

                            ))




(define (is_derived gr s)(

search (last_character_remover (main_strings_list_generator (nonterminal_list_extractor gr "A" '()) (nonterminal_list_extractor gr "B" '()) (nonterminal_list_extractor gr "C" '()) (nonterminal_list_extractor gr "D" '()) (nonterminal_list_extractor gr "S" '()) (string-length s) (nonterminal_list_extractor gr "S" '())) '() ) s


                          ))






