/**
 * Objective-J grammar for Antlr-4 version 1
 *
 * This grammar includes Javascript grammar as Objective-J requires any JS code can also be
 * valid Objective-J code.
 *
 * I used an Objective-C grammar written by Cedric Cuche as a basis.
 * The Objective-C grammar is available at ANTLR3 grammar list page http://www.antlr3.org/grammar/list.html
 *
 * Sep, 2012 noda yoshikazu
 *
 * Licence
 * (The MIT License)
 * 
 * Copyright (c) 2013 Noda Yoshikazu <noda.yoshikazu@gmail.com>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this
 * software and associated documentation files (the 'Software'), to deal in the Software 
 * without restriction, including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
 * to whom the Software is furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all copies 
 * or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
 * PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
 * FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 **/

grammar objj;

options {
  language=Java;
}


program: external_declaration+ EOF;

external_declaration
    : preprocessor_declaration
    | import_statement
    | class_implementation
    | variable_declaration
    | declaration 
    | function_definition
    | class_declaration_list
    | js_function_definition
    | statement    /* for javascript */
    | init_declarator_list ';'
    ;

import_statement
    : '@import' file_specification
    ;
        
preprocessor_declaration
    : '#define' expression expression
    | '#elif' expression
    | '#else'
    | '#ifdef' expression
    | '#ifndef' expression
    | '#undef' expression
    | '#endif'
    | '#if' expression
    ;

simple_funcall
    : _identifier '(' constant ')'
    ;

file_specification
    : '<' ( IDENTIFIER ('/' | '\\' | '.')? )+ '>'
    | STRING_LITERAL
    ;


function_definition 
    : declaration_specifiers declarator compound_statement
    ;

/****** Javascript function *******/
js_function_definition
	: 'function' _identifier '(' ( parameter_list )? ')' compound_statement
	;
	
/* Closure style functions
    e.g. window.setTimeout(function() { [self reload]; }, 0);
*/
js_function_expression
    : 'function' _identifier? '(' ( parameter_list )? ')' compound_statement
	;

js_new_expression
	: 'new' _identifier ( '.' _identifier )* '(' ( argument_expression_list )? ')' ( js_member_expression_suffix )*
	| 'new' _identifier ( '.' _identifier )* 
	;
	
js_member_expression_suffix
	: '[' expression ']'
	| '.' _identifier
	;

js_object_literal
	: '{' js_property_nameandvalue ( ',' js_property_nameandvalue )* '}'
	;
	
js_property_nameandvalue
	: js_property_name ':' assignment_expression
	;

js_property_name
	: _identifier
	| constant
	;

js_throw_statement
	: 'throw' expression ';'
	;

/****** end Javascript function *******/

class_declaration_list
    : ( '@class' class_list ';' )
	;

class_list
    : class_name ( ',' class_name )*
    ;

class_implementation
    : ATIMPLEMENTATION ( class_name (':' superclass_name)?
            ( protocol_reference_list )? 
            ( instance_variables )?
            ( implementation_definition_list )?
        )
        ATEND 
    ;

variable_declaration
	: 'var' variable_declaration_list  (';')?
	;
	
variable_declaration_list
	: var_decl ( ',' var_decl )*
	;
	
var_decl
	: _identifier ( '=' expression )?
    | expression
	;

class_name
    : _identifier
    ;

superclass_name
    : _identifier
    ;

category_name
    : _identifier
    ;

implementation_definition_list
	: (function_definition 
        | declaration 
        | class_method_definition 
        | instance_method_definition 
        | pragam_specification
      )+
    ;

pragam_specification
    : '#pragma' ( _identifier | simple_funcall )+ ( '-' )?
    ;

class_method_definition
    : ('+' method_definition )
	;

instance_method_definition
    : ('-' method_definition )
	;

method_definition
    : (method_type)? method_selector ( init_declarator_list )?  compound_statement
    ;

method_selector
    : selector 
    | (keyword_declarator+ ( parameter_list )? )
	;

keyword_declarator
    : selector? ':' method_type* IDENTIFIER
    ;

parameter_list 
    : parameter_declaration_list ( ',' '...' )? 
    ;

parameter_declaration_list
    : parameter_declaration ( ',' parameter_declaration )* 
    ;

parameter_declaration 
    : declaration_specifiers (declarator? | abstract_declarator) 
    ;

init_declarator_list 
    : init_declarator ( ',' init_declarator )* 
    ;

init_declarator 
    : declarator ('=' initializer)?
    ;

/***STMT*****************************/
compound_statement
    : '{' ( statement )* '}'
    ;

statement 
    : labeled_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    | ';' 
    | expression (';')?
    | try_block
    | variable_declaration    /* objj */
    | declaration             /* objj */
    | preprocessor_declaration
    | js_throw_statement
    | compound_statement
    ;

iteration_statement
    : 'while' '(' expression ')' statement
    | 'do' statement 'while' '(' expression ')' ';'
    | 'for' '(' expression? ';' expression? ';' expression? ')' statement 
    | 'for' '(' 'var' variable_declaration_list ';' expression? ';' expression? ')' statement   /* javascript */
	| 'for' '(' ('var')? var_decl 'in' expression ')' statement     /* javascript */
	;
	
labeled_statement
    : _identifier ':' statement
    | 'case' constant_expression ':' statement
    | 'default' ':' statement 
    ;

selection_statement
    : 'if' '(' expression ')' statement ( 'else' statement )?
    | 'switch' '(' expression ')' statement 
    ;

jump_statement
    : 'goto' _identifier ';'
    | 'continue' ';'
    | 'break' ';'
    | 'return' expression? (';')?
    ;

try_statement
    : ATTRYSTATEMENT
    ;

catch_statement
    : ATCATCH '(' exception_declarator ')' statement
    ;

exception_declarator
    : declarator
    ;

finally_statement
    : ATFINALLY statement
    ;

throw_statement
    : ATTHROW '('IDENTIFIER')'
    ;

try_block
    : try_statement catch_statement	( finally_statement )?
    ;

synchronized_statement
    : ATSYNCHRONIZED '(' IDENTIFIER ')' statement
    ;

/***DECL*****************************/
declaration 
    : init_declarator_list? ';'    /* change for objj */
    ;

declaration_specifiers 
    : ( type_specifier )*  
    ;

init_declarator_list
    : init_declarator ( ',' init_declarator )* 
    ;

init_declarator 
    : declarator ( '=' initializer )? 
    ;

initializer 
    : assignment_expression
    | '{' initializer ( ',' initializer )* '}' 
    ;

struct_declaration 
    : specifier_qualifier_list struct_declarator_list ';' 
    ;

specifier_qualifier_list 
    : ( type_specifier )+ 
    ;

struct_declarator_list 
    : struct_declarator ( ',' struct_declarator )*
    ;

struct_declarator 
    : declarator 
    | declarator? ':' constant
    ;

/***end DECL*************************/
/*
BNF
interface-declaration-list:
   declaration
  |method-declaration
  |interface-declaration-list  declaration
  |interface-declaration-list  method-declaration
*/

interface_declaration_list
    : declaration 
    | method_declaration 
    | interface_declaration_list  declaration
    | interface_declaration_list  method_declaration 
    ;

method_declaration
    : class_method_declaration
    | instance_method_declaration
    ;

class_method_declaration
    : '+' ( method_type )? method_selector  ';'
    ;

instance_method_declaration
    : '-' ( method_type )?  method_selector  ';'
    ;


instance_variables
    : '{' ( ( visibility_specification )? struct_declarator_list ( instance_variables )? )* '}'   
	;

visibility_specification
    : ATPRIVATE
	| ATPROTECTED
	| ATPUBLIC
    ;

struct_declarator_list
    : struct_declarator ( ',' struct_declarator )* 
    ;

struct_declarator 
    : declarator ';'
    | declarator? ':' constant ';'
    ;

declarator
    : direct_declarator 
    | ( ATOUTLET | IBOUTLET )? type_specifier direct_declarator  ATACCESSORS?    /* objj additon */
    | ( ATOUTLET | IBOUTLET )? typedef_name declarator  ATACCESSORS?             /* objj additon */
    ;

direct_declarator 
    : _identifier declarator_suffix*
    | '(' declarator ')' declarator_suffix* 
    ;

declarator_suffix
    : '[' constant_expression? ']'
    | '(' parameter_list? ')'
    ;

parameter_list
    : parameter_declaration_list ( ',' '...' )? 
    ;

parameter_declaration_list
    : parameter_declaration ( ',' parameter_declaration )* 
    ;

parameter_declaration 
    : declaration_specifiers (declarator? | abstract_declarator) 
    | 'self'    /* javascript */
    ;

protocol_reference_list
    : ('<' protocol_list '>')
    ;

protocol_list
    : protocol_name ( ',' protocol_name )*
    ;

protocol_name
    : _identifier 
    ;

/***EXPR*************************/
expression
    : assignment_expression (',' assignment_expression )* 
    ;

argument_expression_list
    : assignment_expression ( ',' assignment_expression )* 
    ;

assignment_expression 
    : conditional_expression ( assignment_operator assignment_expression )? 
    ;

assignment_operator
    : '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=' | '>>>=' 
    ;

constant_expression
    : conditional_expression
    ;

conditional_expression
    //: logical_or_expression  ( '?' logical_or_expression ':' logical_or_expression )? 
    : logical_or_expression  ( '?' expression ':' expression )? 
    ;

logical_or_expression
    : logical_and_expression ('||' logical_and_expression )* 
    ;

logical_and_expression
    : inclusive_or_expression ('&&' inclusive_or_expression )* 
    ;

inclusive_or_expression
    : exclusive_or_expression ('|' exclusive_or_expression )* 
    ;

exclusive_or_expression
    : and_expression ('^' and_expression)* ;

and_expression 
    : equality_expression ('&' equality_expression)* 
    ;

equality_expression 
    /* === and !== for javascript */
    : relational_expression (('!=' | '==' | '===' | '!==' ) relational_expression)* 
    ;

relational_expression
    /* instanceof and in are for javascript */
    : shift_expression (('<' | '>' | '<=' | '>=' | 'instanceof' | 'in' ) shift_expression)* 
    ;

shift_expression 
    : additive_expression (('<<' | '>>') additive_expression)* 
    ;

additive_expression
    : multiplicative_expression (('+' | '-') multiplicative_expression)* 
    ;

multiplicative_expression
    : unary_expression ( ('*' | '/' | '%') unary_expression )*    /* no cast expression for objj */
    ;

unary_expression 
    : postfix_expression
    | '++' unary_expression
    | '--' unary_expression
    | unary_operator unary_expression   /* no cast expression for objj */
    ;

unary_operator
    : '&' | '*' | '-' | '~' | '!' 
    ;

postfix_expression
    : primary_expression
        ( '[' ']' 
        | '[' expression ']' 
        | '(' ')'
        | '(' argument_expression_list ')'
        | '.' _identifier
        | '++'
        | '--'
        )* 
    ;

primary_expression
	: 'self'
    | 'true' | 'false'
    | 'defined' '(' expression ')'  /*  for prepocessing */
    | 'typeof' _identifier                 /* javascript */
//    | '[' expression? ']'     /* objj array? DIES */
    | '[' expression ',' expression ( ',' expression )* ']'   /* objj array? DIES */
	| message_expression        /*   '[' receiver message_selector ']'  */
	| selector_expression
	| protocol_expression
	| encode_expression
    | js_function_expression     /* js additon */
    | js_new_expression          /* js additon */
    | js_object_literal          /* javascript */
    | '{' STRING_LITERAL ':' primary_expression ( ','  STRING_LITERAL ':' primary_expression )* '}'  /* js obj array? */
    | '{' '}'                                                 /* js empty obj array? */
	| constant
    | _identifier
	| '(' expression ')'
    ;

message_expression
    : '[' receiver message_selector ']' 
	;

receiver
    : expression
    | class_name 
	| 'super'
    ;

message_selector
    : selector
	| keyword_argument+
    | 'new'
    ;

keyword_argument
    : selector? ':' expression
    | selector? ':' '[' expression ']'    /* objj */
    ;

selector_expression
    : ATSELECTOR '(' selector_name ')'
    ;

selector_name
    : selector
	| (selector? ':')+
    ;

protocol_expression
    : ATPROTOCOL '(' protocol_name ')'
    ;

protocol_name
    : _identifier 
    ;

encode_expression
    : ATENCODE '(' type_name ')'
    ;

constant
    : DECIMAL_LITERAL 
    | HEX_LITERAL 
    | OCTAL_LITERAL 
    | FLOATING_POINT_LITERAL
    | STRING_LITERAL
    | REGEX_LITERAL     /* javascript crap */ 
    ;

STRING_LITERAL
    //:  '@'? '"' ( EscapeSequence | ~('\\'|'"') )* '"'   /* @".." for NSString */
    :  '@' '"' ('\\"'|.)* '"'   /* @".." for NSString */
    |  '"' ('\\"'|.)* '"'
    | '\'' ('\''|.)* '\''    /* JS string literal with single quote */
    ;

HEX_LITERAL 
    : '0' ('x'|'X') [0-9a-fA-F]+ [uUlL]?
    ;

DECIMAL_LITERAL
    : ('0' | [1-9][0-9]*) [uUlL]?
    ;

OCTAL_LITERAL 
    : '0' [0-7]+ [uUlL]? 
    ;

FLOATING_POINT_LITERAL
    : [0-9]+ ('.' [0-9]*)? Exponent? [fFdD]?
	;

Exponent 
    : [eE][+\-]?[0-9]+
    ;

EscapeSequence
    : '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    | OctalEscape
    ;

OctalEscape
    : '\\' [0-3][0-7][0-7]
    | '\\' [0-7][0-7]
    | '\\' [0-7]
    ;

UnicodeEscape
    : '\\' 'u' [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]
    ;

/********************************/
selector
    : _identifier
    ;

method_type
    : '(' type_name ')'
    ;

type_name 
    : specifier_qualifier_list abstract_declarator 
    ;

specifier_qualifier_list 
    : ( type_specifier | typedef_name )+
    ;

abstract_declarator 
    : '(' abstract_declarator ')' abstract_declarator_suffix+
    | ( '[' constant_expression? ']' )+
    | 
    ;

abstract_declarator_suffix
    : '[' constant_expression? ']'
    | '('  parameter_declaration_list? ')' 
    ;

parameter_declaration_list
    : parameter_declaration ( ',' parameter_declaration )* 
    ;

declaration_specifiers 
    : ( type_specifier )+ 
    ;

type_specifier
    : 'void'
    | 'var'   /* javascript only */
    | IBACTION  
    | ATACTION
	| (class_name ( protocol_reference_list )?)
    ;

typedef_name
    : _identifier      /* This is a Yoshi addition */
    ;

WS  :  (' '|'\r'|'\t'|'\u000C'|'\n') -> skip
    ;

LINE_COMMENT
    //: '//' ~('\n'|'\r')* '\r'? ('\n'|EOF)    -> skip
    : '//' .* '\n'    -> skip
    ;

COMMENT
    : '/*' .* '*/'         -> skip
    ;

/* for javascript regular expression. this must come after comment! */
REGEX_LITERAL   
    : '/' ~('*'|'/') ('\\/'| ~('\n') )* '/' [a-z]*
    ;


ATIMPLEMENTATION : '@implementation' ;
ATINTERFACE : '@interface' ;
ATPROTOCOL : '@protocol' ;
ATSELECTOR : '@selector' ;
ATENCODE : '@encode' ;
ATEND : '@end' ;
ATPRIVATE : '@private' ;
ATPROTECTED : '@protected' ;
ATPUBLIC : '@public' ;
ATTRYSTATEMENT : '@trystatement' ;
ATCATCH : '@catch' ;
ATFINALLY : '@finally' ;
ATTHROW : '@throw' ;
ATSYNCHRONIZED : '@synchronized' ;
ATPROTOCOL : '@protocol' ;
ATOUTLET : '@outlet' ;
ATACCESSORS : '@accessors' ;
ATACTION : '@action' ;
IBOUTLET : 'IBOutlet' ;
IBACTION : 'IBAction' ;

_identifier
    : a=IDENTIFIER { /*System.out.println ($a.text);*/ }
    ;

IDENTIFIER
	: [_A-Za-z][A-Za-z0-9_]*
	;

