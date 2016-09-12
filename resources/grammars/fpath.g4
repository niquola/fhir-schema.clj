grammar fpath;

expr
   : chain
   | op
   ;

op
  : chain '=' ID
  ;

chain
    : step 
    | (step | where | call) ('.' (step | where | call))+
    ;

step
   : ID
   ;

where
    : 'where(' expr ')'
    ;

call
  : ID '(' chain ')'
  | ID '()';

ID: ALPHA ALPHANUM* ;

fragment ALPHA: [a-zA-Z];
fragment ALPHANUM: ALPHA | [0-9];

WS: [ \r\n\t]+ -> skip;