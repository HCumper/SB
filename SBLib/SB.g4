grammar SB;

program : line+ EOF;

line :
	Integer? (stmtlist)? Newline
	| Integer Colon Newline
	;

stmtlist : stmt (':' stmt)*;

constexpr : Integer | Real | String | ID;
rangeexpr : constexpr To constexpr
		  | constexpr
;

stmt :
	Dimension ID parenthesizedlist																	#Dim
	| Local unparenthesizedlist																		#Loc
	| Implic unparenthesizedlist																	#Implicit
	| Refer unparenthesizedlist																		#Reference
	| prochdr line* Integer? enddef ID?																#Proc
	| funchdr line* Integer? enddef ID?																#Func
	| For ID Equal expr To expr Newline line* Integer? EndFor ID?									#Longfor
	| For ID Equal expr To expr Colon stmtlist														#Shortfor
	| Repeat ID Colon stmtlist																		#Shortrepeat
	| Repeat ID Newline line* Integer? (EndRepeat ID? /*| { _input.Lt(1).Type == EndDef }?*/)		#Longrepeat
	| If expr (Then | Colon) stmtlist (Colon Else Colon stmtlist)?									#Shortif
	| If expr (Then)? Newline line+ (Integer? Else line+)? Integer? EndIf							#Longif
    | Select constexpr Newline line* Integer? EndSelect												#Longselect
	| On (constexpr) Equal rangeexpr																#Onselect
	| Exit ID?																						#Exitstmt
	| identifier Equal expr																			#Assignment
	| identifier																					#IdentifierOnly
	;

prochdr : DefProc identifier parenthesizedlist? Newline											#Procheader
	;

funchdr : DefFunc identifier parenthesizedlist? Newline											#Funcheader
	;

enddef  : 'END DEFine';

identifier :
	ID (parenthesizedlist | unparenthesizedlist)?;

parenthesizedlist :	LeftParen expr (separator expr)* RightParen									#Parenthesizedl;
unparenthesizedlist : expr (separator expr)*													#Unparenthesized;

separator : Comma | Bang | Semi | To;

expr :
	  LeftParen expr RightParen														#Parenthesized
	| (Plus | Minus) expr															#UnaryAdditive
	| expr Amp expr																	#Binary
	| <assoc=right> (String | ID) Instr expr										#Instr
	| <assoc=right> expr Caret expr													#Binary
	| expr (Multiply | Divide | Mod | Div) expr										#Binary
	| expr (Plus | Minus) expr														#Binary
	| expr (Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual) expr		#Binary
	| Not expr																		#Not
	| expr And expr																	#Binary
	| expr (Or | Xor) expr															#Binary
	| (Integer | String | Real | identifier)										#Term
	;


/* Tokens */
Refer : 'REFERENCE';
Implic : 'IMPLICIT%' | 'IMPLICIT$';
Local : 'LOCal';
Dimension : 'DIM';
DefProc : 'DEFine PROCedure';
DefFunc : 'DEFine FuNction';
If : 'IF';
Else : 'ELSE';
Then : 'THEN';
EndIf : 'END IF';
Select : 'SELect ON';
EndSelect : 'END SELect';
On : 'ON';
For : 'FOR';
Next : 'NEXT';
To : 'TO';
EndFor : 'END FOR';
Step : 'STEP';
Repeat : 'REPeat';
Exit : 'EXIT';
Until : 'UNTIL';
EndRepeat : 'END REPeat';

LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';

Equal : '=';
NotEqual : '<>';
Less : '<';
LessEqual : '<=';
Greater : '>';
GreaterEqual : '>=';

Plus : '+';
Minus : '-';
Multiply : '*';
Divide : '/';
Mod : 'MOD';
Div : 'DIV';

And : 'AND';
Or : 'OR';
Xor : 'XOR';
Caret : '^';
Not : 'NOT';
Tilde : '~';

Instr : 'INSTR';
Amp : '&';
Question : '?';
Colon : ':';
Semi : ';';
Comma : ',';
Point : '.';

Bang : '!';
       
Whitespace
    :   [ \t]+
        -> skip
    ;

Let : 'LET' -> skip;

Newline
    :   (( '\r' '\n') |   '\n') 
	;

String : '"' ~('"')* '"';

Comment
	:  'REMark' ~( '\r' | '\n' )* -> skip
	;

ID : LETTER ([0-9] | [A-Za-z] | '_')* '$'
	| LETTER ([0-9] | [A-Za-z] | '_')* '%'
	| LETTER ([0-9] | [A-Za-z] | '_')*;

Integer : DIGIT+;

Real
	: DIGIT+ Point DIGIT*
	| Point DIGIT+
	;

Unknowntype:;
Void:;
Scalar:;
LineNumber:;

fragment LETTER : [a-zA-Z];
fragment DIGIT : [0-9];
fragment ESC : '\\"' | '\\\\' ;
