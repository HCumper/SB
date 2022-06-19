grammar SB;

program : line+ EOF;

line :
	lineNumber? (stmtlist)? Newline
	| lineNumber Colon Newline
	;

stmtlist : stmt (':' stmt?)*;

stmt :
	Dimension ID parenthesizedlist														#Dim
	| Local unparenthesizedlist															#Loc
	| Implic unparenthesizedlist														#Implicit
	| Refer unparenthesizedlist															#Reference
	| prochdr line* lineNumber? endDef ID?												#Proc
	| funchdr line* lineNumber? endDef ID?												#Func
	| For ID Equal expr To expr (Step terminator)? Newline line* lineNumber? endFor ID?	#Longfor
	| For ID Equal expr To expr Colon stmtlist											#Shortfor
	| Repeat ID Colon stmtlist															#Shortrepeat
	| Repeat ID Newline line* lineNumber? endRepeat ID?									#Longrepeat
	| If expr (Then | Colon) stmtlist (Colon Else Colon stmtlist)?						#Shortif
	| If expr (Then)? Newline line+ (lineNumber? Else line+)? lineNumber? endIf			#Longif
    | Select constexpr Newline line* lineNumber? endSelect								#Longselect
	| Comment																			#Remark
 	| On (constexpr) Equal rangeexpr													#Onselect
	| Next ID																			#Nextstmt
	| Exit ID																			#Exitstmt
	| identifier Equal expr																#Assignment
	| identifier																		#IdentifierOnly
	;

prochdr : DefProc identifier parenthesizedlist? Newline;
funchdr : DefFunc identifier parenthesizedlist? Newline;
identifier : ID (parenthesizedlist | unparenthesizedlist)?;
parenthesizedlist :	LeftParen expr (separator expr)* RightParen;
unparenthesizedlist : expr (separator expr)*;
separator : Comma | Bang | Semi | To;
constexpr : Integer | Real | String | ID;
rangeexpr : constexpr To constexpr | constexpr;
terminator : Integer | String | Real | identifier;

lineNumber : Integer;
endFor : EndFor;
endDef  : EndDef;
endRepeat : EndRepeat;
endIf : EndIf;
endSelect : EndSelect;

/*
    SB has 11 levels of operator precedence
    (1)  Unary plus and minus
    (2)  String concatenation
    (3)  String search (the INSTR operator)
    (4)  Exponentiation
    (5)  Multiplication, Division, Modulus calculation
    (6)  Addition and Subtraction
    (7)  Logical comparison (not equal, greater, etc)
    (8)  Unary logical NOT
    (9)  Logical AND
    (10) Logical OR and XOR
    (11) Expressions (inc. Function parameters and array
         subscripts) enclosed in parentheses.
    Every terminal and expression must have an associated precedence to determine whether parentheses are needed
*/

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
	| terminator																	#Term
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
EndDef  : 'END DEFine';
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
       
Whitespace : [ \t]+ -> skip;

Newline : (( '\r' '\n') |   '\n');
Let : 'LET' -> skip;
Comment	:  'REMark' ~( '\r' | '\n' )*;

ID : LETTER ([0-9] | [A-Za-z] | '_')* ('$'|'%')?;

Integer : DIGIT+;
Real : DIGIT* Point DIGIT*;
String : '"' ~('"')* '"';

Unknowntype : 'program use only';
Void : 'program use only';

fragment LETTER : [a-zA-Z];
fragment DIGIT : [0-9];
fragment ESC : '\\"' | '\\\\' ;
