grammar SB;

program : line+ EOF;

line :
	lineNumber? stmtlist? Newline
	| lineNumber Colon Newline
	;

stmtlist : stmt (':' stmt)*; 

stmt :
	Dimension ID parenthesizedlist		 												#Dim
	| Local unparenthesizedlist															#Loc
	| Implic unparenthesizedlist														#Implicit
	| Refer unparenthesizedlist															#Reference155
	| prochdr line* lineNumber? endDef ID?												#Proc
	| funchdr line* lineNumber? endDef ID?		 										#Func
    | For ID Equal expr To expr (Step expr)?
        (
          // Long form with remark
          Colon Comment Newline line* lineNumber? endFor ID?
           
          // Short form
          | Colon stmtlist
     
          // Long form without remark
          | Newline line* lineNumber? endFor ID?
        )
         #Forloop
                 
    | Repeat significantIdentifier 
      ( Colon stmtlist                                    // Short form
      | Newline line* lineNumber? endRepeat ID?           // Long form
      )                                                                                 #Repeat

    | If expr 
      ( (Then | Colon) stmtlist (Colon Else Colon stmtlist)?                   // Short form
      | (Then)? Newline line+ (lineNumber? Else line+)? lineNumber? endIf      // Long form
      )                                                                                 #If

    | Select constexpr Newline line* lineNumber? endSelect								#Longselect
	| Comment																			#Remark
 	| On (constexpr) Equal rangeexpr													#Onselect
	| Next ID																			#Nextstmt
	| Exit ID																			#Exitstmt
	| ID (parenthesizedlist)? Equal expr												#Assignment
	| ID (unparenthesizedlist)?															#ProcCall
	| identifier																		#IdentifierOnly
	;

prochdr : DefProc identifier parenthesizedlist? Newline;
funchdr : DefFunc identifier parenthesizedlist? Newline;
identifier : ID (parenthesizedlist)?;
parenthesizedlist :	LeftParen expr (separator expr)* RightParen;
unparenthesizedlist : expr (separator expr)*;
separator : Comma | Bang | Semi | To;
constexpr : Integer | Real | String | ID;
rangeexpr : constexpr To constexpr | constexpr;
unaryTerminator : (Minus Integer | Minus Real | Minus identifier);
significantIdentifier : identifier;

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
    
    Relies on Antlr's precedence rules to handle the precedence of the operators
*/
expr
    : LeftParen expr RightParen                                                         #Parenthesized
    | expr Amp expr                                                                     #Binary //Concatenation
    | expr (Plus | Minus) expr                                                          #Binary //Add/Subtract
    | expr (Multiply | Divide | Mod | Div) expr                                         #Binary //Multiply/Divide
    | expr Caret expr                                                                   #Exponentiation
    | expr (Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual) expr          #Comparison
    | expr And expr                                                                     #Logical //AND
    | expr (Or | Xor) expr                                                              #Logical //OR/XOR
    | Not expr                                                                          #Unary //Not
    | (Plus | Minus)? primary                                                           #Unary //plus/minus
    | (Integer | Real | String | identifier)                                            #Term
    ;
    
primary
    : LeftParen expr RightParen
    | Integer
    | Real
    | String
    | identifier
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
Real : '-'? DIGIT* Point DIGIT*;
String : '"' ~('"')* '"';

Unknowntype : 'program use only';
Void : 'program use only';

fragment LETTER : [a-zA-Z];
fragment DIGIT : [0-9];
fragment ESC : '\\"' | '\\\\' ;
