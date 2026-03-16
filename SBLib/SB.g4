grammar SB;

// ============================================================
// Parser Rules
// ============================================================

program
    : line* EOF
    ;

line
    : lineNumber? stmtlist? Newline
    | lineNumber Colon Newline
    ;

stmtlist
    : stmt (Colon stmt)* Colon?
    ;

plainStmtlist
    : stmt (Colon stmt)*
    ;

stmt
    : simpleStmt                                                                      #SimpleStatement
    | procedureDef                                                                    #ProcedureDefinitionStmt
    | functionDef                                                                     #FunctionDefinitionStmt
    | forStmt                                                                         #ForStatement
    | repeatStmt                                                                      #RepeatStatement
    | ifStmt                                                                          #IfStatement
    | selectStmt                                                                      #SelectStatement
    | whenStmt                                                                        #WhenStatement
    ;

simpleStmt
    : Dimension dimList                                                               #Dim
    | Local localList                                                                 #LocalDecl
    | Implic unparenthesizedlist                                                      #ImplicitDecl
    | Refer unparenthesizedlist                                                       #ReferenceDecl
    | Comment                                                                         #Remark
    | Next ID                                                                         #NextStmt
    | Exit ID                                                                         #ExitStmt
    | gotoStmt                                                                        #GotoStatement
    | gosubStmt                                                                       #GosubStatement
    | onGotoStmt                                                                      #OnGotoStatement
    | onGosubStmt                                                                     #OnGosubStatement
    | Return expr?                                                                    #ReturnStmt
    | Data exprList                                                                   #DataStmt
    | Read lvalueList                                                                 #ReadStmt
    | Restore expr?                                                                   #RestoreStmt
    | assignmentStmt                                                                  #AssignmentStatement
    | channelProcCallStmt                                                             #ChannelProcCall
    | procedureCallStmt                                                               #ProcedureCallStatement
    ;

// ============================================================
// GO TO / GO SUB
// ============================================================

gotoStmt
    : Go To expr
    | GoTo expr
    ;

gosubStmt
    : Go Sub expr
    | GoSub expr
    ;

onGotoStmt
    : On expr (Go To | GoTo) exprList
    ;

onGosubStmt
    : On expr (Go Sub | GoSub) exprList
    ;

// ============================================================
// Assignment / calls
// ============================================================

assignmentStmt
    : lvalue Equal expr
    ;

// Procedure call with leading channel argument e.g. PRINT #1, x
channelProcCallStmt
    : ID chanArg stmtTail?
    ;

// Ordinary procedure call — arglist is optional to allow bare calls like CLS, STOP
procedureCallStmt
    : ID stmtArglist?
    ;

lvalue
    : postfixName
    ;

lvalueList
    : lvalue (Comma lvalue)*
    ;

stmtArglist
    : parenthesizedlist
    | stmtTail
    ;

stmtTail
    : stmtSegment+
    ;

stmtSegment
    : separator stmtArg?
    | stmtArg
    ;

stmtArg
    : chanArg
    | rangedExpr
    ;

rangedExpr
    : expr (To expr)?
    ;

chanArg
    : Hash expr
    ;

arg
    : chanArg
    | expr
    ;

// ============================================================
// Identifier / postfix forms
// ============================================================

postfixName
    : ID postfixArg?
    ;

postfixArg
    : LeftParen postfixArgList? RightParen
    ;

postfixArgList
    : postfixArgItem (Comma postfixArgItem)*
    ;

postfixArgItem
    : expr (To expr)?
    ;

// ============================================================
// Procedure / Function definitions
// ============================================================

procedureDef
    : Define ProcedureKw ID formalParams Newline
      line*
      lineNumber? endDef ID?
    ;

functionDef
    : Define FunctionKw ID formalParams Newline
      line*
      lineNumber? endDef ID?
    ;

// Formal parameters are plain identifiers only — not full expressions
formalParams
    : (LeftParen ID (Comma ID)* RightParen)?
    ;

// ============================================================
// Compound end-markers as parser rules
// (avoids multi-word lexer tokens with embedded spaces)
// ============================================================

endDef
    : End Define
    ;

endFor
    : End For
    ;

endRepeat
    : End Repeat
    ;

endIf
    : End If
    ;

endSelect
    : End SelectKw
    ;

endWhen
    : End When
    ;

// ============================================================
// Control flow
// ============================================================

forStmt
    : For ID Equal expr To expr (Step expr)?
      (
          Colon Comment Newline forBody                            // long form + trailing remark
        | Colon stmtlist                                            // short form
        | Newline forBody                                          // long form
      )
    ;

forBody
    : forBodyLine*? forTerminator
    ;

forBodyLine
    : lineNumber? plainStmtlist? Newline
    | lineNumber Colon Newline
    ;

forTerminator
    : lineNumber? endFor ID?
      (Colon stmtlist)?
    | lineNumber? plainStmtlist Colon endFor ID?
      (Colon stmtlist)?
    | lineNumber? Next ID
      (Colon stmtlist)?
    | lineNumber? plainStmtlist Colon Next ID
      (Colon stmtlist)?
    ;

repeatStmt
    : Repeat ID
      (
          Colon stmtlist
        | Newline line* lineNumber? endRepeat ID?
      )
    ;

ifStmt
    : If expr
      (
          (Then | Colon) stmtlist (Colon Else Colon stmtlist)?      // short form
        | (Then)? Newline ifBlock elseBlock? lineNumber? endIf      // long form
      )
    ;

ifBlock
    : line+
    ;

elseBlock
    : lineNumber? Else Newline? line+
    ;

// SELect ON expr
//   ON expr = range : stmts
//   ...
// END SELect
selectStmt
    : SelectKw On expr Newline
      selectItem*
      lineNumber? endSelect
    ;

selectItem
    : onClause
    | line
    ;

onClause
    : lineNumber? On expr Equal rangeexpr (Colon stmtlist)? Newline
    ;

dimList
    : dimItem (Comma dimItem)*
    ;

dimItem
    : ID LeftParen exprList RightParen
    ;
    
// WHEN ERROR ... END WHEN
// WHEN expr  ... END WHEN
whenStmt
    : When ErrorKw Newline
      line*
      lineNumber? endWhen                                           #WhenErrorStmt
    | When expr Newline
      line*
      lineNumber? endWhen                                           #WhenCondStmt
    ;

// ============================================================
// Expressions
// ============================================================

exprList
    : expr (Comma expr)*
    ;

expr
    : orExpr
    ;

orExpr
    : andExpr ((Or | Xor) andExpr)*
    ;

andExpr
    : notExpr ((And | AmpAmp) notExpr)*
    ;

notExpr
    : Not notExpr
    | compareExpr
    ;

// Includes == case-sensitive equality
compareExpr
    : instrExpr ((Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual | CaseSensEq) instrExpr)*
    ;

// INSTR sits between comparison and concatenation in precedence
instrExpr
    : concatExpr (Instr concatExpr)?
    ;

concatExpr
    : addExpr (Amp addExpr)*
    ;

addExpr
    : mulExpr ((Plus | Minus) mulExpr)*
    ;

mulExpr
    : powExpr ((Multiply | Divide | Mod | Div) powExpr)*
    ;

powExpr
    : unaryExpr (Caret unaryExpr)*
    ;

// Unary minus/plus handled here — NOT in the Real lexer rule
unaryExpr
    : (Plus | Minus | Not) unaryExpr
    | primary
    ;

primary
    : Integer
    | Real
    | String
    | LeftParen expr RightParen
    | postfixName
    ;

// ============================================================
// Shared sub-rules
// ============================================================

parenthesizedlist
    : LeftParen expr (separator expr)* RightParen
    ;

unparenthesizedlist
    : expr (separator expr)*
    ;

localList
    : localItem (Comma localItem)*
    ;

localItem
    : ID (LeftParen exprList RightParen)?
    ;

separator
    : Comma
    | Bang
    | Semi
    | Backslash
    ;

// REMAINDER is the catch-all clause in SELect ON
rangeexpr
    : expr To expr
    | expr
    | Remainder
    ;

lineNumber
    : Integer
    ;

// ============================================================
// Lexer Rules
// ============================================================

// ---------- Single-word keywords ----------
// All compound end-markers are handled at the parser level
// so no multi-word tokens with embedded spaces appear here.

Define       : 'DEFine' ;
ProcedureKw  : 'PROCedure' ;
FunctionKw   : 'FuNction' ;
End          : 'END' ;
SelectKw     : 'SELect' ;
ErrorKw      : 'ERROR' ;

Refer        : 'REFERENCE' ;
Implic       : 'IMPLICIT%' | 'IMPLICIT$' ;
Local        : 'LOCal' ;
Dimension    : 'DIM' ;
If           : 'IF' ;
Else         : 'ELSE' ;
Then         : 'THEN' ;
On           : 'ON' ;
For          : 'FOR' ;
Next         : 'NEXT' ;
To           : 'TO' ;
Step         : 'STEP' ;
Repeat       : 'REPeat' ;
Exit         : 'EXIT' ;
Until        : 'UNTIL' ;
When         : 'WHEN' ;
Return       : 'RETurn' ;
Go           : 'GO' ;
GoTo         : 'GOTO' ;
Sub          : 'SUB' ;
GoSub        : 'GOSUB' ;
Data         : 'DATA' ;
Read         : 'READ' ;
Restore      : 'RESTore' ;
Remainder    : 'REMAINDER' ;

// ---------- Operators ----------
And          : 'AND' ;
Or           : 'OR' ;
Xor          : 'XOR' ;
Not          : 'NOT' ;
Mod          : 'MOD' ;
Div          : 'DIV' ;
Instr        : 'INSTR' ;

// ---------- Multi-char symbols (longest match first) ----------
CaseSensEq   : '==' ;
NotEqual     : '<>' ;
LessEqual    : '<=' ;
GreaterEqual : '>=' ;
Less         : '<' ;
Greater      : '>' ;
Equal        : '=' ;
Plus         : '+' ;
Minus        : '-' ;
Multiply     : '*' ;
Divide       : '/' ;
Caret        : '^' ;
Amp          : '&' ;
AmpAmp       : '&&' ;
Hash         : '#' ;
Colon        : ':' ;
Semi         : ';' ;
Comma        : ',' ;
Bang         : '!' ;
Backslash    : '\\' ;
Tilde        : '~' ;
Question     : '?' ;
Point        : '.' ;
LeftParen    : '(' ;
RightParen   : ')' ;
LeftBracket  : '[' ;
RightBracket : ']' ;

// ---------- Whitespace ----------
Whitespace   : [ \t]+ -> skip ;
Newline      : '\r'? '\n' ;

// LET is legal but meaningless — skip it entirely
Let          : 'LET' -> skip ;

// ---------- Comments ----------
// REMark consumes to end of line as a single token
Comment      : 'REMark' ~[\r\n]* ;

// ---------- Identifiers ----------
// Must appear after ALL keyword rules so keywords take lexer precedence
ID           : LETTER [0-9A-Za-z_]* [%$]? ;

// ---------- Literals ----------
Integer      : DIGIT+ ;

// No leading minus — unaryExpr handles negation
// DIGIT* allows forms like .5
Real         : DIGIT* Point DIGIT+ ([Ee] [+\-]? DIGIT+)?
             | DIGIT+ [Ee] [+\-]? DIGIT+
             ;

String       : '"' ~["\r\n]* '"'
             | '\'' ~['\r\n]* '\''
             ;

// ---------- Fragments ----------
fragment LETTER : [A-Za-z] ;
fragment DIGIT  : [0-9] ;
