module Templates

let assignment left right = $@"{left} = {right};)"

let procFunc returnType routineName parameters =
    $@"{returnType} {routineName} {parameters}    
    {{    
    "


(*
declarationsTemplate(scope, reals, realArrays, integers, integerArrays, strings, stringArrays) ::= <<
<if (reals)>
    <scope> float <reals:{it|<it.Value.Name>};separator=", ">;
<endif>
<if (realArrays)>
    <realArrays:{it|<scope> float[] <it.Value.Name> = new float[<it.Value.dimensions:{ot|<ot>};separator=", ">];
    }>
<endif>
<if (integers)>
    <scope> int <integers:{ut|<ut.Value.Name>};separator=", ">;
    <endif>
<if (integerArrays)>
    <scope> <integerArrays:{it|float[] <it.Value.Name> = new int[<it.Value.dimensions:{ot|<ot>};separator=", ">];
    }>
<endif>
<if (strings)>
    <scope> string <strings:{ut|<ut.Value.Name>};separator=", ">;
    <endif>
<if (stringArrays)>
    <scope> <stringArrays:{it|float[] <it.Value.Name> = new string[<it.Value.dimensions:{ot|<ot>};separator=", ">];
    }>
<endif>
>>

forTemplate(id, expr1, expr2, increment, body) ::= <<for (<id> = <expr1>; <id> \<= <expr2>; <id> <increment>)
{
<body>
};
>>

identifierOnlyTemplate(id) ::= "<id>();"

printTemplate(params) ::= <<Console.Writeline( (<params:{it|<it>};separator=" + ">)>>
>>

programTemplate(programName, when, declarationsTemplate, programContent) ::= <<
// Super Basic To C# Port of <programName> on <when>

public class SB
{
<declarationsTemplate>

    public static void main()
    {
        <programContent>
    }
}
>>

statementListTemplate(statements) ::= <<<statements:{it|<it>};separator="
"> >>
*)
