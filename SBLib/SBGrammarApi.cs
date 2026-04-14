using Antlr4.Runtime;

public static class SBGrammarApi
{
    public static Lexer CreateLexer(ICharStream input, ITokenFactory factory)
    {
        return new SBLexer(input) { TokenFactory = factory };
    }

    public static string? GetTokenSymbolicName(int tokenType)
    {
        return SBLexer.DefaultVocabulary.GetSymbolicName(tokenType);
    }
}
