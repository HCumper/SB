using System;
using Antlr4.Runtime;

namespace SB
{
    public class SBToken : CommonToken
    {
        public SBToken(int type, string text) : base(type, text)
        {
        }

        public SBToken(Tuple<ITokenSource, ICharStream> source, int type, int channel, int start,
            int stop)
            : base(source, type, channel, start, stop)
        {
        }

        public int EvaluatedType { get; set; } // SBLexer type
    }
}