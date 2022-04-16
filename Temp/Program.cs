using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using SBLib;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SB
{
    class Program
    {
        static void Main(string[] args)
        {
            var filename = @"C:\Users\hcump\Source\Repos\SB\SBLib\q3.sb";

            var reader = File.OpenText(filename);

            ICharStream cs = new AntlrInputStream(reader);
            var factory = new SBTokenFactory();
            var lexer = new SBLib.SBLexer(cs);
            {
                lexer.TokenFactory = factory;
            };
            var tokens = new CommonTokenStream(lexer);
            var parser = new SBParser(tokens);
            //parser.TokenFactory = factory;
            IParseTree tree = parser.program();

//            Debug.WriteLine(tree.ToStringTree(parser));

        }
    }
}
