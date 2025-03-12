using FSharpPlus.Control;
using static Runtime;
using static Runtime.InteropWrapper;


public static class TestPrograms
{
    public static void Main()
    {
        declareGlobal("a", new IntegerValue(3));
        var b = lookupVar("a");
        
        declareLocal("myString", new StringValue("hello"));
        var doubled = Wrapper.Call(CalledFunction);
        
    }

    public static string CalledFunction()
    {
//        pushFrame();
//        var y = lookupVar("myString");
//        var temp = y.Value.value as StringValue;
  //      popFrame();
  var temp = getString("myString");
  temp = "potato";
  putString("myString", temp);
  var v = getString("myString");
    return temp + temp;
    }

}
