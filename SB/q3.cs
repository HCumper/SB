using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;

public static class SB
{
    private static readonly Dictionary<string, object?> v34_A = new();
    private static readonly Dictionary<string, object?> v35_B = new();
    private static object? v37_FINISHED;
    private static object? v39_LO;
    private static object? v40_N;
    private static object? v41_NOW;
    private static object? v42_PARTITIONS;
    private static object? v49_SHORTLIST;
    private static object? v50_SORTS;
    private static object? v52_T_;
    private static object? v53_TEMP;

    private static readonly object?[] __data = new object?[] {  };
    private static readonly Dictionary<int, int> __restorePoints = new Dictionary<int, int> {  };
    private static int __dataPointer;
    private static string[] __inputBuffer = Array.Empty<string>();

    private sealed class LoopControlException : Exception
    {
        public LoopControlException(int loopId, bool isNext)
        {
            LoopId = loopId;
            IsNext = isNext;
        }
    
        public int LoopId { get; }
        public bool IsNext { get; }
    }
    
    private static int AsInt(object? value)
    {
        return value switch
        {
            null => 0,
            int i => i,
            double d => (int)Math.Round(d),
            float f => (int)Math.Round(f),
            string s when int.TryParse(s, NumberStyles.Integer, CultureInfo.InvariantCulture, out var i) => i,
            string s when double.TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out var d) => (int)Math.Round(d),
            _ => 0
        };
    }
    
    private static double AsDouble(object? value)
    {
        return value switch
        {
            null => 0.0,
            int i => i,
            double d => d,
            float f => f,
            string s when double.TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out var d) => d,
            _ => 0.0
        };
    }
    
    private static string AsString(object? value) => value switch
    {
        null => string.Empty,
        string s => s,
        double d => d.ToString("G17", CultureInfo.InvariantCulture),
        float f => f.ToString("G9", CultureInfo.InvariantCulture),
        _ => Convert.ToString(value, CultureInfo.InvariantCulture) ?? string.Empty
    };
    
    private static bool IsTrue(object? value)
    {
        return value switch
        {
            null => false,
            string s => !string.IsNullOrEmpty(s),
            _ => Math.Abs(AsDouble(value)) > double.Epsilon
        };
    }
    
    private static string ArrayKey(params object?[] indexes) => string.Join(",", indexes.Select(AsInt));
    
    private static object? GetArrayValue(Dictionary<string, object?> array, params object?[] indexes)
    {
        return array.TryGetValue(ArrayKey(indexes), out var value) ? value : null;
    }
    
    private static void SetArrayValue(Dictionary<string, object?> array, object? value, params object?[] indexes)
    {
        array[ArrayKey(indexes)] = value;
    }
    
    private static object? Identity(object? value) => value;
    private static object? Negate(object? value) => -AsDouble(value);
    private static object? BitwiseNot(object? value) => ~AsInt(value);
    private static object? Add(object? left, object? right) => left is string || right is string ? AsString(left) + AsString(right) : AsDouble(left) + AsDouble(right);
    private static object? Subtract(object? left, object? right) => AsDouble(left) - AsDouble(right);
    private static object? Multiply(object? left, object? right) => AsDouble(left) * AsDouble(right);
    private static object? Divide(object? left, object? right) => AsDouble(left) / AsDouble(right);
    private static object? Power(object? left, object? right) => Math.Pow(AsDouble(left), AsDouble(right));
    private static object? Concat(object? left, object? right) => AsString(left) + AsString(right);
    private static object? IntegerDivide(object? left, object? right) => AsInt(left) / AsInt(right);
    private static object? Modulo(object? left, object? right) => AsInt(left) % AsInt(right);
    private static object? BitwiseAnd(object? left, object? right) => AsInt(left) & AsInt(right);
    private static object? BitwiseOr(object? left, object? right) => AsInt(left) | AsInt(right);
    private static object? BitwiseXor(object? left, object? right) => AsInt(left) ^ AsInt(right);
    private static int CompareEqual(object? left, object? right) => Equals(left is string || right is string ? AsString(left) : AsDouble(left), left is string || right is string ? AsString(right) : AsDouble(right)) ? 1 : 0;
    private static int CompareNotEqual(object? left, object? right) => CompareEqual(left, right) == 0 ? 1 : 0;
    private static int CompareLessThan(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) < 0 : AsDouble(left) < AsDouble(right)) ? 1 : 0;
    private static int CompareLessThanOrEqual(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) <= 0 : AsDouble(left) <= AsDouble(right)) ? 1 : 0;
    private static int CompareGreaterThan(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) > 0 : AsDouble(left) > AsDouble(right)) ? 1 : 0;
    private static int CompareGreaterThanOrEqual(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) >= 0 : AsDouble(left) >= AsDouble(right)) ? 1 : 0;
    private static int Instr(object? left, object? right) => AsString(left).IndexOf(AsString(right), StringComparison.Ordinal) + 1;
    private static object? SliceRange(object? left, object? right) => (AsInt(left), AsInt(right));
    private static object? ApplyUnary(string name, object? value) => throw new NotSupportedException($"Unary operator '{name}' is not supported by the generated backend yet.");
    private static object? ApplyBinary(string name, object? left, object? right) => throw new NotSupportedException($"Binary operator '{name}' is not supported by the generated backend yet.");
    
    private static object? InvokeBuiltInFunction(string name, params object?[] args)
    {
        switch (name.ToUpperInvariant())
        {
            case "ABS": return Math.Abs(AsDouble(args.ElementAtOrDefault(0)));
            case "INT": return (int)Math.Floor(AsDouble(args.ElementAtOrDefault(0)));
            case "ROUND": return (int)Math.Round(AsDouble(args.ElementAtOrDefault(0)));
            case "STR$": return AsString(args.ElementAtOrDefault(0));
            case "VAL": return AsDouble(args.ElementAtOrDefault(0));
            case "LEFT$":
            {
                var source = AsString(args.ElementAtOrDefault(0));
                var length = Math.Max(0, Math.Min(source.Length, AsInt(args.ElementAtOrDefault(1))));
                return source.Substring(0, length);
            }
            case "RIGHT$":
            {
                var source = AsString(args.ElementAtOrDefault(0));
                var length = Math.Max(0, Math.Min(source.Length, AsInt(args.ElementAtOrDefault(1))));
                return source.Substring(source.Length - length, length);
            }
            case "DATE": return (int)DateTimeOffset.UtcNow.ToUnixTimeSeconds();
            default: throw new NotSupportedException($"Built-in function '{name}' is not supported by the generated C# backend yet.");
        }
    }
    
    private static void ExecuteBuiltInStatement(string name, object? channel, params object?[] args)
    {
        switch (name.ToUpperInvariant())
        {
            case "PRINT":
                Console.WriteLine(string.Join(" ", args.Select(AsString)));
                break;
            case "REFERENCE":
                break;
            case "STOP":
                throw new OperationCanceledException("STOP encountered.");
            default:
                if (name.StartsWith("TURBO", StringComparison.OrdinalIgnoreCase))
                {
                    return;
                }
                throw new NotSupportedException($"Built-in statement '{name}' is not supported by the generated backend yet.");
        }
    }
    
    private static void ExecuteInput(object? channel, object?[] prompts)
    {
        if (prompts.Length > 0)
        {
            Console.Write(string.Join(" ", prompts.Select(AsString)));
        }
        var line = Console.ReadLine() ?? string.Empty;
        __inputBuffer = line.Split(',').Select(part => part.Trim()).ToArray();
    }
    
    private static object? ReadInputValue(int index, string targetType)
    {
        var raw = index < __inputBuffer.Length ? __inputBuffer[index] : string.Empty;
        return targetType switch
        {
            "string" => raw,
            "float" when double.TryParse(raw, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out var d) => d,
            "float" => 0.0,
            _ when int.TryParse(raw, NumberStyles.Integer, CultureInfo.InvariantCulture, out var i) => i,
            _ => 0
        };
    }
    
    private static object? ReadDataValue(string targetType)
    {
        if (__dataPointer >= __data.Length)
        {
            throw new InvalidOperationException("READ moved past the end of DATA.");
        }
        var value = __data[__dataPointer++];
        return targetType switch
        {
            "string" => AsString(value),
            "float" => AsDouble(value),
            _ => AsInt(value)
        };
    }
    
    private static void RestoreToLine(int line)
    {
        if (!__restorePoints.TryGetValue(line, out var slot))
        {
            throw new InvalidOperationException($"RESTORE line {line} does not exist.");
        }
        __dataPointer = slot;
    }

    private static object? r36_check()
    {
        object? v17_LOOP = null;
        line_140: ;
        line_150: ;
        line_350: ;
        ExecuteBuiltInStatement("PRINT", null, "Time", Subtract(InvokeBuiltInFunction("DATE"), v41_NOW));
        line_360: ;
        {
            var __loop0Step = AsInt(1);
            var __loop0End = AsInt(Subtract(v40_N, 1));
            for (var __loop0Index = AsInt(1); __loop0Step >= 0 ? __loop0Index <= __loop0End : __loop0Index >= __loop0End; __loop0Index += __loop0Step)
            {
                v17_LOOP = __loop0Index;
                try
                {
                    line_365: ;
                    ExecuteBuiltInStatement("PRINT", null, GetArrayValue(v35_B, v17_LOOP));
                    line_370: ;
                    if (IsTrue(CompareGreaterThan(GetArrayValue(v35_B, v17_LOOP), GetArrayValue(v35_B, Add(v17_LOOP, 1)))))
                    {
                        ExecuteBuiltInStatement("PRINT", null, "Error at", v17_LOOP);
                    }
                }
                catch (LoopControlException ex)
                {
                    if (ex.LoopId != 0)
                    {
                        throw;
                    }
                    if (!ex.IsNext)
                    {
                        break;
                    }
                }
            }
        }
        return null;
    }
    private static object? r48_set_up_tabLe()
    {
        object? v30_ENTRY = null;
        line_410: ;
        line_420: ;
        ExecuteInput(null, new object?[] { "How many numbers? " });
        v40_N = ReadInputValue(0, "void");
        line_430: ;
        {
            var __loop0Step = AsInt(1);
            var __loop0End = AsInt(v40_N);
            for (var __loop0Index = AsInt(1); __loop0Step >= 0 ? __loop0Index <= __loop0End : __loop0Index >= __loop0End; __loop0Index += __loop0Step)
            {
                v30_ENTRY = __loop0Index;
                try
                {
                    line_440: ;
                    SetArrayValue(v34_A, InvokeBuiltInFunction("RND", SliceRange(1, 10000)), v30_ENTRY);
                }
                catch (LoopControlException ex)
                {
                    if (ex.LoopId != 0)
                    {
                        throw;
                    }
                    if (!ex.IsNext)
                    {
                        break;
                    }
                }
            }
        }
        line_460: ;
        v41_NOW = InvokeBuiltInFunction("DATE");
        return null;
    }
    private static object? r43_print_table()
    {
        object? v23_POS = null;
        line_500: ;
        line_510: ;
        v37_FINISHED = Subtract(InvokeBuiltInFunction("DATE"), v41_NOW);
        line_520: ;
        ExecuteBuiltInStatement("PRINT", null);
        ExecuteBuiltInStatement("PRINT", null);
        line_530: ;
        ExecuteBuiltInStatement("BEEP", null, 10000, 100);
        line_540: ;
        {
            var __loop0Step = AsInt(1);
            var __loop0End = AsInt(v40_N);
            for (var __loop0Index = AsInt(Subtract(v40_N, 4)); __loop0Step >= 0 ? __loop0Index <= __loop0End : __loop0Index >= __loop0End; __loop0Index += __loop0Step)
            {
                v23_POS = __loop0Index;
                try
                {
                    line_550: ;
                    ExecuteBuiltInStatement("PRINT", null, GetArrayValue(v34_A, v23_POS));
                }
                catch (LoopControlException ex)
                {
                    if (ex.LoopId != 0)
                    {
                        throw;
                    }
                    if (!ex.IsNext)
                    {
                        break;
                    }
                }
            }
        }
        line_570: ;
        ExecuteBuiltInStatement("PRINT", null, "Time taken", v37_FINISHED);
        return null;
    }
    private static object? r45_quicksort(object? v27_L, object? v29_R)
    {
        object? v24_EXCHANGE = null;
        object? v25_I = null;
        object? v26_J = null;
        object? v28_PIVOT = null;
        line_620: ;
        line_630: ;
        v25_I = v27_L;
        v26_J = v29_R;
        line_640: ;
        // REMark IF b(i)<b(i+1) : swap b(i),b(i+1)
        line_650: ;
        // REMark IF b(i+1)>b(j) : swap b(i+1),b(j)
        line_660: ;
        // REMark IF b(i)>b(i+1) : swap b(i),b(i+1)
        line_670: ;
        v28_PIVOT = GetArrayValue(v35_B, v25_I);
        line_680: ;
        while (true)
        {
            try
            {
                line_690: ;
                while (true)
                {
                    try
                    {
                        line_700: ;
                        if (IsTrue(CompareLessThanOrEqual(v28_PIVOT, GetArrayValue(v35_B, v25_I))))
                        {
                            throw new LoopControlException(1, false);
                        }
                        line_710: ;
                        v25_I = Add(v25_I, 1);
                    }
                    catch (LoopControlException ex)
                    {
                        if (ex.LoopId != 1)
                        {
                            throw;
                        }
                        if (!ex.IsNext)
                        {
                            break;
                        }
                    }
                }
                line_730: ;
                while (true)
                {
                    try
                    {
                        line_740: ;
                        if (IsTrue(CompareGreaterThanOrEqual(v28_PIVOT, GetArrayValue(v35_B, v26_J))))
                        {
                            throw new LoopControlException(2, false);
                        }
                        line_750: ;
                        v26_J = Subtract(v26_J, 1);
                    }
                    catch (LoopControlException ex)
                    {
                        if (ex.LoopId != 2)
                        {
                            throw;
                        }
                        if (!ex.IsNext)
                        {
                            break;
                        }
                    }
                }
                line_770: ;
                if (IsTrue(CompareLessThan(v25_I, v26_J)))
                {
                    v24_EXCHANGE = GetArrayValue(v35_B, v25_I);
                    SetArrayValue(v35_B, GetArrayValue(v35_B, v26_J), v25_I);
                    SetArrayValue(v35_B, v24_EXCHANGE, v26_J);
                }
                line_780: ;
                if (IsTrue(CompareLessThanOrEqual(v25_I, v26_J)))
                {
                    v25_I = Add(v25_I, 1);
                    v26_J = Subtract(v26_J, 1);
                }
                line_790: ;
                if (IsTrue(CompareGreaterThan(v25_I, v26_J)))
                {
                    throw new LoopControlException(0, false);
                }
            }
            catch (LoopControlException ex)
            {
                if (ex.LoopId != 0)
                {
                    throw;
                }
                if (!ex.IsNext)
                {
                    break;
                }
            }
        }
        line_810: ;
        if (IsTrue(CompareLessThan(v27_L, v26_J)))
        {
            line_820: ;
            if (IsTrue(CompareGreaterThanOrEqual(Subtract(v26_J, v27_L), v49_SHORTLIST)))
            {
                line_830: ;
                r45_quicksort(v27_L, v26_J);
            }
            else
            {
                line_850: ;
                r38_insertionsort(v27_L, v26_J);
            }
        }
        line_880: ;
        if (IsTrue(CompareLessThan(v25_I, v29_R)))
        {
            line_890: ;
            if (IsTrue(CompareGreaterThanOrEqual(Subtract(v29_R, 1), v49_SHORTLIST)))
            {
                line_900: ;
                r45_quicksort(v25_I, v29_R);
            }
            else
            {
                line_920: ;
                r38_insertionsort(v25_I, v29_R);
            }
        }
        return null;
    }
    private static object? r51_swap(object? v33_THIS, object? v32_THAT)
    {
        object? v31_T = null;
        line_990: ;
        line_1000: ;
        v31_T = v33_THIS;
        line_1010: ;
        v33_THIS = v32_THAT;
        line_1020: ;
        v32_THAT = v31_T;
        return null;
    }
    private static object? r38_insertionsort(object? v18_FIRST, object? v20_LAST)
    {
        object? v19_ITEM = null;
        object? v21_P = null;
        object? v22_TEMPORARY = null;
        line_1070: ;
        line_1080: ;
        {
            var __loop0Step = AsInt(1);
            var __loop0End = AsInt(v20_LAST);
            for (var __loop0Index = AsInt(Add(v18_FIRST, 1)); __loop0Step >= 0 ? __loop0Index <= __loop0End : __loop0Index >= __loop0End; __loop0Index += __loop0Step)
            {
                v19_ITEM = __loop0Index;
                try
                {
                    line_1090: ;
                    v21_P = v19_ITEM;
                    line_1100: ;
                    v22_TEMPORARY = GetArrayValue(v35_B, v21_P);
                    line_1110: ;
                    while (true)
                    {
                        try
                        {
                            line_1120: ;
                            if (IsTrue(CompareGreaterThanOrEqual(v22_TEMPORARY, GetArrayValue(v35_B, Subtract(v21_P, 1)))))
                            {
                                throw new LoopControlException(1, false);
                            }
                            line_1130: ;
                            if (IsTrue(CompareEqual(v21_P, v18_FIRST)))
                            {
                                throw new LoopControlException(1, false);
                            }
                            line_1140: ;
                            SetArrayValue(v35_B, GetArrayValue(v35_B, Subtract(v21_P, 1)), v21_P);
                            line_1150: ;
                            v21_P = Subtract(v21_P, 1);
                        }
                        catch (LoopControlException ex)
                        {
                            if (ex.LoopId != 1)
                            {
                                throw;
                            }
                            if (!ex.IsNext)
                            {
                                break;
                            }
                        }
                    }
                    line_1170: ;
                    SetArrayValue(v35_B, v22_TEMPORARY, v21_P);
                }
                catch (LoopControlException ex)
                {
                    if (ex.LoopId != 0)
                    {
                        throw;
                    }
                    if (!ex.IsNext)
                    {
                        break;
                    }
                }
            }
        }
        return null;
    }
    private static object? r46_QUICKSORT1(object? v8_BOTTOM, object? v13_TOP)
    {
        object? v9_HIGH = null;
        object? v10_LOW = null;
        object? v11_PTR = null;
        object? v12_SORT_LOOP = null;
        line_1220: ;
        line_1230: ;
        v10_LOW = v8_BOTTOM;
        v9_HIGH = v13_TOP;
        v11_PTR = v8_BOTTOM;
        line_1240: ;
        while (true)
        {
            try
            {
                line_1250: ;
                if (IsTrue(CompareGreaterThanOrEqual(v10_LOW, v9_HIGH)))
                {
                    throw new LoopControlException(0, false);
                }
                line_1260: ;
                if (IsTrue(CompareGreaterThan(GetArrayValue(v35_B, v10_LOW), GetArrayValue(v35_B, v9_HIGH))))
                {
                    line_1270: ;
                    v53_TEMP = GetArrayValue(v35_B, v10_LOW);
                    line_1280: ;
                    SetArrayValue(v35_B, GetArrayValue(v35_B, v9_HIGH), v10_LOW);
                    line_1290: ;
                    SetArrayValue(v35_B, v53_TEMP, v9_HIGH);
                    line_1300: ;
                    if (IsTrue(CompareEqual(v11_PTR, v10_LOW)))
                    {
                        line_1310: ;
                        v10_LOW = Add(v10_LOW, 1);
                        v11_PTR = v9_HIGH;
                    }
                    else
                    {
                        line_1330: ;
                        v9_HIGH = Subtract(v9_HIGH, 1);
                        v11_PTR = v10_LOW;
                    }
                }
                else
                {
                    line_1360: ;
                    if (IsTrue(CompareEqual(v11_PTR, v10_LOW)))
                    {
                        v9_HIGH = Subtract(v9_HIGH, 1);
                    }
                    else
                    {
                        v10_LOW = Add(v10_LOW, 1);
                    }
                }
            }
            catch (LoopControlException ex)
            {
                if (ex.LoopId != 0)
                {
                    throw;
                }
                if (!ex.IsNext)
                {
                    break;
                }
            }
        }
        line_1390: ;
        if (IsTrue(CompareLessThan(InvokeBuiltInFunction("ABS", Subtract(v13_TOP, v8_BOTTOM)), 2)))
        {
            return null;
        }
        line_1400: ;
        r46_QUICKSORT1(v8_BOTTOM, Subtract(v11_PTR, 1));
        line_1410: ;
        r46_QUICKSORT1(Add(v11_PTR, 1), v13_TOP);
        return null;
    }
    private static object? r47_QUICKSORT2(object? v14_LOWER, object? v16_UPPER)
    {
        object? v15_PART_ELEM = null;
        line_1500: ;
        line_1510: ;
        v50_SORTS = Add(v50_SORTS, 1);
        line_1520: ;
        if (IsTrue(CompareLessThan(v14_LOWER, v16_UPPER)))
        {
            line_1530: ;
            v15_PART_ELEM = Add(v16_UPPER, 1);
            line_1540: ;
            r44_QS2_PARTITION(v14_LOWER, v15_PART_ELEM);
            line_1550: ;
            r47_QUICKSORT2(v14_LOWER, Subtract(v15_PART_ELEM, 1));
            line_1560: ;
            r47_QUICKSORT2(Add(v15_PART_ELEM, 1), v16_UPPER);
        }
        return null;
    }
    private static object? r44_QS2_PARTITION(object? v2_I, object? v4_J)
    {
        object? v0_DEC_J = null;
        object? v1_EXCHANGE = null;
        object? v3_INC_K = null;
        object? v5_K = null;
        object? v6_LOOP = null;
        object? v7_TEMP = null;
        line_1610: ;
        line_1620: ;
        v7_TEMP = GetArrayValue(v35_B, Divide(Add(v2_I, v4_J), 2));
        v5_K = v2_I;
        line_1630: ;
        v42_PARTITIONS = Add(v42_PARTITIONS, 1);
        line_1640: ;
        while (true)
        {
            try
            {
                line_1650: ;
                v42_PARTITIONS = Add(v42_PARTITIONS, 1);
                line_1660: ;
                while (true)
                {
                    try
                    {
                        line_1670: ;
                        v5_K = Add(v5_K, 1);
                        line_1680: ;
                        // REMark this is where the 'guaranteed highest value' comes in
                        line_1690: ;
                        if (IsTrue(CompareGreaterThanOrEqual(GetArrayValue(v35_B, v5_K), v7_TEMP)))
                        {
                            throw new LoopControlException(1, false);
                        }
                    }
                    catch (LoopControlException ex)
                    {
                        if (ex.LoopId != 1)
                        {
                            throw;
                        }
                        if (!ex.IsNext)
                        {
                            break;
                        }
                    }
                }
                line_1710: ;
                while (true)
                {
                    try
                    {
                        line_1720: ;
                        v4_J = Subtract(v4_J, 1);
                        line_1730: ;
                        // REMark conversely we don't need a 'guaranteed less than' value
                        line_1740: ;
                        // REMark at the start of the array as the sorting process does
                        line_1750: ;
                        // REMark this for us anyway
                        line_1760: ;
                        if (IsTrue(CompareLessThanOrEqual(GetArrayValue(v35_B, v4_J), v7_TEMP)))
                        {
                            throw new LoopControlException(2, false);
                        }
                    }
                    catch (LoopControlException ex)
                    {
                        if (ex.LoopId != 2)
                        {
                            throw;
                        }
                        if (!ex.IsNext)
                        {
                            break;
                        }
                    }
                }
                line_1780: ;
                if (IsTrue(CompareLessThan(v5_K, v4_J)))
                {
                    line_1790: ;
                    // REMark swap the two entries around
                    line_1800: ;
                    v1_EXCHANGE = GetArrayValue(v35_B, v5_K);
                    SetArrayValue(v35_B, GetArrayValue(v35_B, v4_J), v5_K);
                    SetArrayValue(v35_B, v1_EXCHANGE, v4_J);
                }
                else
                {
                    line_1820: ;
                    throw new LoopControlException(0, false);
                }
            }
            catch (LoopControlException ex)
            {
                if (ex.LoopId != 0)
                {
                    throw;
                }
                if (!ex.IsNext)
                {
                    break;
                }
            }
        }
        line_1850: ;
        SetArrayValue(v35_B, GetArrayValue(v35_B, v4_J), v2_I);
        SetArrayValue(v35_B, v7_TEMP, v4_J);
        return null;
    }

    public static void Main()
    {
        line_100: ;
        // REMark CLS
        line_110: ;
        ExecuteBuiltInStatement("TURBO_REPFIL", null);
        line_120: ;
        v50_SORTS = 0;
        v42_PARTITIONS = 0;
        line_130: ;
        line_140: ;
        line_150: ;
        line_160: ;
        v49_SHORTLIST = 9;
        line_170: ;
        r48_set_up_tabLe();
        line_180: ;
        {
            var __loop0Step = AsInt(1);
            var __loop0End = AsInt(v40_N);
            for (var __loop0Index = AsInt(1); __loop0Step >= 0 ? __loop0Index <= __loop0End : __loop0Index >= __loop0End; __loop0Index += __loop0Step)
            {
                v39_LO = __loop0Index;
                try
                {
                    SetArrayValue(v35_B, GetArrayValue(v34_A, v39_LO), v39_LO);
                }
                catch (LoopControlException ex)
                {
                    if (ex.LoopId != 0)
                    {
                        throw;
                    }
                    if (!ex.IsNext)
                    {
                        break;
                    }
                }
            }
        }
        line_190: ;
        v41_NOW = InvokeBuiltInFunction("DATE");
        line_200: ;
        SetArrayValue(v35_B, 10001, Add(v40_N, 1));
        line_210: ;
        r45_quicksort(1, v40_N);
        line_220: ;
        r36_check();
        line_230: ;
        ExecuteInput(null, new object?[] {  });
        v52_T_ = ReadInputValue(0, "string");
        line_240: ;
        ExecuteBuiltInStatement("STOP", null);
        line_250: ;
        {
            var __loop1Step = AsInt(1);
            var __loop1End = AsInt(v40_N);
            for (var __loop1Index = AsInt(1); __loop1Step >= 0 ? __loop1Index <= __loop1End : __loop1Index >= __loop1End; __loop1Index += __loop1Step)
            {
                v39_LO = __loop1Index;
                try
                {
                    SetArrayValue(v35_B, GetArrayValue(v34_A, v39_LO), v39_LO);
                }
                catch (LoopControlException ex)
                {
                    if (ex.LoopId != 1)
                    {
                        throw;
                    }
                    if (!ex.IsNext)
                    {
                        break;
                    }
                }
            }
        }
        line_260: ;
        // REMark b(n+1)=10001
        line_270: ;
        v50_SORTS = 0;
        v42_PARTITIONS = 0;
        line_280: ;
        v41_NOW = InvokeBuiltInFunction("DATE");
        line_290: ;
        r47_QUICKSORT2(1, v40_N);
        line_300: ;
        ExecuteBuiltInStatement("PRINT", null, v50_SORTS, v42_PARTITIONS);
        line_310: ;
        r36_check();
        line_320: ;
        ExecuteBuiltInStatement("STOP", null);
        line_330: ;
        line_340: ;
        line_400: ;
        line_480: ;
        line_490: ;
        line_590: ;
        line_600: ;
        line_610: ;
        line_960: ;
        line_970: ;
        line_980: ;
        line_1040: ;
        line_1050: ;
        line_1060: ;
        line_1200: ;
        line_1210: ;
        line_1430: ;
        line_1440: ;
        // REMark Quicksort, faster variety, by Marcus Jeffery
        line_1450: ;
        line_1460: ;
        // REMark prepare an array to sort, remembering that this routine needs
        line_1470: ;
        // REMark a predefined 'highest value entry' at the end of the array
        line_1480: ;
        // REMark QUICKSORT2 also needs the procedure QS2_PARTITION below
        line_1490: ;
        line_1590: ;
        line_1600: ;
    }
}
