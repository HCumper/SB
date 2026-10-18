using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using SBRuntime;

namespace SBGeneratedRuntime;

public sealed class LoopControlException : Exception
{
    public LoopControlException(int loopId, bool isNext)
    {
        LoopId = loopId;
        IsNext = isNext;
    }

    public int LoopId { get; }
    public bool IsNext { get; }
}

public sealed class RetryControlException : Exception
{
    public RetryControlException(int? lineNumber)
    {
        LineNumber = lineNumber;
    }

    public int? LineNumber { get; }
}

public sealed class ContinueControlException : Exception
{
    public ContinueControlException(int? lineNumber)
    {
        LineNumber = lineNumber;
    }

    public int? LineNumber { get; }
}

public enum ErrorActionKind
{
    Retry,
    Continue,
    Stop
}

public readonly struct ErrorAction
{
    private ErrorAction(ErrorActionKind kind, int? lineNumber)
    {
        Kind = kind;
        LineNumber = lineNumber;
    }

    public ErrorActionKind Kind { get; }
    public int? LineNumber { get; }

    public static ErrorAction Retry(int? lineNumber) => new ErrorAction(ErrorActionKind.Retry, lineNumber);
    public static ErrorAction Continue(int? lineNumber) => new ErrorAction(ErrorActionKind.Continue, lineNumber);
    public static ErrorAction Stop() => new ErrorAction(ErrorActionKind.Stop, null);
}

public sealed class Cell
{
    public Cell(object? value = null)
    {
        Value = value;
    }

    public object? Value { get; set; }
}

public static class GeneratedRuntime
{
    private sealed class DynamicFrameScope : IDisposable
    {
        private readonly Dictionary<string, Cell> _bindings;

        public DynamicFrameScope(Dictionary<string, Cell> bindings)
        {
            _bindings = bindings;
            __dynamicFrames.Add(bindings);
        }

        public void Dispose()
        {
            if (__dynamicFrames.Count > 0 && ReferenceEquals(__dynamicFrames[^1], _bindings))
            {
                __dynamicFrames.RemoveAt(__dynamicFrames.Count - 1);
            }
        }
    }

    private static readonly DateTime QlEpoch = new DateTime(1961, 1, 1, 0, 0, 0, DateTimeKind.Utc);
    private static readonly Dictionary<int, byte> __memory = new Dictionary<int, byte>();
    private static readonly Dictionary<int, StreamReader> __channelReaders = new Dictionary<int, StreamReader>();
    private static readonly Dictionary<int, StreamWriter> __channelWriters = new Dictionary<int, StreamWriter>();
    private static readonly List<Dictionary<string, Cell>> __dynamicFrames = new List<Dictionary<string, Cell>>();
    private static readonly Dictionary<string, Cell> __globalCells = new Dictionary<string, Cell>(StringComparer.OrdinalIgnoreCase);
    private static readonly Dictionary<Cell, int[]> __arrayDimensions = new Dictionary<Cell, int[]>();
    private static Random __random = new Random();
    private static int __screenMode = 4;
    private static bool __beeping;
    private static int __lastErrorNumber;
    private static string __lastErrorName = string.Empty;
    private static string __lastErrorDescription = string.Empty;
    private static int? __lastErrorLine;
    private static int? __lastRetryLine;
    private static int? __lastContinueLine;
    private static readonly Dictionary<int, (int Left, int Top)> __screenWindowOrigins = new Dictionary<int, (int Left, int Top)>();
    private static readonly Dictionary<int, (int X, int Y)> __screenCursors = new Dictionary<int, (int X, int Y)>();
    private static readonly Dictionary<int, (int X, int Y)> __graphicsCursors = new Dictionary<int, (int X, int Y)>();
    private static readonly Dictionary<int, HashSet<string>> __graphicsPixels = new Dictionary<int, HashSet<string>>();
    private static readonly Dictionary<int, int> __graphicsFillModes = new Dictionary<int, int>();
    private static readonly Dictionary<int, (double X, double Y, double Z)> __graphicsScales = new Dictionary<int, (double X, double Y, double Z)>();
    private static readonly Dictionary<int, int> __graphicsOverModes = new Dictionary<int, int>();
    private static readonly Dictionary<int, int> __graphicsUnderModes = new Dictionary<int, int>();
    private static readonly Dictionary<int, int> __graphicsFlashModes = new Dictionary<int, int>();
    private static readonly Dictionary<int, bool> __graphicsPenDownModes = new Dictionary<int, bool>();
    private static readonly Dictionary<int, double> __graphicsHeadings = new Dictionary<int, double>();
    private static readonly Dictionary<int, (int Width, int Height)> __screenCharacterSizes = new Dictionary<int, (int Width, int Height)>();
    private static readonly Dictionary<int, (int Font1, int Font2)> __screenCharacterFonts = new Dictionary<int, (int Font1, int Font2)>();
    private static readonly Dictionary<int, int> __screenScrollValues = new Dictionary<int, int>();
    private static readonly Dictionary<int, int?> __screenWidthValues = new Dictionary<int, int?>();
    private static readonly Dictionary<int, int> __screenPanValues = new Dictionary<int, int>();
    private static readonly Dictionary<int, int[]?> __screenRecolorValues = new Dictionary<int, int[]?>();
    private static readonly Dictionary<int, int[]?> __screenPaletteValues = new Dictionary<int, int[]?>();
    private static readonly Dictionary<int, int[]> __screenInkColors = new Dictionary<int, int[]>();
    private static readonly Dictionary<int, int> __screenPaperColors = new Dictionary<int, int>();
    private static readonly Dictionary<int, int[]> __screenStripColors = new Dictionary<int, int[]>();
    private static readonly Dictionary<int, (int Size, int? Color)> __screenBorderValues = new Dictionary<int, (int Size, int? Color)>();
    private static object?[] __data = Array.Empty<object?>();
    private static Dictionary<int, int> __restorePoints = new Dictionary<int, int>();
    private static int __dataPointer;
    private static string[] __inputBuffer = Array.Empty<string>();
    private static long __clockOffsetSeconds;

    public static void InitializeProgramState(object?[] data, Dictionary<int, int> restorePoints)
    {
        foreach (var reader in __channelReaders.Values)
        {
            reader.Dispose();
        }

        foreach (var writer in __channelWriters.Values)
        {
            writer.Dispose();
        }

        __channelReaders.Clear();
        __channelWriters.Clear();
        __memory.Clear();
        __dynamicFrames.Clear();
        __globalCells.Clear();
        __random = new Random();
        __screenMode = 4;
        __beeping = false;
        __lastErrorNumber = 0;
        __lastErrorName = string.Empty;
        __lastErrorDescription = string.Empty;
        __lastErrorLine = null;
        __lastRetryLine = null;
        __lastContinueLine = null;
        __screenWindowOrigins.Clear();
        __screenCursors.Clear();
        __graphicsCursors.Clear();
        __graphicsPixels.Clear();
        __graphicsFillModes.Clear();
        __graphicsScales.Clear();
        __graphicsOverModes.Clear();
        __graphicsUnderModes.Clear();
        __graphicsFlashModes.Clear();
        __graphicsPenDownModes.Clear();
        __graphicsHeadings.Clear();
        __screenCharacterSizes.Clear();
        __screenCharacterFonts.Clear();
        __screenScrollValues.Clear();
        __screenWidthValues.Clear();
        __screenPanValues.Clear();
        __screenRecolorValues.Clear();
        __screenPaletteValues.Clear();
        __screenInkColors.Clear();
        __screenPaperColors.Clear();
        __screenStripColors.Clear();
        __screenBorderValues.Clear();
        __data = data ?? Array.Empty<object?>();
        __restorePoints = restorePoints ?? new Dictionary<int, int>();
        __dataPointer = 0;
        __inputBuffer = Array.Empty<string>();
        __clockOffsetSeconds = 0L;
    }

    public static int AsInt(object? value)
    {
        return value switch
        {
            null => 0,
            int i => i,
            double d => (int)Math.Round(d),
            float f => (int)Math.Round(f),
            string s when int.TryParse(s, NumberStyles.Integer, CultureInfo.InvariantCulture, out var i) => i,
            string s when double.TryParse(s, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out var d) => (int)Math.Round(d),
            ValueTuple<int, int> tuple => tuple.Item2,
            _ => 0
        };
    }

    public static double AsDouble(object? value)
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

    public static string AsString(object? value) => value switch
    {
        null => string.Empty,
        string s => s,
        double d => d.ToString("G17", CultureInfo.InvariantCulture),
        float f => f.ToString("G9", CultureInfo.InvariantCulture),
        ValueTuple<int, int> tuple => $"{tuple.Item1} TO {tuple.Item2}",
        _ => Convert.ToString(value, CultureInfo.InvariantCulture) ?? string.Empty
    };

    public static bool IsTrue(object? value)
    {
        return value switch
        {
            null => false,
            string s => !string.IsNullOrEmpty(s),
            _ => Math.Abs(AsDouble(value)) > double.Epsilon
        };
    }

    public static bool IsControlFlowException(Exception ex)
    {
        return ex is LoopControlException
            || ex is RetryControlException
            || ex is ContinueControlException
            || ex is OperationCanceledException;
    }

    public static string ArrayKey(params object?[] indexes) => string.Join(",", indexes.Select(AsInt));

    public static void RecordLastError(Exception ex, int? lineNumber, int? retryLineNumber, int? continueLineNumber)
    {
        var error = ex switch
        {
            NotSupportedException => (-19, "ERR_NI", "Not implemented"),
            InvalidOperationException invalid when invalid.Message.Contains("Memory address", StringComparison.OrdinalIgnoreCase) => (-15, "ERR_BP", "Bad parameter"),
            InvalidOperationException invalid when invalid.Message.Contains("READ moved past the end of DATA", StringComparison.OrdinalIgnoreCase) => (-21, "ERR_BL", "Bad line of Basic"),
            InvalidOperationException invalid when invalid.Message.Contains("RESTORE line", StringComparison.OrdinalIgnoreCase) => (-21, "ERR_BL", "Bad line of Basic"),
            InvalidOperationException invalid when invalid.Message.Contains("Dynamic storage", StringComparison.OrdinalIgnoreCase) => (-15, "ERR_BP", "Bad parameter"),
            IOException => (-16, "ERR_FE", "File error"),
            _ => (-19, "ERR_NI", "Not implemented")
        };

        __lastErrorNumber = error.Item1;
        __lastErrorName = error.Item2;
        __lastErrorDescription = error.Item3;
        __lastErrorLine = lineNumber;
        __lastRetryLine = retryLineNumber;
        __lastContinueLine = continueLineNumber;
    }

    public static string FormatLastError()
    {
        if (string.IsNullOrEmpty(__lastErrorDescription))
        {
            return string.Empty;
        }

        return __lastErrorLine.HasValue
            ? $"{__lastErrorDescription} ({__lastErrorNumber}) at line {__lastErrorLine.Value}"
            : $"{__lastErrorDescription} ({__lastErrorNumber})";
    }

    public static IDisposable PushDynamicFrame(Dictionary<string, Cell> bindings) => new DynamicFrameScope(bindings);

    public static void RegisterGlobal(string name, Cell cell)
    {
        __globalCells[name] = cell;
    }

    public static void RegisterArrayDimensions(Cell cell, params int[] dimensions)
    {
        __arrayDimensions[cell] = dimensions.ToArray();
    }

    public static int[] GetArrayDimensions(Cell cell)
    {
        return __arrayDimensions.TryGetValue(cell, out var dimensions)
            ? dimensions
            : Array.Empty<int>();
    }

    public static string GraphicsPointKey(int x, int y) => $"{x},{y}";

    public static HashSet<string> EnsureGraphicsPixels(int channelId)
    {
        EnsureScreenState(channelId);
        if (!__graphicsPixels.TryGetValue(channelId, out var pixels))
        {
            pixels = new HashSet<string>(StringComparer.Ordinal);
            __graphicsPixels[channelId] = pixels;
        }

        return pixels;
    }

    public static (int X, int Y) GetGraphicsCursor(int channelId)
    {
        EnsureScreenState(channelId);
        return __graphicsCursors[channelId];
    }

    public static void SetGraphicsCursor(int channelId, int x, int y)
    {
        EnsureScreenState(channelId);
        __graphicsCursors[channelId] = (x, y);
    }

    public static void ClearGraphicsState(int channelId)
    {
        EnsureScreenState(channelId);
        __graphicsPixels[channelId].Clear();
        __graphicsCursors[channelId] = (0, 0);
    }

    public static void ClearConsoleForChannel(object? channel, int channelId)
    {
        if (channel is not null)
        {
            return;
        }

        try
        {
            Console.Clear();
        }
        catch
        {
        }
    }

    public static void PlotPoint(int channelId, int x, int y)
    {
        EnsureGraphicsPixels(channelId).Add(GraphicsPointKey(x, y));
        RenderGraphicsPointToConsole(channelId, x, y);
    }

    public static ConsoleColor MapQlColorToConsole(int color)
    {
        return (Math.Abs(color) % 8) switch
        {
            0 => ConsoleColor.Black,
            1 => ConsoleColor.Blue,
            2 => ConsoleColor.Red,
            3 => ConsoleColor.Magenta,
            4 => ConsoleColor.Green,
            5 => ConsoleColor.Cyan,
            6 => ConsoleColor.Yellow,
            _ => ConsoleColor.White
        };
    }

    public static void RenderGraphicsPointToConsole(int channelId, int x, int y)
    {
        if (!IsConsoleBackedChannel(channelId))
        {
            return;
        }

        try
        {
            if (Console.IsOutputRedirected || x < 0 || y < 0)
            {
                return;
            }

            EnsureScreenState(channelId);
            var origin = __screenWindowOrigins[channelId];
            var left = origin.Left + (x / 8);
            var top = origin.Top + (y / 10);

            if (left < 0 || top < 0)
            {
                return;
            }

            if (Console.BufferWidth > 0)
            {
                left = Math.Min(left, Console.BufferWidth - 1);
            }

            if (Console.BufferHeight > 0)
            {
                top = Math.Min(top, Console.BufferHeight - 1);
            }

            var previousLeft = Console.CursorLeft;
            var previousTop = Console.CursorTop;
            var previousForeground = Console.ForegroundColor;
            var previousBackground = Console.BackgroundColor;
            var inks = __screenInkColors[channelId];
            var paper = __screenPaperColors[channelId];
            Console.ForegroundColor = MapQlColorToConsole(inks.Length > 0 ? inks[0] : 7);
            Console.BackgroundColor = MapQlColorToConsole(paper);
            Console.SetCursorPosition(left, top);
            Console.Write('█');
            Console.ForegroundColor = previousForeground;
            Console.BackgroundColor = previousBackground;
            Console.SetCursorPosition(previousLeft, previousTop);
        }
        catch
        {
        }
    }

    public static int HasPoint(int channelId, int x, int y)
    {
        return EnsureGraphicsPixels(channelId).Contains(GraphicsPointKey(x, y)) ? 1 : 0;
    }

    public static void DrawLineSegment(int channelId, int startX, int startY, int endX, int endY)
    {
        var deltaX = endX - startX;
        var deltaY = endY - startY;
        var steps = Math.Max(Math.Abs(deltaX), Math.Abs(deltaY));
        if (steps == 0)
        {
            PlotPoint(channelId, startX, startY);
            return;
        }

        for (var step = 0; step <= steps; step++)
        {
            var x = (int)Math.Round(startX + (deltaX * (step / (double)steps)));
            var y = (int)Math.Round(startY + (deltaY * (step / (double)steps)));
            PlotPoint(channelId, x, y);
        }
    }

    public static void PlotPointDouble(int channelId, double x, double y)
    {
        PlotPoint(channelId, (int)Math.Round(x), (int)Math.Round(y));
    }

    public static void DrawLineSegmentDouble(int channelId, double startX, double startY, double endX, double endY)
    {
        DrawLineSegment(channelId, (int)Math.Round(startX), (int)Math.Round(startY), (int)Math.Round(endX), (int)Math.Round(endY));
    }

    public static void DrawPolyline(int channelId, IReadOnlyList<(double X, double Y)> points, bool closeShape)
    {
        if (points.Count == 0)
        {
            return;
        }

        if (points.Count == 1)
        {
            var point = points[0];
            PlotPointDouble(channelId, point.X, point.Y);
            return;
        }

        for (var index = 1; index < points.Count; index++)
        {
            var start = points[index - 1];
            var end = points[index];
            DrawLineSegmentDouble(channelId, start.X, start.Y, end.X, end.Y);
        }

        if (closeShape)
        {
            var start = points[^1];
            var end = points[0];
            DrawLineSegmentDouble(channelId, start.X, start.Y, end.X, end.Y);
        }
    }

    public static void FillRectanglePixels(int channelId, int startX, int startY, int endX, int endY)
    {
        var minX = Math.Min(startX, endX);
        var maxX = Math.Max(startX, endX);
        var minY = Math.Min(startY, endY);
        var maxY = Math.Max(startY, endY);

        for (var y = minY; y <= maxY; y++)
        {
            for (var x = minX; x <= maxX; x++)
            {
                PlotPoint(channelId, x, y);
            }
        }
    }

    public static void DrawFilledEllipse(int channelId, double centerX, double centerY, double radiusX, double radiusY, double angleDegrees)
    {
        var normalizedRadiusX = Math.Max(1.0, Math.Abs(radiusX));
        var normalizedRadiusY = Math.Max(1.0, Math.Abs(radiusY));
        var rotation = angleDegrees * Math.PI / 180.0;
        var cosAngle = Math.Cos(rotation);
        var sinAngle = Math.Sin(rotation);
        var minX = (int)Math.Floor(centerX - normalizedRadiusX - 1.0);
        var maxX = (int)Math.Ceiling(centerX + normalizedRadiusX + 1.0);
        var minY = (int)Math.Floor(centerY - normalizedRadiusY - 1.0);
        var maxY = (int)Math.Ceiling(centerY + normalizedRadiusY + 1.0);

        for (var py = minY; py <= maxY; py++)
        {
            for (var px = minX; px <= maxX; px++)
            {
                var localX = px - centerX;
                var localY = py - centerY;
                var rotatedX = (localX * cosAngle) + (localY * sinAngle);
                var rotatedY = (-localX * sinAngle) + (localY * cosAngle);
                var norm =
                    ((rotatedX * rotatedX) / (normalizedRadiusX * normalizedRadiusX))
                    + ((rotatedY * rotatedY) / (normalizedRadiusY * normalizedRadiusY));

                if (norm <= 1.0)
                {
                    PlotPoint(channelId, px, py);
                }
            }
        }
    }

    public static List<(double X, double Y)> SampleEllipse(double centerX, double centerY, double radiusX, double radiusY, double angleDegrees, double startAngle, double endAngle, bool closeShape)
    {
        var normalizedRadiusX = Math.Max(1.0, Math.Abs(radiusX));
        var normalizedRadiusY = Math.Max(1.0, Math.Abs(radiusY));
        var rotation = angleDegrees * Math.PI / 180.0;
        var startRadians = startAngle * Math.PI / 180.0;
        var endRadians = endAngle * Math.PI / 180.0;
        var delta =
            closeShape
                ? Math.PI * 2.0
                : endRadians >= startRadians
                    ? endRadians - startRadians
                    : ((Math.PI * 2.0) - startRadians) + endRadians;
        var span = Math.Max(0.001, delta);
        var steps = Math.Max(24, (int)Math.Ceiling(Math.Max(normalizedRadiusX, normalizedRadiusY) * span / 3.0));
        var points = new List<(double X, double Y)>(steps + 1);

        for (var step = 0; step <= steps; step++)
        {
            var t = startRadians + (span * step / (double)steps);
            var localX = normalizedRadiusX * Math.Cos(t);
            var localY = normalizedRadiusY * Math.Sin(t);
            var rotatedX = (localX * Math.Cos(rotation)) - (localY * Math.Sin(rotation));
            var rotatedY = (localX * Math.Sin(rotation)) + (localY * Math.Cos(rotation));
            points.Add((centerX + rotatedX, centerY + rotatedY));
        }

        return points;
    }

    public static void DrawEllipseShape(int channelId, double centerX, double centerY, double radiusX, double radiusY, double angleDegrees)
    {
        var points = SampleEllipse(centerX, centerY, radiusX, radiusY, angleDegrees, 0.0, 360.0, true);
        DrawPolyline(channelId, points, true);

        if (GetGraphicsFillMode(channelId) != 0)
        {
            DrawFilledEllipse(channelId, centerX, centerY, radiusX, radiusY, angleDegrees);
        }
    }

    public static void DrawArcShape(int channelId, double startX, double startY, double endX, double endY, double angleRadians)
    {
        var deltaX = endX - startX;
        var deltaY = endY - startY;
        var chord = Math.Sqrt((deltaX * deltaX) + (deltaY * deltaY));

        if (chord < 0.001 || Math.Abs(angleRadians) < 0.001)
        {
            DrawLineSegmentDouble(channelId, startX, startY, endX, endY);
            return;
        }

        var halfAngle = angleRadians / 2.0;
        var sinHalf = Math.Sin(halfAngle);
        var tanHalf = Math.Tan(halfAngle);

        if (Math.Abs(sinHalf) < 0.000001 || Math.Abs(tanHalf) < 0.000001)
        {
            DrawLineSegmentDouble(channelId, startX, startY, endX, endY);
            return;
        }

        var radius = chord / (2.0 * Math.Abs(sinHalf));
        var midpointX = (startX + endX) / 2.0;
        var midpointY = (startY + endY) / 2.0;
        var perpendicularX = -deltaY / chord;
        var perpendicularY = deltaX / chord;
        var offset = chord / (2.0 * tanHalf);
        var centerX = midpointX + (perpendicularX * offset);
        var centerY = midpointY + (perpendicularY * offset);
        var startAngle = Math.Atan2(startY - centerY, startX - centerX);
        var steps = Math.Max(24, (int)Math.Ceiling(radius * Math.Abs(angleRadians) / 3.0));
        var points = new List<(double X, double Y)>(steps + 1);

        for (var step = 0; step <= steps; step++)
        {
            var t = startAngle + (angleRadians * step / (double)steps);
            points.Add((centerX + (radius * Math.Cos(t)), centerY + (radius * Math.Sin(t))));
        }

        DrawPolyline(channelId, points, false);
    }

    public static void SetPaperColor(int channelId, int value)
    {
        EnsureScreenState(channelId);
        __screenPaperColors[channelId] = value;
    }

    public static void SetCharacterSizeState(int channelId, int width, int height)
    {
        EnsureScreenState(channelId);
        __screenCharacterSizes[channelId] = (width, height);
    }

    public static void SetCharacterFontsState(int channelId, int font1, int font2)
    {
        EnsureScreenState(channelId);
        var current = __screenCharacterFonts[channelId];
        __screenCharacterFonts[channelId] =
            (font1 == -1 ? current.Font1 : font1, font2 == -1 ? current.Font2 : font2);
    }

    public static void SetScrollState(int channelId, int value)
    {
        EnsureScreenState(channelId);
        __screenScrollValues[channelId] = value;
    }

    public static void SetWidthState(int channelId, int value)
    {
        EnsureScreenState(channelId);
        __screenWidthValues[channelId] = value;
    }

    public static void SetPanState(int channelId, int value)
    {
        EnsureScreenState(channelId);
        __screenPanValues[channelId] = value;
    }

    public static void SetRecolorState(int channelId, int[] values)
    {
        EnsureScreenState(channelId);
        __screenRecolorValues[channelId] = values.ToArray();
    }

    public static void SetPaletteState(int channelId, int[] values)
    {
        EnsureScreenState(channelId);
        __screenPaletteValues[channelId] = values.ToArray();
    }

    public static void SetInkColors(int channelId, int[] values)
    {
        EnsureScreenState(channelId);
        __screenInkColors[channelId] = values.ToArray();
    }

    public static void SetStripColors(int channelId, int[] values)
    {
        EnsureScreenState(channelId);
        __screenStripColors[channelId] = values.ToArray();
    }

    public static void SetBorderState(int channelId, int size, int? color)
    {
        EnsureScreenState(channelId);
        __screenBorderValues[channelId] = (size, color);
    }

    public static int GetGraphicsFillMode(int channelId)
    {
        EnsureScreenState(channelId);
        return __graphicsFillModes[channelId];
    }

    public static void SetGraphicsFillMode(int channelId, int value)
    {
        EnsureScreenState(channelId);
        __graphicsFillModes[channelId] = value;
    }

    public static void SetGraphicsScaleState(int channelId, double x, double y, double z)
    {
        EnsureScreenState(channelId);
        __graphicsScales[channelId] = (x, y, z);
    }

    public static void SetGraphicsOverMode(int channelId, int value)
    {
        EnsureScreenState(channelId);
        __graphicsOverModes[channelId] = value;
    }

    public static void SetGraphicsUnderMode(int channelId, int value)
    {
        EnsureScreenState(channelId);
        __graphicsUnderModes[channelId] = value;
    }

    public static void SetGraphicsFlashMode(int channelId, int value)
    {
        EnsureScreenState(channelId);
        __graphicsFlashModes[channelId] = value;
    }

    public static void SetGraphicsPenDownMode(int channelId, bool value)
    {
        EnsureScreenState(channelId);
        __graphicsPenDownModes[channelId] = value;
    }

    public static void SetGraphicsHeading(int channelId, double value)
    {
        EnsureScreenState(channelId);
        __graphicsHeadings[channelId] = value;
    }

    public static double GetGraphicsHeading(int channelId)
    {
        EnsureScreenState(channelId);
        return __graphicsHeadings[channelId];
    }

    public static Cell LookupDynamicCell(string name)
    {
        for (var index = __dynamicFrames.Count - 1; index >= 0; index--)
        {
            if (__dynamicFrames[index].TryGetValue(name, out var frameCell))
            {
                return frameCell;
            }
        }

        if (__globalCells.TryGetValue(name, out var globalCell))
        {
            return globalCell;
        }

        throw new InvalidOperationException($"Dynamic storage '{name}' does not exist.");
    }

    public static Dictionary<string, Cell> EnsureArray(Cell arrayCell)
    {
        if (arrayCell.Value is Dictionary<string, Cell> existing)
        {
            return existing;
        }

        var created = new Dictionary<string, Cell>(StringComparer.OrdinalIgnoreCase);
        arrayCell.Value = created;
        return created;
    }

    public static Cell GetArrayCell(Cell arrayCell, params object?[] indexes)
    {
        var array = EnsureArray(arrayCell);
        var key = ArrayKey(indexes);
        if (!array.TryGetValue(key, out var cell))
        {
            cell = new Cell();
            array[key] = cell;
        }

        return cell;
    }

    public static object? GetArrayValue(Cell arrayCell, params object?[] indexes)
    {
        return GetArrayCell(arrayCell, indexes).Value;
    }

    public static void SetArrayValue(Cell arrayCell, object? value, params object?[] indexes)
    {
        GetArrayCell(arrayCell, indexes).Value = value;
    }

    public static object? GetStringCharValue(object? source, object? index)
    {
        var text = AsString(source);
        var oneBasedIndex = AsInt(index);
        if (oneBasedIndex < 1 || oneBasedIndex > text.Length)
        {
            return string.Empty;
        }

        return text[oneBasedIndex - 1].ToString();
    }

    public static Cell InvalidReferenceActualCell(string message) => throw new InvalidOperationException(message);
    public static object? InvalidReferenceActualValue(string message) => throw new InvalidOperationException(message);

    private static bool IsGraphicsBuiltInName(string name)
    {
        switch (name.ToUpperInvariant())
        {
            case "CLEAR":
            case "WIDTH":
            case "PAN":
            case "PALETTE":
            case "PLOT":
            case "POINT":
            case "POINT_R":
            case "DRAW":
            case "DLINE":
            case "LINE_R":
            case "CIRCLE_R":
            case "ELLIPSE_R":
            case "ARC_R":
            case "FILL":
            case "PENDOWN":
            case "PENUP":
            case "TURN":
            case "TURNTO":
                return true;
            default:
                return false;
        }
    }

    private static Exception UnsupportedBuiltInFunctionException(string name)
    {
        if (IsGraphicsBuiltInName(name))
        {
            return new NotSupportedException($"Graphics built-in function '{name}' is not supported by the generated C# backend yet.");
        }

        return new NotSupportedException($"Built-in function '{name}' is not supported by the generated C# backend yet.");
    }

    private static Exception UnsupportedBuiltInStatementException(string name)
    {
        switch (name.ToUpperInvariant())
        {
            case "RUN":
            case "LOAD":
            case "LRUN":
            case "MERGE":
            case "MRUN":
            case "SAVE":
            case "NEW":
                return new NotSupportedException("Program-management built-ins are not meaningful in generated C# programs.");
            case "SEXEC":
            case "SBYTES":
            case "CALL":
            case "RESPR":
            case "LOADMEM":
            case "SAVEMEM":
            case "SYSVAR":
                return new NotSupportedException($"Host-specific built-in statement '{name}' is explicitly unsupported by the generated C# backend.");
        }

        if (name.StartsWith("TURBO", StringComparison.OrdinalIgnoreCase))
        {
            return new NotSupportedException($"Turbo toolkit built-in statement '{name}' is not supported by the generated C# backend yet.");
        }

        if (IsGraphicsBuiltInName(name))
        {
            return new NotSupportedException($"Graphics built-in statement '{name}' is not supported by the generated C# backend yet.");
        }

        return new NotSupportedException($"Built-in statement '{name}' is not supported by the generated C# backend yet.");
    }

    public static void SetStringCharValue(Cell target, object? index, object? replacement)
    {
        var text = AsString(target.Value);
        var oneBasedIndex = AsInt(index);
        if (oneBasedIndex < 1)
        {
            return;
        }

        var zeroBasedIndex = oneBasedIndex - 1;
        var replacementText = AsString(replacement);
        var replacementChar = replacementText.Length > 0 ? replacementText[0] : ' ';
        var bufferLength = Math.Max(text.Length, zeroBasedIndex + 1);
        var buffer = new string(' ', bufferLength).ToCharArray();

        for (var i = 0; i < text.Length; i++)
        {
            buffer[i] = text[i];
        }

        buffer[zeroBasedIndex] = replacementChar;
        target.Value = new string(buffer);
    }

    public static object? Identity(object? value) => value;
    public static object? Negate(object? value) => -AsDouble(value);
    public static object? BitwiseNot(object? value) => ~AsInt(value);
    public static object? Add(object? left, object? right) => left is string || right is string ? AsString(left) + AsString(right) : AsDouble(left) + AsDouble(right);
    public static object? Subtract(object? left, object? right) => AsDouble(left) - AsDouble(right);
    public static object? Multiply(object? left, object? right) => AsDouble(left) * AsDouble(right);
    public static object? Divide(object? left, object? right) => AsDouble(left) / AsDouble(right);
    public static object? Power(object? left, object? right) => Math.Pow(AsDouble(left), AsDouble(right));
    public static object? Concat(object? left, object? right) => AsString(left) + AsString(right);
    public static object? IntegerDivide(object? left, object? right) => AsInt(left) / AsInt(right);
    public static object? Modulo(object? left, object? right) => AsInt(left) % AsInt(right);
    public static object? BitwiseAnd(object? left, object? right) => AsInt(left) & AsInt(right);
    public static object? BitwiseOr(object? left, object? right) => AsInt(left) | AsInt(right);
    public static object? BitwiseXor(object? left, object? right) => AsInt(left) ^ AsInt(right);
    public static int CompareEqual(object? left, object? right) => Equals(left is string || right is string ? AsString(left) : AsDouble(left), left is string || right is string ? AsString(right) : AsDouble(right)) ? 1 : 0;
    public static int CompareNotEqual(object? left, object? right) => CompareEqual(left, right) == 0 ? 1 : 0;
    public static int CompareLessThan(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) < 0 : AsDouble(left) < AsDouble(right)) ? 1 : 0;
    public static int CompareLessThanOrEqual(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) <= 0 : AsDouble(left) <= AsDouble(right)) ? 1 : 0;
    public static int CompareGreaterThan(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) > 0 : AsDouble(left) > AsDouble(right)) ? 1 : 0;
    public static int CompareGreaterThanOrEqual(object? left, object? right) => (left is string || right is string ? string.CompareOrdinal(AsString(left), AsString(right)) >= 0 : AsDouble(left) >= AsDouble(right)) ? 1 : 0;
    public static int Instr(object? left, object? right) => AsString(left).IndexOf(AsString(right), StringComparison.Ordinal) + 1;
    public static object? SliceRange(object? left, object? right) => (AsInt(left), AsInt(right));
    public static object? ApplyUnary(string name, object? value) => throw new NotSupportedException($"Unary operator '{name}' is not supported by the generated backend yet.");
    public static object? ApplyBinary(string name, object? left, object? right) => throw new NotSupportedException($"Binary operator '{name}' is not supported by the generated backend yet.");

    public static DateTime CurrentUtcNow() => DateTime.UtcNow.AddSeconds(__clockOffsetSeconds);

    public static int SecondsSinceQlEpoch() => (int)CurrentUtcNow().Subtract(QlEpoch).TotalSeconds;

    public static DateTime FromQlSeconds(long seconds) => QlEpoch.AddSeconds(seconds);

    public static bool TryConsumePendingInputKey()
    {
        if (Console.IsInputRedirected)
        {
            try
            {
                if (Console.In.Peek() >= 0)
                {
                    Console.In.Read();
                    return true;
                }
            }
            catch
            {
            }
        }

        try
        {
            if (Console.KeyAvailable)
            {
                Console.ReadKey(intercept: true);
                return true;
            }
        }
        catch
        {
        }

        return false;
    }

    public static int NormalizeChannelId(object? channel, int defaultChannel) => channel is null ? defaultChannel : AsInt(channel);

    public static bool IsConsoleBackedChannel(int channelId) =>
        !__channelReaders.ContainsKey(channelId) && !__channelWriters.ContainsKey(channelId);

    public static void EnsureScreenState(int channelId)
    {
        if (!__screenWindowOrigins.ContainsKey(channelId))
        {
            __screenWindowOrigins[channelId] = (0, 0);
        }

        if (!__screenCursors.ContainsKey(channelId))
        {
            __screenCursors[channelId] = (0, 0);
        }

        if (!__graphicsCursors.ContainsKey(channelId))
        {
            __graphicsCursors[channelId] = (0, 0);
        }

        if (!__graphicsPixels.ContainsKey(channelId))
        {
            __graphicsPixels[channelId] = new HashSet<string>(StringComparer.Ordinal);
        }

        if (!__screenPaperColors.ContainsKey(channelId))
        {
            __screenPaperColors[channelId] = 0;
        }

        if (!__screenInkColors.ContainsKey(channelId))
        {
            __screenInkColors[channelId] = new[] { 7 };
        }

        if (!__screenStripColors.ContainsKey(channelId))
        {
            __screenStripColors[channelId] = new[] { __screenPaperColors[channelId] };
        }

        if (!__screenCharacterSizes.ContainsKey(channelId))
        {
            __screenCharacterSizes[channelId] = (0, 0);
        }

        if (!__graphicsFillModes.ContainsKey(channelId))
        {
            __graphicsFillModes[channelId] = 0;
        }

        if (!__graphicsScales.ContainsKey(channelId))
        {
            __graphicsScales[channelId] = (100.0, 0.0, 0.0);
        }

        if (!__graphicsOverModes.ContainsKey(channelId))
        {
            __graphicsOverModes[channelId] = 0;
        }

        if (!__graphicsUnderModes.ContainsKey(channelId))
        {
            __graphicsUnderModes[channelId] = 0;
        }

        if (!__graphicsFlashModes.ContainsKey(channelId))
        {
            __graphicsFlashModes[channelId] = 0;
        }

        if (!__graphicsPenDownModes.ContainsKey(channelId))
        {
            __graphicsPenDownModes[channelId] = true;
        }

        if (!__graphicsHeadings.ContainsKey(channelId))
        {
            __graphicsHeadings[channelId] = 0.0;
        }

        if (!__screenCharacterFonts.ContainsKey(channelId))
        {
            __screenCharacterFonts[channelId] = (-1, -1);
        }

        if (!__screenScrollValues.ContainsKey(channelId))
        {
            __screenScrollValues[channelId] = 0;
        }

        if (!__screenWidthValues.ContainsKey(channelId))
        {
            __screenWidthValues[channelId] = null;
        }

        if (!__screenPanValues.ContainsKey(channelId))
        {
            __screenPanValues[channelId] = 0;
        }

        if (!__screenRecolorValues.ContainsKey(channelId))
        {
            __screenRecolorValues[channelId] = null;
        }

        if (!__screenPaletteValues.ContainsKey(channelId))
        {
            __screenPaletteValues[channelId] = null;
        }

        if (!__screenBorderValues.ContainsKey(channelId))
        {
            __screenBorderValues[channelId] = (0, null);
        }
    }

    public static void SetScreenCursor(int channelId, int x, int y)
    {
        EnsureScreenState(channelId);
        __screenCursors[channelId] = (Math.Max(0, x), Math.Max(0, y));
    }

    public static void SetWindowOrigin(int channelId, int x, int y)
    {
        EnsureScreenState(channelId);
        __screenWindowOrigins[channelId] = (Math.Max(0, x), Math.Max(0, y));
        __screenCursors[channelId] = (0, 0);
    }

    public static void PositionConsoleCursor(int channelId)
    {
        if (!IsConsoleBackedChannel(channelId))
        {
            return;
        }

        try
        {
            if (Console.IsOutputRedirected)
            {
                return;
            }

            EnsureScreenState(channelId);
            var origin = __screenWindowOrigins[channelId];
            var cursor = __screenCursors[channelId];
            var left = Math.Max(0, origin.Left + cursor.X);
            var top = Math.Max(0, origin.Top + cursor.Y);
            if (Console.BufferWidth > 0)
            {
                left = Math.Min(left, Console.BufferWidth - 1);
            }

            if (Console.BufferHeight > 0)
            {
                top = Math.Min(top, Console.BufferHeight - 1);
            }

            Console.SetCursorPosition(left, top);
        }
        catch
        {
        }
    }

    public static void CaptureConsoleCursor(int channelId)
    {
        if (!IsConsoleBackedChannel(channelId))
        {
            return;
        }

        try
        {
            if (Console.IsOutputRedirected)
            {
                return;
            }

            EnsureScreenState(channelId);
            var origin = __screenWindowOrigins[channelId];
            var left = Console.CursorLeft;
            var top = Console.CursorTop;
            __screenCursors[channelId] = (Math.Max(0, left - origin.Left), Math.Max(0, top - origin.Top));
        }
        catch
        {
        }
    }

    public static TextWriter ResolveWriter(object? channel)
    {
        var channelId = NormalizeChannelId(channel, 1);
        if (__channelWriters.TryGetValue(channelId, out var writer))
        {
            return writer;
        }

        return Console.Out;
    }

    public static TextReader ResolveReader(object? channel)
    {
        var channelId = NormalizeChannelId(channel, 0);
        if (__channelReaders.TryGetValue(channelId, out var reader))
        {
            return reader;
        }

        return Console.In;
    }

    public static void WriteLineToChannel(object? channel, string line)
    {
        var channelId = NormalizeChannelId(channel, 1);
        PositionConsoleCursor(channelId);
        var writer = ResolveWriter(channel);
        writer.WriteLine(line);
        writer.Flush();
        CaptureConsoleCursor(channelId);
    }

    public static void WriteToChannel(object? channel, string text)
    {
        var channelId = NormalizeChannelId(channel, 1);
        PositionConsoleCursor(channelId);
        var writer = ResolveWriter(channel);
        writer.Write(text);
        writer.Flush();
        CaptureConsoleCursor(channelId);
    }

    public static string? ReadLineFromChannel(object? channel)
    {
        return ResolveReader(channel).ReadLine();
    }

    public static void CloseChannel(int channelId)
    {
        if (__channelReaders.TryGetValue(channelId, out var reader))
        {
            reader.Dispose();
            __channelReaders.Remove(channelId);
        }

        if (__channelWriters.TryGetValue(channelId, out var writer))
        {
            writer.Dispose();
            __channelWriters.Remove(channelId);
        }
    }

    public static void CopyFilePath(object? sourceValue, object? targetValue)
    {
        var sourcePath = ResolvePath(sourceValue);
        var targetPath = ResolvePath(targetValue);
        File.Copy(sourcePath, targetPath, overwrite: true);
    }

    public static Stream ResolveChannelStream(object? channel)
    {
        var channelId = NormalizeChannelId(channel, 3);
        if (__channelReaders.TryGetValue(channelId, out var reader))
        {
            return reader.BaseStream;
        }

        if (__channelWriters.TryGetValue(channelId, out var writer))
        {
            writer.Flush();
            return writer.BaseStream;
        }

        throw new InvalidOperationException("Channel is not open.");
    }

    public static List<(double X, double Y, double Radius, double? Ratio, double? Angle)> ParseCircleGroups(object?[] args)
    {
        var values = args.Select(AsDouble).ToArray();
        List<(double X, double Y, double Radius, double? Ratio, double? Angle)>? Loop(int index)
        {
            if (index == values.Length)
            {
                return new List<(double X, double Y, double Radius, double? Ratio, double? Angle)>();
            }

            if (values.Length - index == 3)
            {
                return new List<(double X, double Y, double Radius, double? Ratio, double? Angle)>
                {
                    (values[index], values[index + 1], values[index + 2], null, null)
                };
            }

            if (values.Length - index == 5)
            {
                return new List<(double X, double Y, double Radius, double? Ratio, double? Angle)>
                {
                    (values[index], values[index + 1], values[index + 2], values[index + 3], values[index + 4])
                };
            }

            if (values.Length - index >= 5)
            {
                var asEllipse = Loop(index + 5);
                if (asEllipse is not null)
                {
                    asEllipse.Insert(0, (values[index], values[index + 1], values[index + 2], values[index + 3], values[index + 4]));
                    return asEllipse;
                }

                var asCircle = Loop(index + 3);
                if (asCircle is not null)
                {
                    asCircle.Insert(0, (values[index], values[index + 1], values[index + 2], null, null));
                    return asCircle;
                }
            }
            else if (values.Length - index >= 3)
            {
                var asCircle = Loop(index + 3);
                if (asCircle is not null)
                {
                    asCircle.Insert(0, (values[index], values[index + 1], values[index + 2], null, null));
                    return asCircle;
                }
            }

            return null;
        }

        return Loop(0) ?? throw new InvalidOperationException("Built-in statement expects arguments in groups of 3 or 5.");
    }

    public static List<(double? StartX, double? StartY, double EndX, double EndY, double Angle)> ParseArcGroups(object?[] args)
    {
        (double, double)? TrySplitRange(object? value) =>
            value is ValueTuple<int, int> tuple ? (tuple.Item1, tuple.Item2) : null;

        List<(double? StartX, double? StartY, double EndX, double EndY, double Angle)> Loop(int index, double? pendingEndX)
        {
            if (index >= args.Length)
            {
                if (pendingEndX.HasValue)
                {
                    throw new InvalidOperationException("Built-in statement expects arguments in groups of x1,y1 TO x2,y2,angle or TO x2,y2,angle.");
                }

                return new List<(double? StartX, double? StartY, double EndX, double EndY, double Angle)>();
            }

            if (pendingEndX.HasValue)
            {
                if (index + 1 >= args.Length)
                {
                    throw new InvalidOperationException("Built-in statement expects arguments in groups of x1,y1 TO x2,y2,angle or TO x2,y2,angle.");
                }

                var y2 = AsDouble(args[index]);
                var angle = AsDouble(args[index + 1]);
                var rest = Loop(index + 2, null);
                rest.Insert(0, (null, null, pendingEndX.Value, y2, angle));
                return rest;
            }

            if (index + 3 < args.Length && TrySplitRange(args[index + 1]) is { } rangedStart)
            {
                var angle = AsDouble(args[index + 3]);
                var rest = Loop(index + 4, null);
                rest.Insert(0, (AsDouble(args[index]), rangedStart.Item1, rangedStart.Item2, AsDouble(args[index + 2]), angle));
                return rest;
            }

            if (index + 4 < args.Length)
            {
                var rest = Loop(index + 5, null);
                rest.Insert(0, (AsDouble(args[index]), AsDouble(args[index + 1]), AsDouble(args[index + 2]), AsDouble(args[index + 3]), AsDouble(args[index + 4])));
                return rest;
            }

            if (index + 2 < args.Length)
            {
                var rest = Loop(index + 3, null);
                rest.Insert(0, (null, null, AsDouble(args[index]), AsDouble(args[index + 1]), AsDouble(args[index + 2])));
                return rest;
            }

            throw new InvalidOperationException("Built-in statement expects arguments in groups of x1,y1 TO x2,y2,angle or TO x2,y2,angle.");
        }

        return Loop(0, null);
    }

    public static void SetChannelTarget(object? channel, object? sourceChannelValue)
    {
        var logicalChannelId = NormalizeChannelId(channel, 1);
        var sourceChannelId = AsInt(sourceChannelValue);

        CloseChannel(logicalChannelId);

        if (__channelReaders.TryGetValue(sourceChannelId, out var reader))
        {
            __channelReaders[logicalChannelId] = reader;
        }

        if (__channelWriters.TryGetValue(sourceChannelId, out var writer))
        {
            __channelWriters[logicalChannelId] = writer;
        }

        if (!__channelReaders.ContainsKey(logicalChannelId) && !__channelWriters.ContainsKey(logicalChannelId))
        {
            throw new InvalidOperationException("SET_CHANNEL failed in generated C# backend.");
        }
    }

    public static string ResolvePath(object? value)
    {
        var raw = AsString(value).Trim();
        if (string.IsNullOrWhiteSpace(raw))
        {
            throw new InvalidOperationException("Path cannot be empty.");
        }

        return Path.GetFullPath(raw);
    }

    public static byte ReadMemoryByte(int address)
    {
        if (address < 0)
        {
            throw new InvalidOperationException($"Memory address {address} is invalid.");
        }

        return __memory.TryGetValue(address, out var value) ? value : (byte)0;
    }

    public static void WriteMemoryByte(int address, int value)
    {
        if (address < 0)
        {
            throw new InvalidOperationException($"Memory address {address} is invalid.");
        }

        __memory[address] = (byte)(value & 255);
    }

    public static int PeekMemory(int address, int width)
    {
        return width switch
        {
            1 => ReadMemoryByte(address),
            2 => ReadMemoryByte(address) | (ReadMemoryByte(address + 1) << 8),
            4 => ReadMemoryByte(address)
                 | (ReadMemoryByte(address + 1) << 8)
                 | (ReadMemoryByte(address + 2) << 16)
                 | (ReadMemoryByte(address + 3) << 24),
            _ => throw new InvalidOperationException($"Unsupported memory width {width}.")
        };
    }

    public static void PokeMemory(int address, int value, int width)
    {
        switch (width)
        {
            case 1:
                WriteMemoryByte(address, value);
                break;
            case 2:
                WriteMemoryByte(address, value);
                WriteMemoryByte(address + 1, value >> 8);
                break;
            case 4:
                WriteMemoryByte(address, value);
                WriteMemoryByte(address + 1, value >> 8);
                WriteMemoryByte(address + 2, value >> 16);
                WriteMemoryByte(address + 3, value >> 24);
                break;
            default:
                throw new InvalidOperationException($"Unsupported memory width {width}.");
        }
    }

    public static bool IsEndOfFile(int channelId)
    {
        if (__channelReaders.TryGetValue(channelId, out var reader))
        {
            return reader.EndOfStream;
        }

        return true;
    }

    public static ConsoleKeyInfo? TryReadConsoleKey()
    {
        if (Console.IsInputRedirected)
        {
            try
            {
                var next = Console.In.Peek();
                if (next >= 0)
                {
                    var ch = (char)Console.In.Read();
                    return new ConsoleKeyInfo(ch, 0, false, false, false);
                }
            }
            catch
            {
            }
        }

        try
        {
            if (Console.KeyAvailable)
            {
                return Console.ReadKey(intercept: true);
            }
        }
        catch
        {
        }

        return null;
    }

    public static object? InvokeBuiltInFunction(string name, params object?[] args)
    {
        switch (name.ToUpperInvariant())
        {
            case "DATE": return SecondsSinceQlEpoch();
            case "EOF": return IsEndOfFile(AsInt(args.ElementAtOrDefault(0))) ? 1 : 0;
            case "ERLIN": return __lastErrorLine ?? 0;
            case "ERNUM": return __lastErrorNumber;
            case "ERR_NC":
            case "ERR_NJ":
            case "ERR_OM":
            case "ERR_OR":
            case "ERR_BO":
            case "ERR_NO":
            case "ERR_NF":
            case "ERR_EX":
            case "ERR_IU":
            case "ERR_EF":
            case "ERR_DF":
            case "ERR_BN":
            case "ERR_TE":
            case "ERR_FF":
            case "ERR_BP":
            case "ERR_FE":
            case "ERR_XP":
            case "ERR_OV":
            case "ERR_NI":
            case "ERR_RO":
            case "ERR_BL":
                return string.Equals(__lastErrorName, name.ToUpperInvariant(), StringComparison.Ordinal) ? 1 : 0;
            case "BEEPING": return __beeping ? 1 : 0;
            case "DIMN":
            {
                if (args.ElementAtOrDefault(0) is not Cell arrayCell)
                {
                    return 0;
                }

                var requestedDimension = AsInt(args.ElementAtOrDefault(1));
                var dimensions = GetArrayDimensions(arrayCell);
                return requestedDimension >= 1 && requestedDimension <= dimensions.Length
                    ? dimensions[requestedDimension - 1]
                    : 0;
            }
            case "PEEK": return PeekMemory(AsInt(args.ElementAtOrDefault(0)), 1);
            case "PEEK_W": return PeekMemory(AsInt(args.ElementAtOrDefault(0)), 2);
            case "PEEK_L": return PeekMemory(AsInt(args.ElementAtOrDefault(0)), 4);
            case "POINT":
            {
                var channelId = args.Length >= 3 ? NormalizeChannelId(args.ElementAtOrDefault(0), 1) : 1;
                var x = args.Length >= 3 ? AsInt(args.ElementAtOrDefault(1)) : AsInt(args.ElementAtOrDefault(0));
                var y = args.Length >= 3 ? AsInt(args.ElementAtOrDefault(2)) : AsInt(args.ElementAtOrDefault(1));
                return HasPoint(channelId, x, y);
            }
            case "POINT_R":
            {
                var channelId = args.Length >= 3 ? NormalizeChannelId(args.ElementAtOrDefault(0), 1) : 1;
                var cursor = GetGraphicsCursor(channelId);
                var deltaX = args.Length >= 3 ? AsInt(args.ElementAtOrDefault(1)) : AsInt(args.ElementAtOrDefault(0));
                var deltaY = args.Length >= 3 ? AsInt(args.ElementAtOrDefault(2)) : AsInt(args.ElementAtOrDefault(1));
                return HasPoint(channelId, cursor.X + deltaX, cursor.Y + deltaY);
            }
            default:
                return ManagedBuiltInBridge.InvokeGeneratedFunction(
                    name,
                    args,
                    new Func<DateTime>(CurrentUtcNow),
                    __random,
                    new Func<string, string>(name => Environment.GetEnvironmentVariable(name) ?? string.Empty),
                    new Func<ManagedKeyInfo>(() =>
                    {
                        var key = TryReadConsoleKey();
                        if (!key.HasValue)
                        {
                            return null!;
                        }

                        var keyChar = key.Value.KeyChar;
                        return new ManagedKeyInfo((int)keyChar, keyChar == '\0' ? string.Empty : keyChar.ToString());
                    }),
                    new Action<int>(milliseconds => System.Threading.Thread.Sleep(milliseconds)),
                    new Func<int, bool>(IsEndOfFile));
        }
    }

    public static void ExecuteBuiltInStatement(string name, object? channel, params object?[] args)
    {
        switch (name.ToUpperInvariant())
        {
            case "PRINT":
                WriteLineToChannel(channel, ManagedBuiltInBridge.FormatPrintLine(args));
                break;
            case "REFERENCE":
                break;
            case "RETRY":
                throw new RetryControlException(args.Length == 0 ? null : AsInt(args.ElementAtOrDefault(0)));
            case "CONTINUE":
                throw new ContinueControlException(args.Length == 0 ? null : AsInt(args.ElementAtOrDefault(0)));
            case "RANDOMISE":
                __random = args.Length == 0 ? new Random() : new Random(AsInt(args.ElementAtOrDefault(0)));
                break;
            case "MODE":
                __screenMode = AsInt(args.ElementAtOrDefault(0));
                break;
            case "PAUSE":
                System.Threading.Thread.Sleep(Math.Max(0, AsInt(args.ElementAtOrDefault(0))));
                break;
            case "WAIT":
            {
                var duration = Math.Max(0, AsInt(args.ElementAtOrDefault(0)));
                while (duration > 0)
                {
                    if (TryConsumePendingInputKey())
                    {
                        break;
                    }

                    var slice = Math.Min(20, duration);
                    System.Threading.Thread.Sleep(slice);
                    duration -= slice;
                }

                break;
            }
            case "SDATE":
                if (args.Length >= 1)
                {
                    __clockOffsetSeconds = AsInt(args.ElementAtOrDefault(0)) - SecondsSinceQlEpoch();
                }
                break;
            case "SLUG":
            {
                var count = Math.Max(0, AsInt(args.ElementAtOrDefault(0)));
                for (var i = 0; i < count; i++)
                {
                    System.Threading.Thread.SpinWait(1000);
                }

                break;
            }
            case "FLUSH":
                while (Console.KeyAvailable)
                {
                    Console.ReadKey(true);
                }
                break;
            case "BEEP":
                __beeping = true;
                try { Console.Beep(Math.Max(37, AsInt(args.ElementAtOrDefault(0))), Math.Max(0, AsInt(args.ElementAtOrDefault(1)))); } catch { }
                __beeping = false;
                break;
            case "POKE":
                PokeMemory(AsInt(args.ElementAtOrDefault(0)), AsInt(args.ElementAtOrDefault(1)), 1);
                break;
            case "POKE_W":
                PokeMemory(AsInt(args.ElementAtOrDefault(0)), AsInt(args.ElementAtOrDefault(1)), 2);
                break;
            case "POKE_L":
                PokeMemory(AsInt(args.ElementAtOrDefault(0)), AsInt(args.ElementAtOrDefault(1)), 4);
                break;
            case "OPEN":
            case "OPEN_IN":
            case "OPEN_NEW":
            case "APPEND":
            {
                var channelId = NormalizeChannelId(channel, 3);
                ManagedBuiltInBridge.ExecuteGeneratedOpen(
                    name,
                    channelId,
                    args,
                    new Action<int>(CloseChannel),
                    new Action<int, string>((id, path) => __channelReaders[id] = new StreamReader(File.Open(ResolvePath(path), FileMode.Open, FileAccess.Read, FileShare.ReadWrite))),
                    new Action<int, string>((id, path) => __channelWriters[id] = new StreamWriter(File.Open(ResolvePath(path), FileMode.Append, FileAccess.Write, FileShare.Read)) { AutoFlush = true }),
                    new Action<int, string>((id, path) => __channelWriters[id] = new StreamWriter(File.Open(ResolvePath(path), FileMode.Create, FileAccess.Write, FileShare.Read)) { AutoFlush = true }));
                break;
            }
            case "CLOSE":
                ManagedBuiltInBridge.ExecuteGeneratedClose(NormalizeChannelId(channel, 3), new Action<int>(CloseChannel));
                break;
            case "DELETE":
                ManagedBuiltInBridge.ExecuteGeneratedDelete(args, new Action<string>(path => File.Delete(ResolvePath(path))));
                break;
            case "COPY":
            case "COPY_N":
                ManagedBuiltInBridge.ExecuteGeneratedCopy(args, new Action<string, string>((sourcePath, targetPath) => File.Copy(ResolvePath(sourcePath), ResolvePath(targetPath), overwrite: true)));
                break;
            case "MOVE":
                ManagedBuiltInBridge.ExecuteGeneratedRename(args, new Action<string, string>((sourcePath, targetPath) => File.Move(ResolvePath(sourcePath), ResolvePath(targetPath), overwrite: true)));
                break;
            case "RENAME":
                ManagedBuiltInBridge.ExecuteGeneratedRename(args, new Action<string, string>((sourcePath, targetPath) => File.Move(ResolvePath(sourcePath), ResolvePath(targetPath), overwrite: true)));
                break;
            case "TRUNCATE":
                ManagedBuiltInBridge.ExecuteGeneratedTruncate(new Action(() =>
                {
                    var stream = ResolveChannelStream(channel);
                    stream.SetLength(stream.Position);
                }));
                break;
            case "SET_POSITION":
            {
                ManagedBuiltInBridge.ExecuteGeneratedSetPosition(
                    args,
                    new Action<int>(position =>
                    {
                        var stream = ResolveChannelStream(channel);
                        stream.Seek(position, SeekOrigin.Begin);
                    }),
                    new Action(() =>
                    {
                        if (__channelReaders.TryGetValue(NormalizeChannelId(channel, 3), out var reader))
                        {
                            reader.DiscardBufferedData();
                        }
                    }));
                break;
            }
            case "SET_CHANNEL":
                ManagedBuiltInBridge.ExecuteGeneratedSetChannel(args.ElementAtOrDefault(0) ?? channel ?? 1, new Action<object?>(source => SetChannelTarget(channel, source)));
                break;
            case "DIR":
            {
                var path = args.Length == 0 ? Directory.GetCurrentDirectory() : ResolvePath(args.ElementAtOrDefault(0));
                foreach (var entry in Directory.EnumerateFileSystemEntries(path).Select(Path.GetFileName).OrderBy(x => x, StringComparer.OrdinalIgnoreCase))
                {
                    WriteLineToChannel(channel, entry ?? string.Empty);
                }

                break;
            }
            case "REPORT":
                if (args.Length == 0)
                {
                    var formatted = FormatLastError();
                    if (!string.IsNullOrEmpty(formatted))
                    {
                        WriteLineToChannel(channel, formatted);
                    }
                }
                else
                {
                    WriteLineToChannel(channel, AsString(args.ElementAtOrDefault(0)));
                }

                break;
            case "CLS":
                ManagedBuiltInBridge.ExecuteGeneratedCls(
                    NormalizeChannelId(channel, 1),
                    new Action<int>(ClearGraphicsState),
                    new Action<int, int, int>(SetScreenCursor),
                    new Action(() => ClearConsoleForChannel(channel, NormalizeChannelId(channel, 1))));
                break;
            case "WINDOW":
                ManagedBuiltInBridge.ExecuteGeneratedWindow(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int, int>(SetWindowOrigin),
                    new Action<int>(PositionConsoleCursor));
                break;
            case "AT":
                ManagedBuiltInBridge.ExecuteGeneratedAt(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int, int>(SetScreenCursor),
                    new Action<int>(PositionConsoleCursor));
                break;
            case "CURSOR":
            {
                if (args.Length >= 2)
                {
                    var channelId = NormalizeChannelId(channel, 1);
                    SetScreenCursor(channelId, AsInt(args.ElementAtOrDefault(0)), AsInt(args.ElementAtOrDefault(1)));
                    PositionConsoleCursor(channelId);
                }

                break;
            }
            case "CSIZE":
                ManagedBuiltInBridge.ExecuteGeneratedCharacterSize(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int, int>(SetCharacterSizeState));
                break;
            case "CHAR_USE":
            case "S_FONT":
                ManagedBuiltInBridge.ExecuteGeneratedCharacterFonts(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int, int>(SetCharacterFontsState));
                break;
            case "PLOT":
                ManagedBuiltInBridge.ExecuteGeneratedPlot(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int, int>(PlotPoint),
                    new Action<int, int, int>(SetGraphicsCursor));
                break;
            case "DRAW":
            case "DLINE":
                ManagedBuiltInBridge.ExecuteGeneratedDrawRelative(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Func<int, ValueTuple<int, int>>(channelId =>
                    {
                        var cursor = GetGraphicsCursor(channelId);
                        return new ValueTuple<int, int>(cursor.X, cursor.Y);
                    }),
                    new Action<int, int, int, int, int>(DrawLineSegment),
                    new Action<int, int, int>(SetGraphicsCursor));
                break;
            case "LINE":
                ManagedBuiltInBridge.ExecuteGeneratedLine(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int, int, int, int>(DrawLineSegment),
                    new Action<int, int, int>(SetGraphicsCursor));
                break;
            case "LINE_R":
                ManagedBuiltInBridge.ExecuteGeneratedLineRelative(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Func<int, ValueTuple<int, int>>(channelId =>
                    {
                        var cursor = GetGraphicsCursor(channelId);
                        return new ValueTuple<int, int>(cursor.X, cursor.Y);
                    }),
                    new Action<int, int, int, int, int>(DrawLineSegment),
                    new Action<int, int, int>(SetGraphicsCursor));
                break;
            case "CIRCLE":
            {
                var channelId = NormalizeChannelId(channel, 1);
                foreach (var group in ParseCircleGroups(args))
                {
                    if (group.Ratio.HasValue && group.Angle.HasValue)
                    {
                        var ratioScale = Math.Max(0.05, Math.Abs(group.Ratio.Value));
                        var ellipseRadiusX = Math.Max(1.0, Math.Abs(group.Radius) * ratioScale);
                        var ellipseRadiusY = Math.Max(1.0, Math.Abs(group.Radius));
                        DrawEllipseShape(channelId, group.X, group.Y, ellipseRadiusX, ellipseRadiusY, -group.Angle.Value);
                    }
                    else
                    {
                        var radius = Math.Max(1.0, Math.Abs(group.Radius));
                        DrawEllipseShape(channelId, group.X, group.Y, radius, radius, 0.0);
                    }

                    SetGraphicsCursor(channelId, (int)Math.Round(group.X), (int)Math.Round(group.Y));
                }

                break;
            }
            case "CIRCLE_R":
            {
                var channelId = NormalizeChannelId(channel, 1);
                foreach (var group in ParseCircleGroups(args))
                {
                    var origin = GetGraphicsCursor(channelId);
                    var centerX = origin.X + group.X;
                    var centerY = origin.Y + group.Y;
                    if (group.Ratio.HasValue && group.Angle.HasValue)
                    {
                        var ratioScale = Math.Max(0.05, Math.Abs(group.Ratio.Value));
                        var ellipseRadiusX = Math.Max(1.0, Math.Abs(group.Radius) * ratioScale);
                        var ellipseRadiusY = Math.Max(1.0, Math.Abs(group.Radius));
                        DrawEllipseShape(channelId, centerX, centerY, ellipseRadiusX, ellipseRadiusY, -group.Angle.Value);
                    }
                    else
                    {
                        var radius = Math.Max(1.0, Math.Abs(group.Radius));
                        DrawEllipseShape(channelId, centerX, centerY, radius, radius, 0.0);
                    }

                    SetGraphicsCursor(channelId, (int)Math.Round(centerX), (int)Math.Round(centerY));
                }

                break;
            }
            case "ELLIPSE":
            {
                if (args.Length >= 5)
                {
                    var channelId = NormalizeChannelId(channel, 1);
                    var x = AsDouble(args.ElementAtOrDefault(0));
                    var y = AsDouble(args.ElementAtOrDefault(1));
                    var radius = Math.Abs(AsDouble(args.ElementAtOrDefault(2)));
                    var ratio = Math.Max(0.05, Math.Abs(AsDouble(args.ElementAtOrDefault(3))));
                    var angle = AsDouble(args.ElementAtOrDefault(4));
                    DrawEllipseShape(channelId, x, y, Math.Max(1.0, radius * ratio), Math.Max(1.0, radius), -angle);
                    SetGraphicsCursor(channelId, (int)Math.Round(x), (int)Math.Round(y));
                }

                break;
            }
            case "ELLIPSE_R":
            {
                if (args.Length >= 5)
                {
                    var channelId = NormalizeChannelId(channel, 1);
                    var origin = GetGraphicsCursor(channelId);
                    var centerX = origin.X + AsDouble(args.ElementAtOrDefault(0));
                    var centerY = origin.Y + AsDouble(args.ElementAtOrDefault(1));
                    var radius = Math.Abs(AsDouble(args.ElementAtOrDefault(2)));
                    var ratio = Math.Max(0.05, Math.Abs(AsDouble(args.ElementAtOrDefault(3))));
                    var angle = AsDouble(args.ElementAtOrDefault(4));
                    DrawEllipseShape(channelId, centerX, centerY, Math.Max(1.0, radius * ratio), Math.Max(1.0, radius), -angle);
                    SetGraphicsCursor(channelId, (int)Math.Round(centerX), (int)Math.Round(centerY));
                }

                break;
            }
            case "ARC":
            {
                var channelId = NormalizeChannelId(channel, 1);
                foreach (var group in ParseArcGroups(args))
                {
                    var startX = group.StartX ?? GetGraphicsCursor(channelId).X;
                    var startY = group.StartY ?? GetGraphicsCursor(channelId).Y;
                    if (Math.Abs(group.Angle) >= (2.0 * Math.PI))
                    {
                        throw new InvalidOperationException("Built-in statement 'ARC' requires ABS(angle) to be less than 2*PI.");
                    }

                    DrawArcShape(channelId, startX, startY, group.EndX, group.EndY, -group.Angle);
                    SetGraphicsCursor(channelId, (int)Math.Round(group.EndX), (int)Math.Round(group.EndY));
                }

                break;
            }
            case "ARC_R":
            {
                var channelId = NormalizeChannelId(channel, 1);
                foreach (var group in ParseArcGroups(args))
                {
                    var origin = GetGraphicsCursor(channelId);
                    var startX = group.StartX.HasValue ? origin.X + group.StartX.Value : origin.X;
                    var startY = group.StartY.HasValue ? origin.Y + group.StartY.Value : origin.Y;
                    var endX = origin.X + group.EndX;
                    var endY = origin.Y + group.EndY;
                    if (Math.Abs(group.Angle) >= (2.0 * Math.PI))
                    {
                        throw new InvalidOperationException("Built-in statement 'ARC_R' requires ABS(angle) to be less than 2*PI.");
                    }

                    DrawArcShape(channelId, startX, startY, endX, endY, -group.Angle);
                    SetGraphicsCursor(channelId, (int)Math.Round(endX), (int)Math.Round(endY));
                }

                break;
            }
            case "BLOCK":
            {
                if (args.Length >= 5)
                {
                    var channelId = NormalizeChannelId(channel, 1);
                    var width = Math.Max(0, (int)Math.Round(AsDouble(args.ElementAtOrDefault(0))));
                    var height = Math.Max(0, (int)Math.Round(AsDouble(args.ElementAtOrDefault(1))));
                    var x = (int)Math.Round(AsDouble(args.ElementAtOrDefault(2)));
                    var y = (int)Math.Round(AsDouble(args.ElementAtOrDefault(3)));
                    FillRectanglePixels(channelId, x, y, x + Math.Max(0, width - 1), y + Math.Max(0, height - 1));
                    SetGraphicsCursor(channelId, x, y);
                }

                break;
            }
            case "FILL":
                ManagedBuiltInBridge.ExecuteGeneratedFill(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int>(SetGraphicsFillMode));
                break;
            case "SCALE":
                ManagedBuiltInBridge.ExecuteGeneratedScale(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, double, double, double>(SetGraphicsScaleState));
                break;
            case "OVER":
                ManagedBuiltInBridge.ExecuteGeneratedGraphicsMode(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int>(SetGraphicsOverMode));
                break;
            case "UNDER":
                ManagedBuiltInBridge.ExecuteGeneratedGraphicsMode(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int>(SetGraphicsUnderMode));
                break;
            case "FLASH":
                ManagedBuiltInBridge.ExecuteGeneratedGraphicsMode(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int>(SetGraphicsFlashMode));
                break;
            case "PENDOWN":
                ManagedBuiltInBridge.ExecuteGeneratedPenDown(
                    NormalizeChannelId(channel, 1),
                    new Action<int, bool>(SetGraphicsPenDownMode));
                break;
            case "PENUP":
                ManagedBuiltInBridge.ExecuteGeneratedPenUp(
                    NormalizeChannelId(channel, 1),
                    new Action<int, bool>(SetGraphicsPenDownMode));
                break;
            case "TURN":
                ManagedBuiltInBridge.ExecuteGeneratedTurn(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Func<int, double>(GetGraphicsHeading),
                    new Action<int, double>(SetGraphicsHeading));
                break;
            case "TURNTO":
                ManagedBuiltInBridge.ExecuteGeneratedTurnTo(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, double>(SetGraphicsHeading));
                break;
            case "PAPER":
                ManagedBuiltInBridge.ExecuteGeneratedPaper(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int>(SetPaperColor),
                    new Action<int, int[]>(SetStripColors));
                break;
            case "INK":
                ManagedBuiltInBridge.ExecuteGeneratedInk(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int[]>(SetInkColors));
                break;
            case "STRIP":
                ManagedBuiltInBridge.ExecuteGeneratedStrip(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int[]>(SetStripColors));
                break;
            case "BORDER":
                ManagedBuiltInBridge.ExecuteGeneratedBorder(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int, int?>((channelId, size, color) => SetBorderState(channelId, size, color)));
                break;
            case "CLEAR":
                ManagedBuiltInBridge.ExecuteGeneratedClear(
                    NormalizeChannelId(channel, 1),
                    new Action<int>(ClearGraphicsState));
                break;
            case "SCROLL":
                ManagedBuiltInBridge.ExecuteGeneratedScroll(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int>(SetScrollState));
                break;
            case "WIDTH":
                ManagedBuiltInBridge.ExecuteGeneratedWidth(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int>(SetWidthState));
                break;
            case "PAN":
                ManagedBuiltInBridge.ExecuteGeneratedPan(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int>(SetPanState));
                break;
            case "RECOL":
                ManagedBuiltInBridge.ExecuteGeneratedRecolor(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int[]>(SetRecolorState));
                break;
            case "PALETTE":
                ManagedBuiltInBridge.ExecuteGeneratedPalette(
                    NormalizeChannelId(channel, 1),
                    args,
                    new Action<int, int[]>(SetPaletteState));
                break;
            case "POINT":
            case "POINT_R":
                throw UnsupportedBuiltInStatementException(name);
            case "STOP":
                throw new OperationCanceledException("STOP encountered.");
            default:
                throw UnsupportedBuiltInStatementException(name);
        }
    }

    public static void ExecuteInput(object? channel, object?[] prompts)
    {
        if (prompts.Length > 0)
        {
            WriteToChannel(channel, ManagedBuiltInBridge.FormatPrintLine(prompts));
        }

        var line = ReadLineFromChannel(channel) ?? string.Empty;
        __inputBuffer = ManagedBuiltInBridge.SplitInputLine(line);
    }

    public static object? ReadInputValue(int index, string targetType)
    {
        var raw = index < __inputBuffer.Length ? __inputBuffer[index] : string.Empty;
        return ManagedBuiltInBridge.ParseInputValue(targetType, raw);
    }

    public static object? ReadDataValue(string targetType)
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

    public static void RestoreToLine(int line)
    {
        var restoreTarget = __restorePoints
            .Where(entry => entry.Key >= line)
            .OrderBy(entry => entry.Key)
            .Select(entry => (int?)entry.Value)
            .FirstOrDefault();

        if (!restoreTarget.HasValue)
        {
            throw new InvalidOperationException($"RESTORE line {line} does not exist.");
        }

        __dataPointer = restoreTarget.Value;
    }
}
