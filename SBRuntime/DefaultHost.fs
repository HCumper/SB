namespace SBRuntime

open System
open System.IO
open System.Collections.Generic

// Minimal default host implementation for console-style execution and tests.

type DefaultHostOptions = {
    ReadLine: unit -> string option
    ReadKey: unit -> KeyInfo option
    KeyAvailable: unit -> bool
    KeyRowState: int -> int
    WriteLine: string -> unit
}

module private ScreenDefaults =
    let defaultWindowForMode (mode: ScreenModeInfo) =
        mode.Width, mode.Height, 0, 0

    let defaultCharacterSizeForMode (_mode: ScreenModeInfo) =
        1, 1

    let supportedModes =
        [ { Mode = QlMode4; Width = 512; Height = 256; Colors = Some 4; Name = "QL Mode 4"; IsQlCompatible = true }
          { Mode = QlMode8; Width = 256; Height = 256; Colors = Some 8; Name = "QL Mode 8"; IsQlCompatible = true }
          { Mode = ExtendedMode 256; Width = 256; Height = 256; Colors = Some 8; Name = "Extended Mode 256"; IsQlCompatible = false }
          { Mode = ExtendedMode 512; Width = 512; Height = 256; Colors = Some 4; Name = "Extended Mode 512"; IsQlCompatible = false } ]

type private DefaultChannel(id: ChannelId, kind: ChannelKind, reader: unit -> string option, writer: string -> unit) =
    interface IChannel with
        member _.Id = id
        member _.Kind = kind
        member _.WriteText text = writer text
        member _.ReadText() = reader ()
        member _.IsEndOfFile() = false
        member _.Flush() = ()
        member _.Close() = ()

type private NullChannel(id: ChannelId) =
    interface IChannel with
        member _.Id = id
        member _.Kind = NamedChannel "NUL"
        member _.WriteText(_text) = ()
        member _.ReadText() = None
        member _.IsEndOfFile() = true
        member _.Flush() = ()
        member _.Close() = ()

type private DefaultPrinterChannel(id: ChannelId, writer: string -> unit) =
    interface IChannel with
        member _.Id = id
        member _.Kind = PrinterChannel
        member _.WriteText text = writer text
        member _.ReadText() = None
        member _.IsEndOfFile() = true
        member _.Flush() = ()
        member _.Close() = ()

type private DefaultScreenChannel(id: ChannelId, kind: ChannelKind, reader: unit -> string option, writer: string -> unit, initialMode: ScreenModeInfo) =
    let mutable window = 0, 0, 0, 0
    let mutable scroll = 0
    let mutable width = None
    let mutable pan = 0
    let mutable recolor = None
    let mutable palette = None
    let mutable cursor = 0, 0
    let mutable characterSize = 0, 0
    let mutable ink = [ 7 ]
    let mutable paper = 0
    let mutable border = 0

    let resetForMode (mode: ScreenModeInfo) =
        window <- ScreenDefaults.defaultWindowForMode mode
        scroll <- 0
        width <- None
        pan <- 0
        recolor <- None
        palette <- None
        cursor <- 0, 0
        characterSize <- ScreenDefaults.defaultCharacterSizeForMode mode
        ink <- [ 7 ]
        paper <- 0
        border <- 0

    do
        resetForMode initialMode

    interface IScreenChannel with
        member _.Id = id
        member _.Kind = kind
        member _.WriteText text = writer text
        member _.ReadText() = reader ()
        member _.IsEndOfFile() = false
        member _.Flush() = ()
        member _.Close() = ()
        member _.Clear() = ()
        member _.SetWindow(width, height, x, y) = window <- width, height, x, y
        member _.GetWindow() = window
        member _.SetScroll(value) = scroll <- value
        member _.GetScroll() = scroll
        member _.SetWidth(value) = width <- Some value
        member _.GetWidth() = width
        member _.SetPan(value) = pan <- value
        member _.GetPan() = pan
        member _.SetRecolor(value) = recolor <- Some value
        member _.GetRecolor() = recolor
        member _.SetPalette(values) = palette <- Some values
        member _.GetPalette() = palette
        member _.SetCursor(x, y) = cursor <- x, y
        member _.GetCursor() = cursor
        member _.SetCharacterSize(width, height) = characterSize <- width, height
        member _.GetCharacterSize() = characterSize
        member _.SetInk(values: int list) = ink <- values
        member _.SetPaper(value: int) = paper <- value
        member _.SetBorder(value: int) = border <- value
    member this.ApplyMode(mode: ScreenModeInfo) =
        resetForMode mode

type private DefaultFileChannel(id: ChannelId, path: string, mode: FileOpenMode) =
    let sharedStream, reader, writer =
        match mode with
        | OpenForInput ->
            None, Some(new StreamReader(File.Open(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))), None
        | OpenForOutput ->
            None, None, Some(new StreamWriter(File.Open(path, FileMode.Create, FileAccess.Write, FileShare.Read)))
        | OpenForUpdate ->
            let stream = File.Open(path, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read)
            Some stream, Some(new StreamReader(stream)), Some(new StreamWriter(stream))

    interface IChannel with
        member _.Id = id
        member _.Kind = FileChannel
        member _.WriteText text =
            match writer with
            | Some streamWriter ->
                streamWriter.WriteLine(text)
                streamWriter.Flush()
            | None -> ()
        member _.ReadText() =
            match reader with
            | Some streamReader when not streamReader.EndOfStream -> Some(streamReader.ReadLine())
            | _ -> None
        member _.IsEndOfFile() =
            match reader with
            | Some streamReader -> streamReader.EndOfStream
            | None -> false
        member _.Flush() =
            match writer with
            | Some streamWriter -> streamWriter.Flush()
            | None -> ()
        member _.Close() =
            match sharedStream with
            | Some stream ->
                stream.Dispose()
            | None ->
                match reader with
                | Some streamReader -> streamReader.Dispose()
                | None -> ()

                match writer with
                | Some streamWriter -> streamWriter.Dispose()
                | None -> ()

type private DefaultChannelManager(defaultChannels: IChannel list, reader: unit -> string option, writer: string -> unit, currentMode: unit -> ScreenModeInfo) as this =
    let channels = Dictionary<ChannelId, IChannel>()
    let fixedChannels = HashSet<ChannelId>()
    let screenChannels = Dictionary<ChannelId, DefaultScreenChannel>()

    do
        for channel in defaultChannels do
            channels[channel.Id] <- channel
            fixedChannels.Add(channel.Id) |> ignore
            match channel with
            | :? DefaultScreenChannel as screenChannel -> screenChannels[channel.Id] <- screenChannel
            | _ -> ()

    let tryResolveDirectoryBackedPath (normalized: string) =
        let knownPrefixes = [ "RAM"; "FLP"; "WIN"; "MDV" ]
        let prefixMatch =
            knownPrefixes
            |> List.tryPick (fun prefix ->
                if normalized.StartsWith(prefix) then
                    let suffix = normalized.Substring(prefix.Length)
                    let digitCount = suffix |> Seq.takeWhile Char.IsDigit |> Seq.length
                    if digitCount > 0 && suffix.Length > digitCount && suffix[digitCount] = '_' then
                        let device = normalized.Substring(0, prefix.Length + digitCount)
                        let rest = normalized.Substring(prefix.Length + digitCount + 1)
                        if String.IsNullOrWhiteSpace rest then None else Some(device, rest)
                    else
                        None
                else
                    None)

        prefixMatch
        |> Option.map (fun (device, leafName) ->
            let root = Path.Combine(Directory.GetCurrentDirectory(), "RuntimeDevices", device.ToLowerInvariant())
            Directory.CreateDirectory(root) |> ignore
            Path.Combine(root, leafName))

    let createNamedChannel (requestedId: ChannelId) (name: string) (fileModeOverride: FileOpenMode option) =
        let normalized = name.Trim().ToUpperInvariant()

        if normalized.StartsWith("CON") then
            let channel = DefaultScreenChannel(requestedId, ConsoleChannel, reader, writer, currentMode())
            Result.Ok(channel :> IChannel)
        elif normalized.StartsWith("SCR") then
            let channel = DefaultScreenChannel(requestedId, ScreenChannel, reader, writer, currentMode())
            Result.Ok(channel :> IChannel)
        elif normalized.StartsWith("NUL") then
            Result.Ok(NullChannel(requestedId) :> IChannel)
        elif normalized.StartsWith("PRT") then
            Result.Ok(DefaultPrinterChannel(requestedId, writer) :> IChannel)
        else
            match tryResolveDirectoryBackedPath normalized with
            | Some fullPath ->
                try
                    let mode = fileModeOverride |> Option.defaultValue OpenForUpdate
                    let channel = DefaultFileChannel(requestedId, fullPath, mode) :> IChannel
                    Result.Ok channel
                with
                | ex -> Result.Error(DeviceOpenFailed ex.Message)
            | None ->
                Result.Error(UnsupportedHostOperation $"Channel '{name}' is not supported by DefaultHost.")

    member _.TryGet(channelId: ChannelId) =
        match channels.TryGetValue channelId with
        | true, channel -> Some channel
        | false, _ -> None

    member _.Register(channel: IChannel) =
        channels[channel.Id] <- channel
        match channel with
        | :? DefaultScreenChannel as screenChannel -> screenChannels[channel.Id] <- screenChannel
        | _ -> ()
        Result.Ok()

    member _.ApplyMode(mode: ScreenModeInfo) =
        for pair in screenChannels do
            pair.Value.ApplyMode(mode)

    member _.OpenResolved(requestedId: ChannelId, name: string, fileModeOverride: FileOpenMode option) =
        match channels.TryGetValue requestedId with
        | true, _ -> Result.Error(InvalidHostArgument $"Channel #{requestedId} already exists.")
        | false, _ ->
            createNamedChannel requestedId name fileModeOverride
            |> Result.map (fun channel -> channels[requestedId] <- channel)

    interface IChannelManager with
        member _.Open(_name: string) =
            Result.Error(UnsupportedHostOperation "Dynamic channel opening is not implemented in DefaultHost.")

        member _.OpenAs(requestedId: ChannelId, name: string) =
            this.OpenResolved(requestedId, name, None)

        member _.Get(channelId: ChannelId) =
            match channels.TryGetValue channelId with
            | true, channel -> Result.Ok channel
            | false, _ -> Result.Error(ChannelNotFound channelId)

        member _.Close(channelId: ChannelId) =
            match channels.TryGetValue channelId with
            | true, channel ->
                if fixedChannels.Contains(channelId) then
                    Result.Error(InvalidHostArgument $"Channel #{channelId} is a default channel and cannot be closed.")
                else
                    channel.Close()
                    channels.Remove(channelId) |> ignore
                    screenChannels.Remove(channelId) |> ignore
                    Result.Ok()
            | false, _ -> Result.Error(ChannelNotFound channelId)

type private DefaultScreenDevice(writer: string -> unit) =
    let mutable window = 0, 0, 0, 0
    let mutable scroll = 0
    let mutable width = None
    let mutable pan = 0
    let mutable recolor = None
    let mutable palette = None
    let mutable cursor = 0, 0
    let mutable characterSize = 0, 0
    let mutable ink = [ 7 ]
    let mutable mode = ScreenDefaults.supportedModes[0]

    let resetForMode selectedMode =
        window <- ScreenDefaults.defaultWindowForMode selectedMode
        scroll <- 0
        width <- None
        pan <- 0
        recolor <- None
        palette <- None
        cursor <- 0, 0
        characterSize <- ScreenDefaults.defaultCharacterSizeForMode selectedMode
        ink <- [ 7 ]

    do
        resetForMode mode

    interface IScreenDevice with
        member _.Clear() = ()
        member _.SetWindow(width, height, x, y) = window <- width, height, x, y
        member _.GetWindow() = window
        member _.SetScroll(value) = scroll <- value
        member _.GetScroll() = scroll
        member _.SetWidth(value) = width <- Some value
        member _.GetWidth() = width
        member _.SetPan(value) = pan <- value
        member _.GetPan() = pan
        member _.SetRecolor(value) = recolor <- Some value
        member _.GetRecolor() = recolor
        member _.SetPalette(values) = palette <- Some values
        member _.GetPalette() = palette
        member _.SetCursor(x, y) = cursor <- x, y
        member _.GetCursor() = cursor
        member _.SetCharacterSize(width, height) = characterSize <- width, height
        member _.GetCharacterSize() = characterSize
        member _.WriteText text = writer text
        member _.SetInk(values: int list) = ink <- values
        member _.SetPaper(_value: int) = ()
        member _.SetBorder(_value: int) = ()
        member _.GetSupportedModes() = ScreenDefaults.supportedModes
        member _.GetMode() = mode
        member _.SetMode requestedMode =
            match ScreenDefaults.supportedModes |> List.tryFind (fun candidate -> candidate.Mode = requestedMode) with
            | Some selected ->
                mode <- selected
                resetForMode selected
                Result.Ok()
            | None ->
                Result.Error(UnsupportedHostOperation $"Screen mode '{requestedMode}' is not supported by DefaultHost.")

type private DefaultGraphicsDevice() =
    let mutable cursor = 0.0, 0.0
    let mutable ink = [ 7 ]
    let mutable fillMode = 0
    let mutable scale = 0.0, 0.0, 0.0
    let mutable overMode = 0
    let mutable underMode = 0
    let mutable flashMode = 0
    let mutable penDown = true
    let mutable heading = 0.0

    interface IGraphicsDevice with
        member _.Plot(x, y) = cursor <- x, y
        member _.Point(x, y) = cursor <- x, y
        member _.PointRelative(dx, dy) =
            let x, y = cursor
            cursor <- x + dx, y + dy
        member _.Draw(x, y) = cursor <- x, y
        member _.LineRelative(values) =
            let pairs =
                values
                |> List.chunkBySize 2
                |> List.choose (function
                    | [ x; y ] -> Some(x, y)
                    | _ -> None)

            match List.tryLast pairs with
            | Some(dx, dy) ->
                let x, y = cursor
                cursor <- x + dx, y + dy
            | None -> ()
        member _.DLine(values) =
            let coordinates =
                values
                |> List.chunkBySize 2
                |> List.choose (function
                    | [ x; y ] -> Some(x, y)
                    | _ -> None)

            match List.tryLast coordinates with
            | Some(x, y) -> cursor <- x, y
            | None -> ()
        member _.Line(_x1, _y1, x2, y2) = cursor <- x2, y2
        member _.Circle(_x, _y, _radius) = ()
        member _.CircleRelative(dx, dy, _radius) =
            let x, y = cursor
            cursor <- x + dx, y + dy
        member _.Ellipse(_x, _y, _radius, _ratio, _angle) = ()
        member _.EllipseRelative(dx, dy, _radius, _ratio, _angle) =
            let x, y = cursor
            cursor <- x + dx, y + dy
        member _.Arc(_x, _y, _radius, _startAngle, _endAngle) = ()
        member _.ArcRelative(dx, dy, _radius, _startAngle, _endAngle) =
            let x, y = cursor
            cursor <- x + dx, y + dy
        member _.Block(_width, _height, x, y, _color) = cursor <- x, y
        member _.SetInk(values: int list) = ink <- values
        member _.SetFill(value: int) = fillMode <- value
        member _.SetScale(x, y, z) = scale <- x, y, z
        member _.SetOver(value: int) = overMode <- value
        member _.SetUnder(value: int) = underMode <- value
        member _.SetFlash(value: int) = flashMode <- value
        member _.SetPenDown(value: bool) = penDown <- value
        member _.Turn(angle) = heading <- heading + angle
        member _.TurnTo(angle) = heading <- angle
        member _.Clear() = ()

type private DefaultInputDevice(reader: unit -> string option, readKey: unit -> KeyInfo option, keyAvailable: unit -> bool, keyRowState: int -> int) =
    interface IInputDevice with
        member _.ReadLine() = reader ()
        member _.ReadKey() = readKey ()
        member _.KeyAvailable() = keyAvailable ()
        member _.GetKeyRow(row) = keyRowState row

type private NullSoundDevice() =
    interface ISoundDevice with
        member _.Beep(_pitch, _duration) = ()

type private NullFileSystem() =
    interface IDeviceFileSystem with
        member _.OpenFile(_path, _mode) =
            Result.Error(UnsupportedHostOperation "File channels are not implemented in DefaultHost.")
        member _.OpenFileAs(_channelId, _path, _mode) =
            Result.Error(UnsupportedHostOperation "File channels are not implemented in DefaultHost.")
        member _.Exists(_path) = false
        member _.Delete(_path) =
            Result.Error(UnsupportedHostOperation "File deletion is not implemented in DefaultHost.")

type private DefaultRuntimeHost(options: DefaultHostOptions) =
    let mutable currentMode = ScreenDefaults.supportedModes[0]
    let screen = DefaultScreenDevice(options.WriteLine) :> IScreenDevice
    let channelManager =
        [ DefaultScreenChannel(ChannelId 0, ChannelKind.ConsoleChannel, options.ReadLine, options.WriteLine, currentMode) :> IChannel
          DefaultScreenChannel(ChannelId 1, ChannelKind.ScreenChannel, options.ReadLine, options.WriteLine, currentMode) :> IChannel
          DefaultScreenChannel(ChannelId 2, ChannelKind.ScreenChannel, options.ReadLine, options.WriteLine, currentMode) :> IChannel ]
        |> fun defaults -> DefaultChannelManager(defaults, options.ReadLine, options.WriteLine, fun () -> currentMode)
    let graphics = DefaultGraphicsDevice() :> IGraphicsDevice
    let input = DefaultInputDevice(options.ReadLine, options.ReadKey, options.KeyAvailable, options.KeyRowState) :> IInputDevice
    let sound = NullSoundDevice() :> ISoundDevice
    let files =
        { new IDeviceFileSystem with
            member _.OpenFile(_path, _mode) =
                Result.Error(UnsupportedHostOperation "DefaultHost requires an explicit channel id for file open.")
            member _.OpenFileAs(channelId, path, mode) =
                let trimmed = path.Trim()
                let looksLikeDevice =
                    trimmed.Contains("_") && not (trimmed.Contains(Path.DirectorySeparatorChar) || trimmed.Contains(Path.AltDirectorySeparatorChar) || Path.IsPathRooted(trimmed))

                if looksLikeDevice then
                    channelManager.OpenResolved(channelId, trimmed, Some mode)
                else
                    match channelManager.TryGet channelId with
                    | Some _ -> Result.Error(InvalidHostArgument $"Channel #{channelId} already exists.")
                    | None ->
                        try
                            let fullPath = Path.GetFullPath(path)
                            let channel = DefaultFileChannel(channelId, fullPath, mode) :> IChannel
                            channelManager.Register(channel)
                        with
                        | ex -> Result.Error(DeviceOpenFailed ex.Message)
            member _.Exists(path) = File.Exists(path)
            member _.Delete(path) =
                try
                    File.Delete(path)
                    Result.Ok()
                with
                | ex -> Result.Error(DeviceOpenFailed ex.Message) }
    let environment =
        { new IEnvironmentProvider with
            member _.GetVariable(name) =
                let normalized = name.Trim().ToUpperInvariant()
                let tryNames names =
                    names
                    |> List.tryPick (fun candidate -> System.Environment.GetEnvironmentVariable(candidate) |> Option.ofObj)

                match normalized with
                | "PROG_USE" -> Some(Directory.GetCurrentDirectory())
                | "HOME" -> tryNames [ "HOME"; "USERPROFILE" ]
                | "TMPDIR"
                | "TEMP_USE" -> tryNames [ normalized; "TEMP"; "TMP" ]
                | "USER" -> tryNames [ "USER"; "USERNAME" ]
                | _ ->
                    match System.Environment.GetEnvironmentVariable(name) |> Option.ofObj with
                    | Some value -> Some value
                    | None -> tryNames [ normalized ] }

    interface IRuntimeHost with
        member _.Channels = channelManager :> IChannelManager
        member _.Screen =
            { new IScreenDevice with
                member _.Clear() = screen.Clear()
                member _.SetWindow(width, height, x, y) = screen.SetWindow(width, height, x, y)
                member _.GetWindow() = screen.GetWindow()
                member _.SetScroll(value) = screen.SetScroll(value)
                member _.GetScroll() = screen.GetScroll()
                member _.SetWidth(value) = screen.SetWidth(value)
                member _.GetWidth() = screen.GetWidth()
                member _.SetPan(value) = screen.SetPan(value)
                member _.GetPan() = screen.GetPan()
                member _.SetRecolor(value) = screen.SetRecolor(value)
                member _.GetRecolor() = screen.GetRecolor()
                member _.SetPalette(values) = screen.SetPalette(values)
                member _.GetPalette() = screen.GetPalette()
                member _.SetCursor(x, y) = screen.SetCursor(x, y)
                member _.GetCursor() = screen.GetCursor()
                member _.SetCharacterSize(width, height) = screen.SetCharacterSize(width, height)
                member _.GetCharacterSize() = screen.GetCharacterSize()
                member _.WriteText(text) = screen.WriteText(text)
                member _.SetInk(values) = screen.SetInk(values)
                member _.SetPaper(value) = screen.SetPaper(value)
                member _.SetBorder(value) = screen.SetBorder(value)
                member _.GetSupportedModes() = screen.GetSupportedModes()
                member _.GetMode() = screen.GetMode()
                member _.SetMode(requestedMode) =
                    match screen.SetMode(requestedMode) with
                    | Result.Ok() ->
                        currentMode <- screen.GetMode()
                        channelManager.ApplyMode(currentMode)
                        Result.Ok()
                    | Result.Error err -> Result.Error err }
        member _.Graphics = graphics
        member _.Input = input
        member _.Sound = sound
        member _.Files = files
        member _.Environment = environment

module DefaultHost =
    let create options : IRuntimeHost =
        DefaultRuntimeHost(options) :> IRuntimeHost
