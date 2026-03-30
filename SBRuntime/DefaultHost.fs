namespace SBRuntime

open System.Collections.Generic

// Minimal default host implementation for console-style execution and tests.

type DefaultHostOptions = {
    ReadLine: unit -> string option
    ReadKey: unit -> KeyInfo option
    KeyAvailable: unit -> bool
    WriteLine: string -> unit
}

type private DefaultChannel(id: ChannelId, kind: ChannelKind, reader: unit -> string option, writer: string -> unit) =
    interface IChannel with
        member _.Id = id
        member _.Kind = kind
        member _.WriteText text = writer text
        member _.ReadText() = reader ()
        member _.Flush() = ()
        member _.Close() = ()

type private DefaultScreenChannel(id: ChannelId, kind: ChannelKind, reader: unit -> string option, writer: string -> unit) =
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

    interface IScreenChannel with
        member _.Id = id
        member _.Kind = kind
        member _.WriteText text = writer text
        member _.ReadText() = reader ()
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

type private DefaultChannelManager(defaultChannels: IChannel list) =
    let channels = Dictionary<ChannelId, IChannel>()

    do
        for channel in defaultChannels do
            channels[channel.Id] <- channel

    interface IChannelManager with
        member _.Open(_name: string) =
            Result.Error(UnsupportedHostOperation "Dynamic channel opening is not implemented in DefaultHost.")

        member _.Get(channelId: ChannelId) =
            match channels.TryGetValue channelId with
            | true, channel -> Result.Ok channel
            | false, _ -> Result.Error(ChannelNotFound channelId)

        member _.Close(channelId: ChannelId) =
            match channels.TryGetValue channelId with
            | true, channel ->
                channel.Close()
                Result.Ok()
            | false, _ -> Result.Error(ChannelNotFound channelId)

type private DefaultScreenDevice(writer: string -> unit) =
    let supportedModes =
        [ { Mode = QlMode4; Width = 512; Height = 256; Colors = Some 4; Name = "QL Mode 4"; IsQlCompatible = true }
          { Mode = QlMode8; Width = 256; Height = 256; Colors = Some 8; Name = "QL Mode 8"; IsQlCompatible = true }
          { Mode = ExtendedMode 256; Width = 256; Height = 256; Colors = Some 8; Name = "Extended Mode 256"; IsQlCompatible = false }
          { Mode = ExtendedMode 512; Width = 512; Height = 256; Colors = Some 4; Name = "Extended Mode 512"; IsQlCompatible = false } ]
    let mutable window = 0, 0, 0, 0
    let mutable scroll = 0
    let mutable width = None
    let mutable pan = 0
    let mutable recolor = None
    let mutable palette = None
    let mutable cursor = 0, 0
    let mutable characterSize = 0, 0
    let mutable ink = [ 7 ]
    let mutable mode = supportedModes[0]

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
        member _.GetSupportedModes() = supportedModes
        member _.GetMode() = mode
        member _.SetMode requestedMode =
            match supportedModes |> List.tryFind (fun candidate -> candidate.Mode = requestedMode) with
            | Some selected ->
                mode <- selected
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

type private DefaultInputDevice(reader: unit -> string option, readKey: unit -> KeyInfo option, keyAvailable: unit -> bool) =
    interface IInputDevice with
        member _.ReadLine() = reader ()
        member _.ReadKey() = readKey ()
        member _.KeyAvailable() = keyAvailable ()

type private NullSoundDevice() =
    interface ISoundDevice with
        member _.Beep(_pitch, _duration) = ()

type private NullFileSystem() =
    interface IDeviceFileSystem with
        member _.OpenFile(_path, _mode) =
            Result.Error(UnsupportedHostOperation "File channels are not implemented in DefaultHost.")

        member _.Exists(_path) = false
        member _.Delete(_path) =
            Result.Error(UnsupportedHostOperation "File deletion is not implemented in DefaultHost.")

type private DefaultRuntimeHost(options: DefaultHostOptions) =
    let channels =
        [ DefaultScreenChannel(ChannelId 0, ChannelKind.ConsoleChannel, options.ReadLine, options.WriteLine) :> IChannel
          DefaultScreenChannel(ChannelId 1, ChannelKind.ScreenChannel, options.ReadLine, options.WriteLine) :> IChannel
          DefaultScreenChannel(ChannelId 2, ChannelKind.ScreenChannel, options.ReadLine, options.WriteLine) :> IChannel ]
        |> DefaultChannelManager
        :> IChannelManager
    let screen = DefaultScreenDevice(options.WriteLine) :> IScreenDevice
    let graphics = DefaultGraphicsDevice() :> IGraphicsDevice
    let input = DefaultInputDevice(options.ReadLine, options.ReadKey, options.KeyAvailable) :> IInputDevice
    let sound = NullSoundDevice() :> ISoundDevice
    let files = NullFileSystem() :> IDeviceFileSystem

    interface IRuntimeHost with
        member _.Channels = channels
        member _.Screen = screen
        member _.Graphics = graphics
        member _.Input = input
        member _.Sound = sound
        member _.Files = files

module DefaultHost =
    let create options : IRuntimeHost =
        DefaultRuntimeHost(options) :> IRuntimeHost
