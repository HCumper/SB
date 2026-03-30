namespace SBRuntime

open System.Collections.Generic

// Minimal default host implementation for console-style execution and tests.

type DefaultHostOptions = {
    ReadLine: unit -> string option
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
    let mutable cursor = 0, 0
    let mutable characterSize = 0, 0
    let mutable ink = 7
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
        member _.SetCursor(x, y) = cursor <- x, y
        member _.GetCursor() = cursor
        member _.SetCharacterSize(width, height) = characterSize <- width, height
        member _.GetCharacterSize() = characterSize
        member _.SetInk(value: int) = ink <- value
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
    let mutable cursor = 0, 0
    let mutable characterSize = 0, 0
    let mutable mode = supportedModes[0]

    interface IScreenDevice with
        member _.Clear() = ()
        member _.SetCursor(x, y) = cursor <- x, y
        member _.GetCursor() = cursor
        member _.SetCharacterSize(width, height) = characterSize <- width, height
        member _.GetCharacterSize() = characterSize
        member _.WriteText text = writer text
        member _.SetInk(_value: int) = ()
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

type private NullGraphicsDevice() =
    interface IGraphicsDevice with
        member _.Plot(_x, _y) = ()
        member _.Draw(_x, _y) = ()
        member _.Line(_x1, _y1, _x2, _y2) = ()
        member _.Circle(_x, _y, _radius) = ()
        member _.Fill(_x, _y) = ()
        member _.Clear() = ()

type private DefaultInputDevice(reader: unit -> string option) =
    interface IInputDevice with
        member _.ReadLine() = reader ()
        member _.ReadKey() = None
        member _.KeyAvailable() = false

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
    let graphics = NullGraphicsDevice() :> IGraphicsDevice
    let input = DefaultInputDevice(options.ReadLine) :> IInputDevice
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
