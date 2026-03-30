namespace SBRuntime

// Host abstraction interfaces for channels, screen, graphics, input, sound, and files.

type ChannelId = ChannelId of int

type ChannelKind =
    | ConsoleChannel
    | ScreenChannel
    | FileChannel
    | PrinterChannel
    | PipeChannel
    | NamedChannel of string

type FileOpenMode =
    | OpenForInput
    | OpenForOutput
    | OpenForUpdate

type KeyInfo = {
    KeyCode: int
    Character: char option
    Shift: bool
    Control: bool
}

type RuntimeHostError =
    | ChannelNotFound of ChannelId
    | DeviceOpenFailed of string
    | UnsupportedHostOperation of string
    | InvalidHostArgument of string

type IChannel =
    abstract Id: ChannelId
    abstract Kind: ChannelKind
    abstract WriteText: string -> unit
    abstract ReadText: unit -> string option
    abstract Flush: unit -> unit
    abstract Close: unit -> unit

type IChannelManager =
    abstract Open: string -> Result<ChannelId, RuntimeHostError>
    abstract Get: ChannelId -> Result<IChannel, RuntimeHostError>
    abstract Close: ChannelId -> Result<unit, RuntimeHostError>

type IScreenDevice =
    abstract Clear: unit -> unit
    abstract SetCursor: int * int -> unit
    abstract GetCursor: unit -> int * int
    abstract WriteText: string -> unit
    abstract SetInk: int -> unit
    abstract SetPaper: int -> unit
    abstract SetBorder: int -> unit

type IGraphicsDevice =
    abstract Plot: int * int -> unit
    abstract Draw: int * int -> unit
    abstract Line: int * int * int * int -> unit
    abstract Circle: int * int * int -> unit
    abstract Fill: int * int -> unit
    abstract Clear: unit -> unit

type IInputDevice =
    abstract ReadLine: unit -> string option
    abstract ReadKey: unit -> KeyInfo option
    abstract KeyAvailable: unit -> bool

type ISoundDevice =
    abstract Beep: int * int -> unit

type IDeviceFileSystem =
    abstract OpenFile: string * FileOpenMode -> Result<ChannelId, RuntimeHostError>
    abstract Exists: string -> bool
    abstract Delete: string -> Result<unit, RuntimeHostError>

type IRuntimeHost =
    abstract Channels: IChannelManager
    abstract Screen: IScreenDevice
    abstract Graphics: IGraphicsDevice
    abstract Input: IInputDevice
    abstract Sound: ISoundDevice
    abstract Files: IDeviceFileSystem
