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
    | OpenForAppend
    | OpenForUpdate

type ScreenMode =
    | QlMode4
    | QlMode8
    | ExtendedMode of int

type ScreenModeInfo = {
    Mode: ScreenMode
    Width: int
    Height: int
    Colors: int option
    Name: string
    IsQlCompatible: bool
    BaseTextCellWidth: int
    BaseTextCellHeight: int
    DefaultCharacterSize: int * int
}

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
    abstract IsEndOfFile: unit -> bool
    abstract Flush: unit -> unit
    abstract Close: unit -> unit

type IScreenChannel =
    inherit IChannel
    abstract Clear: unit -> unit
    abstract NewLine: unit -> unit
    abstract SetWindow: int * int * int * int -> unit
    abstract GetWindow: unit -> int * int * int * int
    abstract SetScroll: int -> unit
    abstract GetScroll: unit -> int
    abstract SetWidth: int -> unit
    abstract GetWidth: unit -> int option
    abstract SetPan: int -> unit
    abstract GetPan: unit -> int
    abstract SetRecolor: int list -> unit
    abstract GetRecolor: unit -> int list option
    abstract SetPalette: int list -> unit
    abstract GetPalette: unit -> int list option
    abstract SetCursor: int * int -> unit
    abstract GetCursor: unit -> int * int
    abstract SetCharacterSize: int * int -> unit
    abstract GetCharacterSize: unit -> int * int
    abstract SetCharacterFonts: int * int -> unit
    abstract GetCharacterFonts: unit -> int * int
    abstract SetInk: int list -> unit
    abstract SetPaper: int -> unit
    abstract SetStrip: int list -> unit
    abstract SetBorder: int * int option -> unit

type IChannelManager =
    abstract Open: string -> Result<ChannelId, RuntimeHostError>
    abstract OpenAs: ChannelId * string -> Result<unit, RuntimeHostError>
    abstract Get: ChannelId -> Result<IChannel, RuntimeHostError>
    abstract Close: ChannelId -> Result<unit, RuntimeHostError>

type IScreenDevice =
    abstract Clear: unit -> unit
    abstract NewLine: unit -> unit
    abstract SetWindow: int * int * int * int -> unit
    abstract GetWindow: unit -> int * int * int * int
    abstract SetScroll: int -> unit
    abstract GetScroll: unit -> int
    abstract SetWidth: int -> unit
    abstract GetWidth: unit -> int option
    abstract SetPan: int -> unit
    abstract GetPan: unit -> int
    abstract SetRecolor: int list -> unit
    abstract GetRecolor: unit -> int list option
    abstract SetPalette: int list -> unit
    abstract GetPalette: unit -> int list option
    abstract SetCursor: int * int -> unit
    abstract GetCursor: unit -> int * int
    abstract SetCharacterSize: int * int -> unit
    abstract GetCharacterSize: unit -> int * int
    abstract SetCharacterFonts: int * int -> unit
    abstract GetCharacterFonts: unit -> int * int
    abstract WriteText: string -> unit
    abstract SetInk: int list -> unit
    abstract SetPaper: int -> unit
    abstract SetStrip: int list -> unit
    abstract SetBorder: int * int option -> unit
    abstract GetSupportedModes: unit -> ScreenModeInfo list
    abstract GetMode: unit -> ScreenModeInfo
    abstract SetMode: ScreenMode -> Result<unit, RuntimeHostError>

type IGraphicsDevice =
    abstract SetDrawingContext: (int * int * int * int) * int * (double * double * double) -> unit
    abstract Plot: double * double -> unit
    abstract Point: double * double -> unit
    abstract PointRelative: double * double -> unit
    abstract Draw: double * double -> unit
    abstract Line: double * double * double * double -> unit
    abstract LineRelative: double list -> unit
    abstract DLine: double list -> unit
    abstract Circle: double * double * double -> unit
    abstract CircleRelative: double * double * double -> unit
    abstract Ellipse: double * double * double * double * double -> unit
    abstract EllipseRelative: double * double * double * double * double -> unit
    abstract Arc: double * double * double * double * double -> unit
    abstract ArcRelative: double * double * double * double * double -> unit
    abstract Block: double * double * double * double * int -> unit
    abstract SetInk: int list -> unit
    abstract SetFill: int -> unit
    abstract SetScale: double * double * double -> unit
    abstract SetOver: int -> unit
    abstract SetUnder: int -> unit
    abstract SetFlash: int -> unit
    abstract SetPenDown: bool -> unit
    abstract Turn: double -> unit
    abstract TurnTo: double -> unit
    abstract Clear: unit -> unit

type IInputDevice =
    abstract ReadLine: unit -> string option
    abstract ReadKey: unit -> KeyInfo option
    abstract KeyAvailable: unit -> bool
    abstract GetKeyRow: int -> int
    abstract Flush: unit -> unit

type ISoundDevice =
    abstract Beep: int * int -> unit
    abstract IsBeeping: unit -> bool

type IDeviceFileSystem =
    abstract OpenFile: string * FileOpenMode -> Result<ChannelId, RuntimeHostError>
    abstract OpenFileAs: ChannelId * string * FileOpenMode -> Result<unit, RuntimeHostError>
    abstract ListDirectory: string option -> Result<string list, RuntimeHostError>
    abstract Exists: string -> bool
    abstract Delete: string -> Result<unit, RuntimeHostError>

type IEnvironmentProvider =
    abstract GetVariable: string -> string option

type IMemoryDevice =
    abstract Peek8: int -> Result<int, RuntimeHostError>
    abstract Peek16: int -> Result<int, RuntimeHostError>
    abstract Peek32: int -> Result<int, RuntimeHostError>
    abstract Poke8: int * int -> Result<unit, RuntimeHostError>
    abstract Poke16: int * int -> Result<unit, RuntimeHostError>
    abstract Poke32: int * int -> Result<unit, RuntimeHostError>

type ScreenTextCell = {
    Character: char
    CodePoint: int
    Ink: int
    Paper: int
    Strip: int
    HasBackground: bool
}

type ScreenPaneSnapshot = {
    ChannelId: int option
    Title: string
    Kind: ChannelKind
    Window: int * int * int * int
    OuterWindow: int * int * int * int
    Cursor: int * int
    CharacterSize: int * int
    Ink: int
    Paper: int
    Strip: int
    BorderSize: int
    BorderColor: int option
    Recolor: int list option
    Palette: int list option
    Text: ScreenTextCell[,]
    Pixels: int[,]
    Surface: int[,]
}

type ScreenDisplaySnapshot = {
    Mode: ScreenModeInfo
    Panes: ScreenPaneSnapshot list
}

type IDisplaySurface =
    abstract GetSnapshot: unit -> ScreenDisplaySnapshot

type IRuntimeHost =
    abstract Channels: IChannelManager
    abstract Screen: IScreenDevice
    abstract Graphics: IGraphicsDevice
    abstract Input: IInputDevice
    abstract Sound: ISoundDevice
    abstract Files: IDeviceFileSystem
    abstract Environment: IEnvironmentProvider
    abstract Memory: IMemoryDevice
