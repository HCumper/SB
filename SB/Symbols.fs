module Symbols

open System
open System.IO
open Antlr4.Runtime
open SB
open SBLib


type MyClassDerived2(source, symtype, channel, start, stop) =
    inherit CommonToken(source, symtype, channel, start, stop)
    let EvaluatedType = 7

type SBTokFactory (source, symtype, text, channel, start, stop, line, charPositionInLine) =
    MyClassDerived2 (source, symtype, channel, start, stop)

