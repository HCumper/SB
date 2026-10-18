module SSBReverse

open System
open System.Text.RegularExpressions
open System.Collections.Generic

module Reverse =

    type FatalReverseError(message: string) =
        inherit Exception(message)

    type NumberedLine =
        { Number : int
          Text : string
          Original : string }

    type ReverseConfig =
        { LabelPrefix : string
          PreserveColonBlankLines : bool }

    let defaultConfig =
        { LabelPrefix = "@L"
          PreserveColonBlankLines = false }

    let private numberedLineRegex =
        Regex(@"^\s*(\d+)\s(.*)$", RegexOptions.Compiled)

    // Match GO TO <number> or GOTO <number>
    let private gotoRegex =
        Regex(@"(?i)\b(GO\s*TO|GOTO)\s+(\d+)\b", RegexOptions.Compiled)

    // Match GO SUB <number> or GOSUB <number>
    let private gosubRegex =
        Regex(@"(?i)\b(GO\s*SUB|GOSUB)\s+(\d+)\b", RegexOptions.Compiled)

    let private parseNumberedLine (line: string) =
        let m = numberedLineRegex.Match(line)
        if not m.Success then
            raise (FatalReverseError(sprintf "Not a numbered line: %s" line))

        let n = Int32.Parse(m.Groups.[1].Value)
        let text = m.Groups.[2].Value

        { Number = n
          Text = text
          Original = line }

    let private tryParseNumberedLine (line: string) =
        let m = numberedLineRegex.Match(line)
        if not m.Success then
            None
        else
            let n = Int32.Parse(m.Groups.[1].Value)
            let text = m.Groups.[2].Value
            Some
                { Number = n
                  Text = text
                  Original = line }

    let private collectTargetsFromText (text: string) =
        seq {
            for m in gotoRegex.Matches(text) do
                yield Int32.Parse(m.Groups.[2].Value)

            for m in gosubRegex.Matches(text) do
                yield Int32.Parse(m.Groups.[2].Value)
        }
        |> Seq.distinct
        |> Seq.toList

    let private buildSyntheticLabel (cfg: ReverseConfig) (lineNumber: int) =
        $"{cfg.LabelPrefix}{lineNumber}"

    let private rewriteTargets (cfg: ReverseConfig) (text: string) =
        let replaceWithLabel (rx: Regex) (input: string) =
            rx.Replace(
                input,
                MatchEvaluator(fun m ->
                    let keyword = m.Groups.[1].Value
                    let target = Int32.Parse(m.Groups.[2].Value)
                    let label = buildSyntheticLabel cfg target
                    $"{keyword} {label}")
            )

        text
        |> replaceWithLabel gotoRegex
        |> replaceWithLabel gosubRegex

    let private isSyntheticBlankLine (text: string) =
        text = ":"

    let reverseLinesWith (cfg: ReverseConfig) (lines: seq<string>) =
        let parsed =
            lines
            |> Seq.filter (fun s -> not (String.IsNullOrWhiteSpace s))
            |> Seq.map parseNumberedLine
            |> Seq.toList

        let existingLineNumbers =
            parsed |> Seq.map (fun x -> x.Number) |> Set.ofSeq

        let referencedTargets =
            parsed
            |> Seq.collect (fun line -> collectTargetsFromText line.Text)
            |> Set.ofSeq

        let validTargets =
            Set.intersect existingLineNumbers referencedTargets

        let labelMap =
            validTargets
            |> Seq.map (fun n -> n, buildSyntheticLabel cfg n)
            |> dict

        let output = ResizeArray<string>()

        for line in parsed do
            if labelMap.ContainsKey(line.Number) then
                output.Add(labelMap.[line.Number])

            let rewritten = rewriteTargets cfg line.Text

            if isSyntheticBlankLine rewritten then
                if cfg.PreserveColonBlankLines then
                    output.Add("")
            else
                output.Add(rewritten)

        output |> Seq.toList

    let reverseTextWith (cfg: ReverseConfig) (numberedSource: string) =
        let lines =
            numberedSource.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n')

        reverseLinesWith cfg lines
        |> String.concat Environment.NewLine

    let reverseText (numberedSource: string) =
        reverseTextWith defaultConfig numberedSource

    // Useful if you want both the reconstructed source and the map.
    type ReverseResult =
        { Source : string
          LabelMap : (int * string) list
          MissingTargets : int list }

    let reverseTextDetailedWith (cfg: ReverseConfig) (numberedSource: string) =
        let lines =
            numberedSource.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n')

        let parsed =
            lines
            |> Seq.filter (fun s -> not (String.IsNullOrWhiteSpace s))
            |> Seq.choose tryParseNumberedLine
            |> Seq.toList

        let existingLineNumbers =
            parsed |> Seq.map (fun x -> x.Number) |> Set.ofSeq

        let allTargets =
            parsed
            |> Seq.collect (fun line -> collectTargetsFromText line.Text)
            |> Set.ofSeq

        let validTargets =
            Set.intersect existingLineNumbers allTargets

        let missingTargets =
            Set.difference allTargets existingLineNumbers
            |> Set.toList
            |> List.sort

        let labelMap =
            validTargets
            |> Seq.map (fun n -> n, buildSyntheticLabel cfg n)
            |> Seq.toList
            |> List.sortBy fst

        let labelDict = dict labelMap

        let output = ResizeArray<string>()

        for line in parsed do
            if labelDict.ContainsKey(line.Number) then
                output.Add(labelDict.[line.Number])

            let rewritten = rewriteTargets cfg line.Text

            if isSyntheticBlankLine rewritten then
                if cfg.PreserveColonBlankLines then
                    output.Add("")
            else
                output.Add(rewritten)

        { Source = String.concat Environment.NewLine output
          LabelMap = labelMap
          MissingTargets = missingTargets }

    let reverseTextDetailed (numberedSource: string) =
        reverseTextDetailedWith defaultConfig numberedSource