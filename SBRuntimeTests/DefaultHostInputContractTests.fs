module SBRuntimeTests.DefaultHostInputContractTests

open NUnit.Framework

open SBRuntimeTests.HostInputContract

[<TestCase(0)>]
[<TestCase(1)>]
let ``default host scripted input handles sequential prompt sessions across channels`` channelId =
    runQueuedSequentialPromptScenario channelId

[<TestCase(0, "Prompt", "ABC")>]
[<TestCase(1, "Enter", "ABC")>]
[<TestCase(2, "Enter", "ABC")>]
let ``default host scripted input handles prompt submissions across channels`` channelId promptText submittedText =
    runQueuedPromptSubmissionScenario channelId promptText submittedText

[<TestCase(0, "Prompt", "ABC")>]
[<TestCase(1, "Enter", "ABC")>]
let ``default host scripted input handles prequeued prompt submissions across channels`` channelId promptText submittedText =
    runQueuedPromptSubmissionPrequeuedScenario channelId promptText submittedText

[<TestCase(0)>]
[<TestCase(1)>]
let ``default host scripted input flush clears queued prompt input across channels`` channelId =
    runQueuedFlushScenario channelId

[<Test>]
let ``default host scripted input handles implicit prompt submission`` () =
    runQueuedImplicitPromptSubmissionScenario "Enter" "42"

[<Test>]
let ``default host scripted input honors positioned prompt placement`` () =
    runQueuedPositionedPromptScenario ()

[<Test>]
let ``default host scripted input flushes pending lines before a built in input`` () =
    runQueuedBuiltInFlushScenario ()

[<Test>]
let ``default host scripted input routes explicit screen channel input through the screen queue`` () =
    runQueuedExplicitScreenChannelScenario ()

[<TestCase(0, 10)>]
[<TestCase(1, 10)>]
let ``default host scripted input handles repeated prompt submissions across channels`` channelId iterations =
    runQueuedRepeatedPromptIterations channelId iterations
