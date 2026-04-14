module SBRuntimeTests.DefaultHostInputContractTests

open NUnit.Framework

open SBRuntimeTests.HostInputContract

[<TestCase(0)>]
[<TestCase(1)>]
let ``default host scripted input handles sequential prompt sessions across channels`` channelId =
    runQueuedSequentialPromptScenario channelId

[<TestCase(0)>]
[<TestCase(1)>]
let ``default host scripted input flush clears queued prompt input across channels`` channelId =
    runQueuedFlushScenario channelId
