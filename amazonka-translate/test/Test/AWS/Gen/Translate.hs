{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Translate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Translate where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Translate
import Test.AWS.Translate.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeTextTranslationJob $
--             describeTextTranslationJob
--
--         , requestListTerminologies $
--             listTerminologies
--
--         , requestGetTerminology $
--             getTerminology
--
--         , requestTranslateText $
--             translateText
--
--         , requestImportTerminology $
--             importTerminology
--
--         , requestStopTextTranslationJob $
--             stopTextTranslationJob
--
--         , requestDeleteTerminology $
--             deleteTerminology
--
--         , requestListTextTranslationJobs $
--             listTextTranslationJobs
--
--         , requestStartTextTranslationJob $
--             startTextTranslationJob
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTextTranslationJob $
--             describeTextTranslationJobResponse
--
--         , responseListTerminologies $
--             listTerminologiesResponse
--
--         , responseGetTerminology $
--             getTerminologyResponse
--
--         , responseTranslateText $
--             translateTextResponse
--
--         , responseImportTerminology $
--             importTerminologyResponse
--
--         , responseStopTextTranslationJob $
--             stopTextTranslationJobResponse
--
--         , responseDeleteTerminology $
--             deleteTerminologyResponse
--
--         , responseListTextTranslationJobs $
--             listTextTranslationJobsResponse
--
--         , responseStartTextTranslationJob $
--             startTextTranslationJobResponse
--
--           ]
--     ]

-- Requests

requestDescribeTextTranslationJob :: DescribeTextTranslationJob -> TestTree
requestDescribeTextTranslationJob = req
    "DescribeTextTranslationJob"
    "fixture/DescribeTextTranslationJob.yaml"

requestListTerminologies :: ListTerminologies -> TestTree
requestListTerminologies = req
    "ListTerminologies"
    "fixture/ListTerminologies.yaml"

requestGetTerminology :: GetTerminology -> TestTree
requestGetTerminology = req
    "GetTerminology"
    "fixture/GetTerminology.yaml"

requestTranslateText :: TranslateText -> TestTree
requestTranslateText = req
    "TranslateText"
    "fixture/TranslateText.yaml"

requestImportTerminology :: ImportTerminology -> TestTree
requestImportTerminology = req
    "ImportTerminology"
    "fixture/ImportTerminology.yaml"

requestStopTextTranslationJob :: StopTextTranslationJob -> TestTree
requestStopTextTranslationJob = req
    "StopTextTranslationJob"
    "fixture/StopTextTranslationJob.yaml"

requestDeleteTerminology :: DeleteTerminology -> TestTree
requestDeleteTerminology = req
    "DeleteTerminology"
    "fixture/DeleteTerminology.yaml"

requestListTextTranslationJobs :: ListTextTranslationJobs -> TestTree
requestListTextTranslationJobs = req
    "ListTextTranslationJobs"
    "fixture/ListTextTranslationJobs.yaml"

requestStartTextTranslationJob :: StartTextTranslationJob -> TestTree
requestStartTextTranslationJob = req
    "StartTextTranslationJob"
    "fixture/StartTextTranslationJob.yaml"

-- Responses

responseDescribeTextTranslationJob :: DescribeTextTranslationJobResponse -> TestTree
responseDescribeTextTranslationJob = res
    "DescribeTextTranslationJobResponse"
    "fixture/DescribeTextTranslationJobResponse.proto"
    translate
    (Proxy :: Proxy DescribeTextTranslationJob)

responseListTerminologies :: ListTerminologiesResponse -> TestTree
responseListTerminologies = res
    "ListTerminologiesResponse"
    "fixture/ListTerminologiesResponse.proto"
    translate
    (Proxy :: Proxy ListTerminologies)

responseGetTerminology :: GetTerminologyResponse -> TestTree
responseGetTerminology = res
    "GetTerminologyResponse"
    "fixture/GetTerminologyResponse.proto"
    translate
    (Proxy :: Proxy GetTerminology)

responseTranslateText :: TranslateTextResponse -> TestTree
responseTranslateText = res
    "TranslateTextResponse"
    "fixture/TranslateTextResponse.proto"
    translate
    (Proxy :: Proxy TranslateText)

responseImportTerminology :: ImportTerminologyResponse -> TestTree
responseImportTerminology = res
    "ImportTerminologyResponse"
    "fixture/ImportTerminologyResponse.proto"
    translate
    (Proxy :: Proxy ImportTerminology)

responseStopTextTranslationJob :: StopTextTranslationJobResponse -> TestTree
responseStopTextTranslationJob = res
    "StopTextTranslationJobResponse"
    "fixture/StopTextTranslationJobResponse.proto"
    translate
    (Proxy :: Proxy StopTextTranslationJob)

responseDeleteTerminology :: DeleteTerminologyResponse -> TestTree
responseDeleteTerminology = res
    "DeleteTerminologyResponse"
    "fixture/DeleteTerminologyResponse.proto"
    translate
    (Proxy :: Proxy DeleteTerminology)

responseListTextTranslationJobs :: ListTextTranslationJobsResponse -> TestTree
responseListTextTranslationJobs = res
    "ListTextTranslationJobsResponse"
    "fixture/ListTextTranslationJobsResponse.proto"
    translate
    (Proxy :: Proxy ListTextTranslationJobs)

responseStartTextTranslationJob :: StartTextTranslationJobResponse -> TestTree
responseStartTextTranslationJob = res
    "StartTextTranslationJobResponse"
    "fixture/StartTextTranslationJobResponse.proto"
    translate
    (Proxy :: Proxy StartTextTranslationJob)
