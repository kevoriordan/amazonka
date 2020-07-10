{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UploadType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.UploadType (
  UploadType (
    ..
    , AndroidApp
    , AppiumJavaJunitTestPackage
    , AppiumJavaJunitTestSpec
    , AppiumJavaTestngTestPackage
    , AppiumJavaTestngTestSpec
    , AppiumNodeTestPackage
    , AppiumNodeTestSpec
    , AppiumPythonTestPackage
    , AppiumPythonTestSpec
    , AppiumRubyTestPackage
    , AppiumRubyTestSpec
    , AppiumWebJavaJunitTestPackage
    , AppiumWebJavaJunitTestSpec
    , AppiumWebJavaTestngTestPackage
    , AppiumWebJavaTestngTestSpec
    , AppiumWebNodeTestPackage
    , AppiumWebNodeTestSpec
    , AppiumWebPythonTestPackage
    , AppiumWebPythonTestSpec
    , AppiumWebRubyTestPackage
    , AppiumWebRubyTestSpec
    , CalabashTestPackage
    , ExternalData
    , InstrumentationTestPackage
    , InstrumentationTestSpec
    , IosApp
    , UiautomationTestPackage
    , UiautomatorTestPackage
    , WebApp
    , XctestTestPackage
    , XctestUiTestPackage
    , XctestUiTestSpec
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UploadType = UploadType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern AndroidApp :: UploadType
pattern AndroidApp = UploadType' "ANDROID_APP"

pattern AppiumJavaJunitTestPackage :: UploadType
pattern AppiumJavaJunitTestPackage = UploadType' "APPIUM_JAVA_JUNIT_TEST_PACKAGE"

pattern AppiumJavaJunitTestSpec :: UploadType
pattern AppiumJavaJunitTestSpec = UploadType' "APPIUM_JAVA_JUNIT_TEST_SPEC"

pattern AppiumJavaTestngTestPackage :: UploadType
pattern AppiumJavaTestngTestPackage = UploadType' "APPIUM_JAVA_TESTNG_TEST_PACKAGE"

pattern AppiumJavaTestngTestSpec :: UploadType
pattern AppiumJavaTestngTestSpec = UploadType' "APPIUM_JAVA_TESTNG_TEST_SPEC"

pattern AppiumNodeTestPackage :: UploadType
pattern AppiumNodeTestPackage = UploadType' "APPIUM_NODE_TEST_PACKAGE"

pattern AppiumNodeTestSpec :: UploadType
pattern AppiumNodeTestSpec = UploadType' "APPIUM_NODE_TEST_SPEC"

pattern AppiumPythonTestPackage :: UploadType
pattern AppiumPythonTestPackage = UploadType' "APPIUM_PYTHON_TEST_PACKAGE"

pattern AppiumPythonTestSpec :: UploadType
pattern AppiumPythonTestSpec = UploadType' "APPIUM_PYTHON_TEST_SPEC"

pattern AppiumRubyTestPackage :: UploadType
pattern AppiumRubyTestPackage = UploadType' "APPIUM_RUBY_TEST_PACKAGE"

pattern AppiumRubyTestSpec :: UploadType
pattern AppiumRubyTestSpec = UploadType' "APPIUM_RUBY_TEST_SPEC"

pattern AppiumWebJavaJunitTestPackage :: UploadType
pattern AppiumWebJavaJunitTestPackage = UploadType' "APPIUM_WEB_JAVA_JUNIT_TEST_PACKAGE"

pattern AppiumWebJavaJunitTestSpec :: UploadType
pattern AppiumWebJavaJunitTestSpec = UploadType' "APPIUM_WEB_JAVA_JUNIT_TEST_SPEC"

pattern AppiumWebJavaTestngTestPackage :: UploadType
pattern AppiumWebJavaTestngTestPackage = UploadType' "APPIUM_WEB_JAVA_TESTNG_TEST_PACKAGE"

pattern AppiumWebJavaTestngTestSpec :: UploadType
pattern AppiumWebJavaTestngTestSpec = UploadType' "APPIUM_WEB_JAVA_TESTNG_TEST_SPEC"

pattern AppiumWebNodeTestPackage :: UploadType
pattern AppiumWebNodeTestPackage = UploadType' "APPIUM_WEB_NODE_TEST_PACKAGE"

pattern AppiumWebNodeTestSpec :: UploadType
pattern AppiumWebNodeTestSpec = UploadType' "APPIUM_WEB_NODE_TEST_SPEC"

pattern AppiumWebPythonTestPackage :: UploadType
pattern AppiumWebPythonTestPackage = UploadType' "APPIUM_WEB_PYTHON_TEST_PACKAGE"

pattern AppiumWebPythonTestSpec :: UploadType
pattern AppiumWebPythonTestSpec = UploadType' "APPIUM_WEB_PYTHON_TEST_SPEC"

pattern AppiumWebRubyTestPackage :: UploadType
pattern AppiumWebRubyTestPackage = UploadType' "APPIUM_WEB_RUBY_TEST_PACKAGE"

pattern AppiumWebRubyTestSpec :: UploadType
pattern AppiumWebRubyTestSpec = UploadType' "APPIUM_WEB_RUBY_TEST_SPEC"

pattern CalabashTestPackage :: UploadType
pattern CalabashTestPackage = UploadType' "CALABASH_TEST_PACKAGE"

pattern ExternalData :: UploadType
pattern ExternalData = UploadType' "EXTERNAL_DATA"

pattern InstrumentationTestPackage :: UploadType
pattern InstrumentationTestPackage = UploadType' "INSTRUMENTATION_TEST_PACKAGE"

pattern InstrumentationTestSpec :: UploadType
pattern InstrumentationTestSpec = UploadType' "INSTRUMENTATION_TEST_SPEC"

pattern IosApp :: UploadType
pattern IosApp = UploadType' "IOS_APP"

pattern UiautomationTestPackage :: UploadType
pattern UiautomationTestPackage = UploadType' "UIAUTOMATION_TEST_PACKAGE"

pattern UiautomatorTestPackage :: UploadType
pattern UiautomatorTestPackage = UploadType' "UIAUTOMATOR_TEST_PACKAGE"

pattern WebApp :: UploadType
pattern WebApp = UploadType' "WEB_APP"

pattern XctestTestPackage :: UploadType
pattern XctestTestPackage = UploadType' "XCTEST_TEST_PACKAGE"

pattern XctestUiTestPackage :: UploadType
pattern XctestUiTestPackage = UploadType' "XCTEST_UI_TEST_PACKAGE"

pattern XctestUiTestSpec :: UploadType
pattern XctestUiTestSpec = UploadType' "XCTEST_UI_TEST_SPEC"

{-# COMPLETE
  AndroidApp,
  AppiumJavaJunitTestPackage,
  AppiumJavaJunitTestSpec,
  AppiumJavaTestngTestPackage,
  AppiumJavaTestngTestSpec,
  AppiumNodeTestPackage,
  AppiumNodeTestSpec,
  AppiumPythonTestPackage,
  AppiumPythonTestSpec,
  AppiumRubyTestPackage,
  AppiumRubyTestSpec,
  AppiumWebJavaJunitTestPackage,
  AppiumWebJavaJunitTestSpec,
  AppiumWebJavaTestngTestPackage,
  AppiumWebJavaTestngTestSpec,
  AppiumWebNodeTestPackage,
  AppiumWebNodeTestSpec,
  AppiumWebPythonTestPackage,
  AppiumWebPythonTestSpec,
  AppiumWebRubyTestPackage,
  AppiumWebRubyTestSpec,
  CalabashTestPackage,
  ExternalData,
  InstrumentationTestPackage,
  InstrumentationTestSpec,
  IosApp,
  UiautomationTestPackage,
  UiautomatorTestPackage,
  WebApp,
  XctestTestPackage,
  XctestUiTestPackage,
  XctestUiTestSpec,
  UploadType' #-}

instance FromText UploadType where
    parser = (UploadType' . mk) <$> takeText

instance ToText UploadType where
    toText (UploadType' ci) = original ci

-- | Represents an enum of /known/ $UploadType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UploadType where
    toEnum i = case i of
        0 -> AndroidApp
        1 -> AppiumJavaJunitTestPackage
        2 -> AppiumJavaJunitTestSpec
        3 -> AppiumJavaTestngTestPackage
        4 -> AppiumJavaTestngTestSpec
        5 -> AppiumNodeTestPackage
        6 -> AppiumNodeTestSpec
        7 -> AppiumPythonTestPackage
        8 -> AppiumPythonTestSpec
        9 -> AppiumRubyTestPackage
        10 -> AppiumRubyTestSpec
        11 -> AppiumWebJavaJunitTestPackage
        12 -> AppiumWebJavaJunitTestSpec
        13 -> AppiumWebJavaTestngTestPackage
        14 -> AppiumWebJavaTestngTestSpec
        15 -> AppiumWebNodeTestPackage
        16 -> AppiumWebNodeTestSpec
        17 -> AppiumWebPythonTestPackage
        18 -> AppiumWebPythonTestSpec
        19 -> AppiumWebRubyTestPackage
        20 -> AppiumWebRubyTestSpec
        21 -> CalabashTestPackage
        22 -> ExternalData
        23 -> InstrumentationTestPackage
        24 -> InstrumentationTestSpec
        25 -> IosApp
        26 -> UiautomationTestPackage
        27 -> UiautomatorTestPackage
        28 -> WebApp
        29 -> XctestTestPackage
        30 -> XctestUiTestPackage
        31 -> XctestUiTestSpec
        _ -> (error . showText) $ "Unknown index for UploadType: " <> toText i
    fromEnum x = case x of
        AndroidApp -> 0
        AppiumJavaJunitTestPackage -> 1
        AppiumJavaJunitTestSpec -> 2
        AppiumJavaTestngTestPackage -> 3
        AppiumJavaTestngTestSpec -> 4
        AppiumNodeTestPackage -> 5
        AppiumNodeTestSpec -> 6
        AppiumPythonTestPackage -> 7
        AppiumPythonTestSpec -> 8
        AppiumRubyTestPackage -> 9
        AppiumRubyTestSpec -> 10
        AppiumWebJavaJunitTestPackage -> 11
        AppiumWebJavaJunitTestSpec -> 12
        AppiumWebJavaTestngTestPackage -> 13
        AppiumWebJavaTestngTestSpec -> 14
        AppiumWebNodeTestPackage -> 15
        AppiumWebNodeTestSpec -> 16
        AppiumWebPythonTestPackage -> 17
        AppiumWebPythonTestSpec -> 18
        AppiumWebRubyTestPackage -> 19
        AppiumWebRubyTestSpec -> 20
        CalabashTestPackage -> 21
        ExternalData -> 22
        InstrumentationTestPackage -> 23
        InstrumentationTestSpec -> 24
        IosApp -> 25
        UiautomationTestPackage -> 26
        UiautomatorTestPackage -> 27
        WebApp -> 28
        XctestTestPackage -> 29
        XctestUiTestPackage -> 30
        XctestUiTestSpec -> 31
        UploadType' name -> (error . showText) $ "Unknown UploadType: " <> original name

-- | Represents the bounds of /known/ $UploadType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UploadType where
    minBound = AndroidApp
    maxBound = XctestUiTestSpec

instance Hashable     UploadType
instance NFData       UploadType
instance ToByteString UploadType
instance ToQuery      UploadType
instance ToHeader     UploadType

instance ToJSON UploadType where
    toJSON = toJSONText

instance FromJSON UploadType where
    parseJSON = parseJSONText "UploadType"
