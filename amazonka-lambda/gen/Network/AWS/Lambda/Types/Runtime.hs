{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Runtime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.Runtime (
  Runtime (
    ..
    , DOTNETCORE1_0
    , DOTNETCORE2_0
    , DOTNETCORE2_1
    , DOTNETCORE3_1
    , GO1_x
    , JAVA11
    , JAVA8
    , NODEJS10_x
    , NODEJS12_x
    , NODEJS4_3
    , NODEJS4_3Edge
    , NODEJS6_10
    , NODEJS8_10
    , Nodejs
    , PYTHON2_7
    , PYTHON3_6
    , PYTHON3_7
    , PYTHON3_8
    , Provided
    , RUBY2_5
    , RUBY2_7
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Runtime = Runtime' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern DOTNETCORE1_0 :: Runtime
pattern DOTNETCORE1_0 = Runtime' "dotnetcore1.0"

pattern DOTNETCORE2_0 :: Runtime
pattern DOTNETCORE2_0 = Runtime' "dotnetcore2.0"

pattern DOTNETCORE2_1 :: Runtime
pattern DOTNETCORE2_1 = Runtime' "dotnetcore2.1"

pattern DOTNETCORE3_1 :: Runtime
pattern DOTNETCORE3_1 = Runtime' "dotnetcore3.1"

pattern GO1_x :: Runtime
pattern GO1_x = Runtime' "go1.x"

pattern JAVA11 :: Runtime
pattern JAVA11 = Runtime' "java11"

pattern JAVA8 :: Runtime
pattern JAVA8 = Runtime' "java8"

pattern NODEJS10_x :: Runtime
pattern NODEJS10_x = Runtime' "nodejs10.x"

pattern NODEJS12_x :: Runtime
pattern NODEJS12_x = Runtime' "nodejs12.x"

pattern NODEJS4_3 :: Runtime
pattern NODEJS4_3 = Runtime' "nodejs4.3"

pattern NODEJS4_3Edge :: Runtime
pattern NODEJS4_3Edge = Runtime' "nodejs4.3-edge"

pattern NODEJS6_10 :: Runtime
pattern NODEJS6_10 = Runtime' "nodejs6.10"

pattern NODEJS8_10 :: Runtime
pattern NODEJS8_10 = Runtime' "nodejs8.10"

pattern Nodejs :: Runtime
pattern Nodejs = Runtime' "nodejs"

pattern PYTHON2_7 :: Runtime
pattern PYTHON2_7 = Runtime' "python2.7"

pattern PYTHON3_6 :: Runtime
pattern PYTHON3_6 = Runtime' "python3.6"

pattern PYTHON3_7 :: Runtime
pattern PYTHON3_7 = Runtime' "python3.7"

pattern PYTHON3_8 :: Runtime
pattern PYTHON3_8 = Runtime' "python3.8"

pattern Provided :: Runtime
pattern Provided = Runtime' "provided"

pattern RUBY2_5 :: Runtime
pattern RUBY2_5 = Runtime' "ruby2.5"

pattern RUBY2_7 :: Runtime
pattern RUBY2_7 = Runtime' "ruby2.7"

{-# COMPLETE
  DOTNETCORE1_0,
  DOTNETCORE2_0,
  DOTNETCORE2_1,
  DOTNETCORE3_1,
  GO1_x,
  JAVA11,
  JAVA8,
  NODEJS10_x,
  NODEJS12_x,
  NODEJS4_3,
  NODEJS4_3Edge,
  NODEJS6_10,
  NODEJS8_10,
  Nodejs,
  PYTHON2_7,
  PYTHON3_6,
  PYTHON3_7,
  PYTHON3_8,
  Provided,
  RUBY2_5,
  RUBY2_7,
  Runtime' #-}

instance FromText Runtime where
    parser = (Runtime' . mk) <$> takeText

instance ToText Runtime where
    toText (Runtime' ci) = original ci

-- | Represents an enum of /known/ $Runtime.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Runtime where
    toEnum i = case i of
        0 -> DOTNETCORE1_0
        1 -> DOTNETCORE2_0
        2 -> DOTNETCORE2_1
        3 -> DOTNETCORE3_1
        4 -> GO1_x
        5 -> JAVA11
        6 -> JAVA8
        7 -> NODEJS10_x
        8 -> NODEJS12_x
        9 -> NODEJS4_3
        10 -> NODEJS4_3Edge
        11 -> NODEJS6_10
        12 -> NODEJS8_10
        13 -> Nodejs
        14 -> PYTHON2_7
        15 -> PYTHON3_6
        16 -> PYTHON3_7
        17 -> PYTHON3_8
        18 -> Provided
        19 -> RUBY2_5
        20 -> RUBY2_7
        _ -> (error . showText) $ "Unknown index for Runtime: " <> toText i
    fromEnum x = case x of
        DOTNETCORE1_0 -> 0
        DOTNETCORE2_0 -> 1
        DOTNETCORE2_1 -> 2
        DOTNETCORE3_1 -> 3
        GO1_x -> 4
        JAVA11 -> 5
        JAVA8 -> 6
        NODEJS10_x -> 7
        NODEJS12_x -> 8
        NODEJS4_3 -> 9
        NODEJS4_3Edge -> 10
        NODEJS6_10 -> 11
        NODEJS8_10 -> 12
        Nodejs -> 13
        PYTHON2_7 -> 14
        PYTHON3_6 -> 15
        PYTHON3_7 -> 16
        PYTHON3_8 -> 17
        Provided -> 18
        RUBY2_5 -> 19
        RUBY2_7 -> 20
        Runtime' name -> (error . showText) $ "Unknown Runtime: " <> original name

-- | Represents the bounds of /known/ $Runtime.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Runtime where
    minBound = DOTNETCORE1_0
    maxBound = RUBY2_7

instance Hashable     Runtime
instance NFData       Runtime
instance ToByteString Runtime
instance ToQuery      Runtime
instance ToHeader     Runtime

instance ToJSON Runtime where
    toJSON = toJSONText

instance FromJSON Runtime where
    parseJSON = parseJSONText "Runtime"
