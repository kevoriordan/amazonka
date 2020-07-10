{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.FileHeaderInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.FileHeaderInfo (
  FileHeaderInfo (
    ..
    , Ignore
    , None
    , Use
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data FileHeaderInfo = FileHeaderInfo' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Ignore :: FileHeaderInfo
pattern Ignore = FileHeaderInfo' "IGNORE"

pattern None :: FileHeaderInfo
pattern None = FileHeaderInfo' "NONE"

pattern Use :: FileHeaderInfo
pattern Use = FileHeaderInfo' "USE"

{-# COMPLETE
  Ignore,
  None,
  Use,
  FileHeaderInfo' #-}

instance FromText FileHeaderInfo where
    parser = (FileHeaderInfo' . mk) <$> takeText

instance ToText FileHeaderInfo where
    toText (FileHeaderInfo' ci) = original ci

-- | Represents an enum of /known/ $FileHeaderInfo.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FileHeaderInfo where
    toEnum i = case i of
        0 -> Ignore
        1 -> None
        2 -> Use
        _ -> (error . showText) $ "Unknown index for FileHeaderInfo: " <> toText i
    fromEnum x = case x of
        Ignore -> 0
        None -> 1
        Use -> 2
        FileHeaderInfo' name -> (error . showText) $ "Unknown FileHeaderInfo: " <> original name

-- | Represents the bounds of /known/ $FileHeaderInfo.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FileHeaderInfo where
    minBound = Ignore
    maxBound = Use

instance Hashable     FileHeaderInfo
instance NFData       FileHeaderInfo
instance ToByteString FileHeaderInfo
instance ToQuery      FileHeaderInfo
instance ToHeader     FileHeaderInfo

instance ToXML FileHeaderInfo where
    toXML = toXMLText
