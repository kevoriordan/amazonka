{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileModeTypeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.FileModeTypeEnum (
  FileModeTypeEnum (
    ..
    , Executable
    , Normal
    , Symlink
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FileModeTypeEnum = FileModeTypeEnum' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Executable :: FileModeTypeEnum
pattern Executable = FileModeTypeEnum' "EXECUTABLE"

pattern Normal :: FileModeTypeEnum
pattern Normal = FileModeTypeEnum' "NORMAL"

pattern Symlink :: FileModeTypeEnum
pattern Symlink = FileModeTypeEnum' "SYMLINK"

{-# COMPLETE
  Executable,
  Normal,
  Symlink,
  FileModeTypeEnum' #-}

instance FromText FileModeTypeEnum where
    parser = (FileModeTypeEnum' . mk) <$> takeText

instance ToText FileModeTypeEnum where
    toText (FileModeTypeEnum' ci) = original ci

-- | Represents an enum of /known/ $FileModeTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FileModeTypeEnum where
    toEnum i = case i of
        0 -> Executable
        1 -> Normal
        2 -> Symlink
        _ -> (error . showText) $ "Unknown index for FileModeTypeEnum: " <> toText i
    fromEnum x = case x of
        Executable -> 0
        Normal -> 1
        Symlink -> 2
        FileModeTypeEnum' name -> (error . showText) $ "Unknown FileModeTypeEnum: " <> original name

-- | Represents the bounds of /known/ $FileModeTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FileModeTypeEnum where
    minBound = Executable
    maxBound = Symlink

instance Hashable     FileModeTypeEnum
instance NFData       FileModeTypeEnum
instance ToByteString FileModeTypeEnum
instance ToQuery      FileModeTypeEnum
instance ToHeader     FileModeTypeEnum

instance ToJSON FileModeTypeEnum where
    toJSON = toJSONText

instance FromJSON FileModeTypeEnum where
    parseJSON = parseJSONText "FileModeTypeEnum"
