{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.FileSystemType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.FileSystemType (
  FileSystemType (
    ..
    , Efs
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FileSystemType = FileSystemType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Efs :: FileSystemType
pattern Efs = FileSystemType' "EFS"

{-# COMPLETE
  Efs,
  FileSystemType' #-}

instance FromText FileSystemType where
    parser = (FileSystemType' . mk) <$> takeText

instance ToText FileSystemType where
    toText (FileSystemType' ci) = original ci

-- | Represents an enum of /known/ $FileSystemType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FileSystemType where
    toEnum i = case i of
        0 -> Efs
        _ -> (error . showText) $ "Unknown index for FileSystemType: " <> toText i
    fromEnum x = case x of
        Efs -> 0
        FileSystemType' name -> (error . showText) $ "Unknown FileSystemType: " <> original name

-- | Represents the bounds of /known/ $FileSystemType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FileSystemType where
    minBound = Efs
    maxBound = Efs

instance Hashable     FileSystemType
instance NFData       FileSystemType
instance ToByteString FileSystemType
instance ToQuery      FileSystemType
instance ToHeader     FileSystemType

instance ToJSON FileSystemType where
    toJSON = toJSONText

instance FromJSON FileSystemType where
    parseJSON = parseJSONText "FileSystemType"
