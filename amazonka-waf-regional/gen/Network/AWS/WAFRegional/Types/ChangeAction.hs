{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ChangeAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.ChangeAction (
  ChangeAction (
    ..
    , Delete
    , Insert
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChangeAction = ChangeAction' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Delete :: ChangeAction
pattern Delete = ChangeAction' "DELETE"

pattern Insert :: ChangeAction
pattern Insert = ChangeAction' "INSERT"

{-# COMPLETE
  Delete,
  Insert,
  ChangeAction' #-}

instance FromText ChangeAction where
    parser = (ChangeAction' . mk) <$> takeText

instance ToText ChangeAction where
    toText (ChangeAction' ci) = original ci

-- | Represents an enum of /known/ $ChangeAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChangeAction where
    toEnum i = case i of
        0 -> Delete
        1 -> Insert
        _ -> (error . showText) $ "Unknown index for ChangeAction: " <> toText i
    fromEnum x = case x of
        Delete -> 0
        Insert -> 1
        ChangeAction' name -> (error . showText) $ "Unknown ChangeAction: " <> original name

-- | Represents the bounds of /known/ $ChangeAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChangeAction where
    minBound = Delete
    maxBound = Insert

instance Hashable     ChangeAction
instance NFData       ChangeAction
instance ToByteString ChangeAction
instance ToQuery      ChangeAction
instance ToHeader     ChangeAction

instance ToJSON ChangeAction where
    toJSON = toJSONText
