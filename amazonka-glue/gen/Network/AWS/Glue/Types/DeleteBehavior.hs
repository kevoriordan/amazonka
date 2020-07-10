{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DeleteBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.DeleteBehavior (
  DeleteBehavior (
    ..
    , DeleteFromDatabase
    , DeprecateInDatabase
    , Log
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeleteBehavior = DeleteBehavior' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern DeleteFromDatabase :: DeleteBehavior
pattern DeleteFromDatabase = DeleteBehavior' "DELETE_FROM_DATABASE"

pattern DeprecateInDatabase :: DeleteBehavior
pattern DeprecateInDatabase = DeleteBehavior' "DEPRECATE_IN_DATABASE"

pattern Log :: DeleteBehavior
pattern Log = DeleteBehavior' "LOG"

{-# COMPLETE
  DeleteFromDatabase,
  DeprecateInDatabase,
  Log,
  DeleteBehavior' #-}

instance FromText DeleteBehavior where
    parser = (DeleteBehavior' . mk) <$> takeText

instance ToText DeleteBehavior where
    toText (DeleteBehavior' ci) = original ci

-- | Represents an enum of /known/ $DeleteBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeleteBehavior where
    toEnum i = case i of
        0 -> DeleteFromDatabase
        1 -> DeprecateInDatabase
        2 -> Log
        _ -> (error . showText) $ "Unknown index for DeleteBehavior: " <> toText i
    fromEnum x = case x of
        DeleteFromDatabase -> 0
        DeprecateInDatabase -> 1
        Log -> 2
        DeleteBehavior' name -> (error . showText) $ "Unknown DeleteBehavior: " <> original name

-- | Represents the bounds of /known/ $DeleteBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeleteBehavior where
    minBound = DeleteFromDatabase
    maxBound = Log

instance Hashable     DeleteBehavior
instance NFData       DeleteBehavior
instance ToByteString DeleteBehavior
instance ToQuery      DeleteBehavior
instance ToHeader     DeleteBehavior

instance ToJSON DeleteBehavior where
    toJSON = toJSONText

instance FromJSON DeleteBehavior where
    parseJSON = parseJSONText "DeleteBehavior"
