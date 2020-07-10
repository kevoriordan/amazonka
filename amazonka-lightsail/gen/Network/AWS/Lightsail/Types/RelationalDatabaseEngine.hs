{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseEngine
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.RelationalDatabaseEngine (
  RelationalDatabaseEngine (
    ..
    , Mysql
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RelationalDatabaseEngine = RelationalDatabaseEngine' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern Mysql :: RelationalDatabaseEngine
pattern Mysql = RelationalDatabaseEngine' "mysql"

{-# COMPLETE
  Mysql,
  RelationalDatabaseEngine' #-}

instance FromText RelationalDatabaseEngine where
    parser = (RelationalDatabaseEngine' . mk) <$> takeText

instance ToText RelationalDatabaseEngine where
    toText (RelationalDatabaseEngine' ci) = original ci

-- | Represents an enum of /known/ $RelationalDatabaseEngine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RelationalDatabaseEngine where
    toEnum i = case i of
        0 -> Mysql
        _ -> (error . showText) $ "Unknown index for RelationalDatabaseEngine: " <> toText i
    fromEnum x = case x of
        Mysql -> 0
        RelationalDatabaseEngine' name -> (error . showText) $ "Unknown RelationalDatabaseEngine: " <> original name

-- | Represents the bounds of /known/ $RelationalDatabaseEngine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RelationalDatabaseEngine where
    minBound = Mysql
    maxBound = Mysql

instance Hashable     RelationalDatabaseEngine
instance NFData       RelationalDatabaseEngine
instance ToByteString RelationalDatabaseEngine
instance ToQuery      RelationalDatabaseEngine
instance ToHeader     RelationalDatabaseEngine

instance FromJSON RelationalDatabaseEngine where
    parseJSON = parseJSONText "RelationalDatabaseEngine"
