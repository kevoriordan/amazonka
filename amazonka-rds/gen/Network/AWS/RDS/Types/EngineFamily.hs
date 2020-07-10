{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EngineFamily
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.EngineFamily (
  EngineFamily (
    ..
    , Mysql
    , Postgresql
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EngineFamily = EngineFamily' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Mysql :: EngineFamily
pattern Mysql = EngineFamily' "MYSQL"

pattern Postgresql :: EngineFamily
pattern Postgresql = EngineFamily' "POSTGRESQL"

{-# COMPLETE
  Mysql,
  Postgresql,
  EngineFamily' #-}

instance FromText EngineFamily where
    parser = (EngineFamily' . mk) <$> takeText

instance ToText EngineFamily where
    toText (EngineFamily' ci) = original ci

-- | Represents an enum of /known/ $EngineFamily.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EngineFamily where
    toEnum i = case i of
        0 -> Mysql
        1 -> Postgresql
        _ -> (error . showText) $ "Unknown index for EngineFamily: " <> toText i
    fromEnum x = case x of
        Mysql -> 0
        Postgresql -> 1
        EngineFamily' name -> (error . showText) $ "Unknown EngineFamily: " <> original name

-- | Represents the bounds of /known/ $EngineFamily.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EngineFamily where
    minBound = Mysql
    maxBound = Postgresql

instance Hashable     EngineFamily
instance NFData       EngineFamily
instance ToByteString EngineFamily
instance ToQuery      EngineFamily
instance ToHeader     EngineFamily
