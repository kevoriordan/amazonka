{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecutionContext
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.QueryExecutionContext where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The database in which the query execution occurs.
--
--
--
-- /See:/ 'queryExecutionContext' smart constructor.
newtype QueryExecutionContext = QueryExecutionContext'{_qecDatabase
                                                       :: Maybe Text}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'QueryExecutionContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qecDatabase' - The name of the database.
queryExecutionContext
    :: QueryExecutionContext
queryExecutionContext
  = QueryExecutionContext'{_qecDatabase = Nothing}

-- | The name of the database.
qecDatabase :: Lens' QueryExecutionContext (Maybe Text)
qecDatabase = lens _qecDatabase (\ s a -> s{_qecDatabase = a})

instance FromJSON QueryExecutionContext where
        parseJSON
          = withObject "QueryExecutionContext"
              (\ x ->
                 QueryExecutionContext' <$> (x .:? "Database"))

instance Hashable QueryExecutionContext where

instance NFData QueryExecutionContext where

instance ToJSON QueryExecutionContext where
        toJSON QueryExecutionContext'{..}
          = object
              (catMaybes [("Database" .=) <$> _qecDatabase])
