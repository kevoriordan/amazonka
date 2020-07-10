{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.TableToReload
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.TableToReload where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the name of the schema and table to be reloaded.
--
--
--
-- /See:/ 'tableToReload' smart constructor.
data TableToReload = TableToReload'{_ttrSchemaName ::
                                    !(Maybe Text),
                                    _ttrTableName :: !(Maybe Text)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TableToReload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttrSchemaName' - The schema name of the table to be reloaded.
--
-- * 'ttrTableName' - The table name of the table to be reloaded.
tableToReload
    :: TableToReload
tableToReload
  = TableToReload'{_ttrSchemaName = Nothing,
                   _ttrTableName = Nothing}

-- | The schema name of the table to be reloaded.
ttrSchemaName :: Lens' TableToReload (Maybe Text)
ttrSchemaName = lens _ttrSchemaName (\ s a -> s{_ttrSchemaName = a})

-- | The table name of the table to be reloaded.
ttrTableName :: Lens' TableToReload (Maybe Text)
ttrTableName = lens _ttrTableName (\ s a -> s{_ttrTableName = a})

instance Hashable TableToReload where

instance NFData TableToReload where

instance ToJSON TableToReload where
        toJSON TableToReload'{..}
          = object
              (catMaybes
                 [("SchemaName" .=) <$> _ttrSchemaName,
                  ("TableName" .=) <$> _ttrTableName])
