{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Database
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Database where

import Network.AWS.Glue.Types.PrincipalPermissions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @Database@ object represents a logical grouping of tables that might reside in a Hive metastore or an RDBMS.
--
--
--
-- /See:/ 'database' smart constructor.
data Database = Database'{_dLocationURI ::
                          !(Maybe Text),
                          _dParameters :: !(Maybe (Map Text Text)),
                          _dDescription :: !(Maybe Text),
                          _dCreateTime :: !(Maybe POSIX),
                          _dCreateTableDefaultPermissions ::
                          !(Maybe [PrincipalPermissions]),
                          _dName :: !Text}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Database' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dLocationURI' - The location of the database (for example, an HDFS path).
--
-- * 'dParameters' - These key-value pairs define parameters and properties of the database.
--
-- * 'dDescription' - A description of the database.
--
-- * 'dCreateTime' - The time at which the metadata database was created in the catalog.
--
-- * 'dCreateTableDefaultPermissions' - Creates a set of default permissions on the table for principals. 
--
-- * 'dName' - The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
database
    :: Text -- ^ 'dName'
    -> Database
database pName_
  = Database'{_dLocationURI = Nothing,
              _dParameters = Nothing, _dDescription = Nothing,
              _dCreateTime = Nothing,
              _dCreateTableDefaultPermissions = Nothing,
              _dName = pName_}

-- | The location of the database (for example, an HDFS path).
dLocationURI :: Lens' Database (Maybe Text)
dLocationURI = lens _dLocationURI (\ s a -> s{_dLocationURI = a})

-- | These key-value pairs define parameters and properties of the database.
dParameters :: Lens' Database (HashMap Text Text)
dParameters = lens _dParameters (\ s a -> s{_dParameters = a}) . _Default . _Map

-- | A description of the database.
dDescription :: Lens' Database (Maybe Text)
dDescription = lens _dDescription (\ s a -> s{_dDescription = a})

-- | The time at which the metadata database was created in the catalog.
dCreateTime :: Lens' Database (Maybe UTCTime)
dCreateTime = lens _dCreateTime (\ s a -> s{_dCreateTime = a}) . mapping _Time

-- | Creates a set of default permissions on the table for principals. 
dCreateTableDefaultPermissions :: Lens' Database [PrincipalPermissions]
dCreateTableDefaultPermissions = lens _dCreateTableDefaultPermissions (\ s a -> s{_dCreateTableDefaultPermissions = a}) . _Default . _Coerce

-- | The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
dName :: Lens' Database Text
dName = lens _dName (\ s a -> s{_dName = a})

instance FromJSON Database where
        parseJSON
          = withObject "Database"
              (\ x ->
                 Database' <$>
                   (x .:? "LocationUri") <*>
                     (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "Description")
                     <*> (x .:? "CreateTime")
                     <*>
                     (x .:? "CreateTableDefaultPermissions" .!= mempty)
                     <*> (x .: "Name"))

instance Hashable Database where

instance NFData Database where
