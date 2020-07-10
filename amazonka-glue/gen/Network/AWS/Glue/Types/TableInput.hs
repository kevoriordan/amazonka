{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableInput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TableInput where

import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure used to define a table.
--
--
--
-- /See:/ 'tableInput' smart constructor.
data TableInput = TableInput'{_tiRetention ::
                              !(Maybe Nat),
                              _tiTableType :: !(Maybe Text),
                              _tiOwner :: !(Maybe Text),
                              _tiViewOriginalText :: !(Maybe Text),
                              _tiViewExpandedText :: !(Maybe Text),
                              _tiLastAnalyzedTime :: !(Maybe POSIX),
                              _tiStorageDescriptor ::
                              !(Maybe StorageDescriptor),
                              _tiParameters :: !(Maybe (Map Text Text)),
                              _tiLastAccessTime :: !(Maybe POSIX),
                              _tiDescription :: !(Maybe Text),
                              _tiPartitionKeys :: !(Maybe [Column]),
                              _tiName :: !Text}
                    deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TableInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiRetention' - The retention time for this table.
--
-- * 'tiTableType' - The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
--
-- * 'tiOwner' - The table owner.
--
-- * 'tiViewOriginalText' - If the table is a view, the original text of the view; otherwise @null@ .
--
-- * 'tiViewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@ .
--
-- * 'tiLastAnalyzedTime' - The last time that column statistics were computed for this table.
--
-- * 'tiStorageDescriptor' - A storage descriptor containing information about the physical storage of this table.
--
-- * 'tiParameters' - These key-value pairs define properties associated with the table.
--
-- * 'tiLastAccessTime' - The last time that the table was accessed.
--
-- * 'tiDescription' - A description of the table.
--
-- * 'tiPartitionKeys' - A list of columns by which the table is partitioned. Only primitive types are supported as partition keys. When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example: @"PartitionKeys": []@ 
--
-- * 'tiName' - The table name. For Hive compatibility, this is folded to lowercase when it is stored.
tableInput
    :: Text -- ^ 'tiName'
    -> TableInput
tableInput pName_
  = TableInput'{_tiRetention = Nothing,
                _tiTableType = Nothing, _tiOwner = Nothing,
                _tiViewOriginalText = Nothing,
                _tiViewExpandedText = Nothing,
                _tiLastAnalyzedTime = Nothing,
                _tiStorageDescriptor = Nothing,
                _tiParameters = Nothing, _tiLastAccessTime = Nothing,
                _tiDescription = Nothing, _tiPartitionKeys = Nothing,
                _tiName = pName_}

-- | The retention time for this table.
tiRetention :: Lens' TableInput (Maybe Natural)
tiRetention = lens _tiRetention (\ s a -> s{_tiRetention = a}) . mapping _Nat

-- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
tiTableType :: Lens' TableInput (Maybe Text)
tiTableType = lens _tiTableType (\ s a -> s{_tiTableType = a})

-- | The table owner.
tiOwner :: Lens' TableInput (Maybe Text)
tiOwner = lens _tiOwner (\ s a -> s{_tiOwner = a})

-- | If the table is a view, the original text of the view; otherwise @null@ .
tiViewOriginalText :: Lens' TableInput (Maybe Text)
tiViewOriginalText = lens _tiViewOriginalText (\ s a -> s{_tiViewOriginalText = a})

-- | If the table is a view, the expanded text of the view; otherwise @null@ .
tiViewExpandedText :: Lens' TableInput (Maybe Text)
tiViewExpandedText = lens _tiViewExpandedText (\ s a -> s{_tiViewExpandedText = a})

-- | The last time that column statistics were computed for this table.
tiLastAnalyzedTime :: Lens' TableInput (Maybe UTCTime)
tiLastAnalyzedTime = lens _tiLastAnalyzedTime (\ s a -> s{_tiLastAnalyzedTime = a}) . mapping _Time

-- | A storage descriptor containing information about the physical storage of this table.
tiStorageDescriptor :: Lens' TableInput (Maybe StorageDescriptor)
tiStorageDescriptor = lens _tiStorageDescriptor (\ s a -> s{_tiStorageDescriptor = a})

-- | These key-value pairs define properties associated with the table.
tiParameters :: Lens' TableInput (HashMap Text Text)
tiParameters = lens _tiParameters (\ s a -> s{_tiParameters = a}) . _Default . _Map

-- | The last time that the table was accessed.
tiLastAccessTime :: Lens' TableInput (Maybe UTCTime)
tiLastAccessTime = lens _tiLastAccessTime (\ s a -> s{_tiLastAccessTime = a}) . mapping _Time

-- | A description of the table.
tiDescription :: Lens' TableInput (Maybe Text)
tiDescription = lens _tiDescription (\ s a -> s{_tiDescription = a})

-- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys. When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example: @"PartitionKeys": []@ 
tiPartitionKeys :: Lens' TableInput [Column]
tiPartitionKeys = lens _tiPartitionKeys (\ s a -> s{_tiPartitionKeys = a}) . _Default . _Coerce

-- | The table name. For Hive compatibility, this is folded to lowercase when it is stored.
tiName :: Lens' TableInput Text
tiName = lens _tiName (\ s a -> s{_tiName = a})

instance Hashable TableInput where

instance NFData TableInput where

instance ToJSON TableInput where
        toJSON TableInput'{..}
          = object
              (catMaybes
                 [("Retention" .=) <$> _tiRetention,
                  ("TableType" .=) <$> _tiTableType,
                  ("Owner" .=) <$> _tiOwner,
                  ("ViewOriginalText" .=) <$> _tiViewOriginalText,
                  ("ViewExpandedText" .=) <$> _tiViewExpandedText,
                  ("LastAnalyzedTime" .=) <$> _tiLastAnalyzedTime,
                  ("StorageDescriptor" .=) <$> _tiStorageDescriptor,
                  ("Parameters" .=) <$> _tiParameters,
                  ("LastAccessTime" .=) <$> _tiLastAccessTime,
                  ("Description" .=) <$> _tiDescription,
                  ("PartitionKeys" .=) <$> _tiPartitionKeys,
                  Just ("Name" .= _tiName)])
