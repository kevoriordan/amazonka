{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.TriggeringDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.TriggeringDataset where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the data set whose content generation triggers the new data set content generation.
--
--
--
-- /See:/ 'triggeringDataset' smart constructor.
newtype TriggeringDataset = TriggeringDataset'{_tdName
                                               :: Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TriggeringDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdName' - The name of the data set whose content generation triggers the new data set content generation.
triggeringDataset
    :: Text -- ^ 'tdName'
    -> TriggeringDataset
triggeringDataset pName_
  = TriggeringDataset'{_tdName = pName_}

-- | The name of the data set whose content generation triggers the new data set content generation.
tdName :: Lens' TriggeringDataset Text
tdName = lens _tdName (\ s a -> s{_tdName = a})

instance FromJSON TriggeringDataset where
        parseJSON
          = withObject "TriggeringDataset"
              (\ x -> TriggeringDataset' <$> (x .: "name"))

instance Hashable TriggeringDataset where

instance NFData TriggeringDataset where

instance ToJSON TriggeringDataset where
        toJSON TriggeringDataset'{..}
          = object (catMaybes [Just ("name" .= _tdName)])
