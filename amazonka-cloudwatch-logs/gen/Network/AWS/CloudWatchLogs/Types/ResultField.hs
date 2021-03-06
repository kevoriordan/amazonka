{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ResultField
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.ResultField where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains one field from one log event returned by a CloudWatch Logs Insights query, along with the value of that field.
--
--
--
-- /See:/ 'resultField' smart constructor.
data ResultField = ResultField'{_rfField ::
                                !(Maybe Text),
                                _rfValue :: !(Maybe Text)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResultField' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfField' - The log event field.
--
-- * 'rfValue' - The value of this field.
resultField
    :: ResultField
resultField
  = ResultField'{_rfField = Nothing,
                 _rfValue = Nothing}

-- | The log event field.
rfField :: Lens' ResultField (Maybe Text)
rfField = lens _rfField (\ s a -> s{_rfField = a})

-- | The value of this field.
rfValue :: Lens' ResultField (Maybe Text)
rfValue = lens _rfValue (\ s a -> s{_rfValue = a})

instance FromJSON ResultField where
        parseJSON
          = withObject "ResultField"
              (\ x ->
                 ResultField' <$> (x .:? "field") <*> (x .:? "value"))

instance Hashable ResultField where

instance NFData ResultField where
