{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeltaTime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DeltaTime where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to limit data to that which has arrived since the last execution of the action.
--
--
--
-- /See:/ 'deltaTime' smart constructor.
data DeltaTime = DeltaTime'{_dtOffsetSeconds :: !Int,
                            _dtTimeExpression :: !Text}
                   deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeltaTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtOffsetSeconds' - The number of seconds of estimated "in flight" lag time of message data. When you create data set contents using message data from a specified time frame, some message data may still be "in flight" when processing begins, and so will not arrive in time to be processed. Use this field to make allowances for the "in flight" time of your message data, so that data not processed from a previous time frame will be included with the next time frame. Without this, missed message data would be excluded from processing during the next time frame as well, because its timestamp places it within the previous time frame.
--
-- * 'dtTimeExpression' - An expression by which the time of the message data may be determined. This may be the name of a timestamp field, or a SQL expression which is used to derive the time the message data was generated.
deltaTime
    :: Int -- ^ 'dtOffsetSeconds'
    -> Text -- ^ 'dtTimeExpression'
    -> DeltaTime
deltaTime pOffsetSeconds_ pTimeExpression_
  = DeltaTime'{_dtOffsetSeconds = pOffsetSeconds_,
               _dtTimeExpression = pTimeExpression_}

-- | The number of seconds of estimated "in flight" lag time of message data. When you create data set contents using message data from a specified time frame, some message data may still be "in flight" when processing begins, and so will not arrive in time to be processed. Use this field to make allowances for the "in flight" time of your message data, so that data not processed from a previous time frame will be included with the next time frame. Without this, missed message data would be excluded from processing during the next time frame as well, because its timestamp places it within the previous time frame.
dtOffsetSeconds :: Lens' DeltaTime Int
dtOffsetSeconds = lens _dtOffsetSeconds (\ s a -> s{_dtOffsetSeconds = a})

-- | An expression by which the time of the message data may be determined. This may be the name of a timestamp field, or a SQL expression which is used to derive the time the message data was generated.
dtTimeExpression :: Lens' DeltaTime Text
dtTimeExpression = lens _dtTimeExpression (\ s a -> s{_dtTimeExpression = a})

instance FromJSON DeltaTime where
        parseJSON
          = withObject "DeltaTime"
              (\ x ->
                 DeltaTime' <$>
                   (x .: "offsetSeconds") <*> (x .: "timeExpression"))

instance Hashable DeltaTime where

instance NFData DeltaTime where

instance ToJSON DeltaTime where
        toJSON DeltaTime'{..}
          = object
              (catMaybes
                 [Just ("offsetSeconds" .= _dtOffsetSeconds),
                  Just ("timeExpression" .= _dtTimeExpression)])
