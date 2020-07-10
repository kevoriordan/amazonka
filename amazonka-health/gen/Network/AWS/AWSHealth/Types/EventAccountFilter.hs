{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventAccountFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.EventAccountFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The values used to filter results from the 'DescribeEventDetailsForOrganization' and 'DescribeAffectedEntitiesForOrganization' operations.
--
--
--
-- /See:/ 'eventAccountFilter' smart constructor.
data EventAccountFilter = EventAccountFilter'{_eafEventARN
                                              :: !Text,
                                              _eafAwsAccountId :: !Text}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventAccountFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eafEventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@ 
--
-- * 'eafAwsAccountId' - The 12-digit AWS account numbers that contains the affected entities.
eventAccountFilter
    :: Text -- ^ 'eafEventARN'
    -> Text -- ^ 'eafAwsAccountId'
    -> EventAccountFilter
eventAccountFilter pEventARN_ pAwsAccountId_
  = EventAccountFilter'{_eafEventARN = pEventARN_,
                        _eafAwsAccountId = pAwsAccountId_}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@ 
eafEventARN :: Lens' EventAccountFilter Text
eafEventARN = lens _eafEventARN (\ s a -> s{_eafEventARN = a})

-- | The 12-digit AWS account numbers that contains the affected entities.
eafAwsAccountId :: Lens' EventAccountFilter Text
eafAwsAccountId = lens _eafAwsAccountId (\ s a -> s{_eafAwsAccountId = a})

instance Hashable EventAccountFilter where

instance NFData EventAccountFilter where

instance ToJSON EventAccountFilter where
        toJSON EventAccountFilter'{..}
          = object
              (catMaybes
                 [Just ("eventArn" .= _eafEventARN),
                  Just ("awsAccountId" .= _eafAwsAccountId)])
