{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an activity that successfully terminated during an execution.
--
--
--
-- /See:/ 'activitySucceededEventDetails' smart constructor.
newtype ActivitySucceededEventDetails = ActivitySucceededEventDetails'{_asedOutput
                                                                       ::
                                                                       Maybe
                                                                         (Sensitive
                                                                            Text)}
                                          deriving (Eq, Show, Data, Typeable,
                                                    Generic)

-- | Creates a value of 'ActivitySucceededEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asedOutput' - The JSON data output by the activity task.
activitySucceededEventDetails
    :: ActivitySucceededEventDetails
activitySucceededEventDetails
  = ActivitySucceededEventDetails'{_asedOutput =
                                     Nothing}

-- | The JSON data output by the activity task.
asedOutput :: Lens' ActivitySucceededEventDetails (Maybe Text)
asedOutput = lens _asedOutput (\ s a -> s{_asedOutput = a}) . mapping _Sensitive

instance FromJSON ActivitySucceededEventDetails where
        parseJSON
          = withObject "ActivitySucceededEventDetails"
              (\ x ->
                 ActivitySucceededEventDetails' <$> (x .:? "output"))

instance Hashable ActivitySucceededEventDetails where

instance NFData ActivitySucceededEventDetails where
