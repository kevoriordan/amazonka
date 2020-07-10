{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the successful termination of the execution.
--
--
--
-- /See:/ 'executionSucceededEventDetails' smart constructor.
newtype ExecutionSucceededEventDetails = ExecutionSucceededEventDetails'{_esedOutput
                                                                         ::
                                                                         Maybe
                                                                           (Sensitive
                                                                              Text)}
                                           deriving (Eq, Show, Data, Typeable,
                                                     Generic)

-- | Creates a value of 'ExecutionSucceededEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esedOutput' - The JSON data output by the execution.
executionSucceededEventDetails
    :: ExecutionSucceededEventDetails
executionSucceededEventDetails
  = ExecutionSucceededEventDetails'{_esedOutput =
                                      Nothing}

-- | The JSON data output by the execution.
esedOutput :: Lens' ExecutionSucceededEventDetails (Maybe Text)
esedOutput = lens _esedOutput (\ s a -> s{_esedOutput = a}) . mapping _Sensitive

instance FromJSON ExecutionSucceededEventDetails
         where
        parseJSON
          = withObject "ExecutionSucceededEventDetails"
              (\ x ->
                 ExecutionSucceededEventDetails' <$> (x .:? "output"))

instance Hashable ExecutionSucceededEventDetails
         where

instance NFData ExecutionSucceededEventDetails where
