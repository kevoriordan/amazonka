{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the start of the execution.
--
--
--
-- /See:/ 'executionStartedEventDetails' smart constructor.
data ExecutionStartedEventDetails = ExecutionStartedEventDetails'{_esedInput
                                                                  ::
                                                                  !(Maybe
                                                                      (Sensitive
                                                                         Text)),
                                                                  _esedRoleARN
                                                                  ::
                                                                  !(Maybe Text)}
                                      deriving (Eq, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'ExecutionStartedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esedInput' - The JSON data input to the execution.
--
-- * 'esedRoleARN' - The Amazon Resource Name (ARN) of the IAM role used for executing AWS Lambda tasks.
executionStartedEventDetails
    :: ExecutionStartedEventDetails
executionStartedEventDetails
  = ExecutionStartedEventDetails'{_esedInput = Nothing,
                                  _esedRoleARN = Nothing}

-- | The JSON data input to the execution.
esedInput :: Lens' ExecutionStartedEventDetails (Maybe Text)
esedInput = lens _esedInput (\ s a -> s{_esedInput = a}) . mapping _Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role used for executing AWS Lambda tasks.
esedRoleARN :: Lens' ExecutionStartedEventDetails (Maybe Text)
esedRoleARN = lens _esedRoleARN (\ s a -> s{_esedRoleARN = a})

instance FromJSON ExecutionStartedEventDetails where
        parseJSON
          = withObject "ExecutionStartedEventDetails"
              (\ x ->
                 ExecutionStartedEventDetails' <$>
                   (x .:? "input") <*> (x .:? "roleArn"))

instance Hashable ExecutionStartedEventDetails where

instance NFData ExecutionStartedEventDetails where
