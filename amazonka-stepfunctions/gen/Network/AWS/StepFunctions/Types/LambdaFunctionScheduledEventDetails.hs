{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a lambda function scheduled during an execution.
--
--
--
-- /See:/ 'lambdaFunctionScheduledEventDetails' smart constructor.
data LambdaFunctionScheduledEventDetails = LambdaFunctionScheduledEventDetails'{_lfsedInput
                                                                                ::
                                                                                !(Maybe
                                                                                    (Sensitive
                                                                                       Text)),
                                                                                _lfsedTimeoutInSeconds
                                                                                ::
                                                                                !(Maybe
                                                                                    Integer),
                                                                                _lfsedResource
                                                                                ::
                                                                                !Text}
                                             deriving (Eq, Show, Data, Typeable,
                                                       Generic)

-- | Creates a value of 'LambdaFunctionScheduledEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfsedInput' - The JSON data input to the lambda function.
--
-- * 'lfsedTimeoutInSeconds' - The maximum allowed duration of the lambda function.
--
-- * 'lfsedResource' - The Amazon Resource Name (ARN) of the scheduled lambda function.
lambdaFunctionScheduledEventDetails
    :: Text -- ^ 'lfsedResource'
    -> LambdaFunctionScheduledEventDetails
lambdaFunctionScheduledEventDetails pResource_
  = LambdaFunctionScheduledEventDetails'{_lfsedInput =
                                           Nothing,
                                         _lfsedTimeoutInSeconds = Nothing,
                                         _lfsedResource = pResource_}

-- | The JSON data input to the lambda function.
lfsedInput :: Lens' LambdaFunctionScheduledEventDetails (Maybe Text)
lfsedInput = lens _lfsedInput (\ s a -> s{_lfsedInput = a}) . mapping _Sensitive

-- | The maximum allowed duration of the lambda function.
lfsedTimeoutInSeconds :: Lens' LambdaFunctionScheduledEventDetails (Maybe Integer)
lfsedTimeoutInSeconds = lens _lfsedTimeoutInSeconds (\ s a -> s{_lfsedTimeoutInSeconds = a})

-- | The Amazon Resource Name (ARN) of the scheduled lambda function.
lfsedResource :: Lens' LambdaFunctionScheduledEventDetails Text
lfsedResource = lens _lfsedResource (\ s a -> s{_lfsedResource = a})

instance FromJSON LambdaFunctionScheduledEventDetails
         where
        parseJSON
          = withObject "LambdaFunctionScheduledEventDetails"
              (\ x ->
                 LambdaFunctionScheduledEventDetails' <$>
                   (x .:? "input") <*> (x .:? "timeoutInSeconds") <*>
                     (x .: "resource"))

instance Hashable LambdaFunctionScheduledEventDetails
         where

instance NFData LambdaFunctionScheduledEventDetails
         where
