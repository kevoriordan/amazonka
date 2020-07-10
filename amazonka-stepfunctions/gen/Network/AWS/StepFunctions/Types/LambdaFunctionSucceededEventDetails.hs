{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a lambda function that successfully terminated during an execution.
--
--
--
-- /See:/ 'lambdaFunctionSucceededEventDetails' smart constructor.
newtype LambdaFunctionSucceededEventDetails = LambdaFunctionSucceededEventDetails'{_lfsedOutput
                                                                                   ::
                                                                                   Maybe
                                                                                     (Sensitive
                                                                                        Text)}
                                                deriving (Eq, Show, Data,
                                                          Typeable, Generic)

-- | Creates a value of 'LambdaFunctionSucceededEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfsedOutput' - The JSON data output by the lambda function.
lambdaFunctionSucceededEventDetails
    :: LambdaFunctionSucceededEventDetails
lambdaFunctionSucceededEventDetails
  = LambdaFunctionSucceededEventDetails'{_lfsedOutput =
                                           Nothing}

-- | The JSON data output by the lambda function.
lfsedOutput :: Lens' LambdaFunctionSucceededEventDetails (Maybe Text)
lfsedOutput = lens _lfsedOutput (\ s a -> s{_lfsedOutput = a}) . mapping _Sensitive

instance FromJSON LambdaFunctionSucceededEventDetails
         where
        parseJSON
          = withObject "LambdaFunctionSucceededEventDetails"
              (\ x ->
                 LambdaFunctionSucceededEventDetails' <$>
                   (x .:? "output"))

instance Hashable LambdaFunctionSucceededEventDetails
         where

instance NFData LambdaFunctionSucceededEventDetails
         where
