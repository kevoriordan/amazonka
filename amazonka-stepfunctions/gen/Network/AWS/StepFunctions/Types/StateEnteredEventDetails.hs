{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateEnteredEventDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.StateEnteredEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about a state entered during an execution.
--
--
--
-- /See:/ 'stateEnteredEventDetails' smart constructor.
data StateEnteredEventDetails = StateEnteredEventDetails'{_sInput
                                                          ::
                                                          !(Maybe
                                                              (Sensitive Text)),
                                                          _sName :: !Text}
                                  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'StateEnteredEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sInput' - The string that contains the JSON input data for the state.
--
-- * 'sName' - The name of the state.
stateEnteredEventDetails
    :: Text -- ^ 'sName'
    -> StateEnteredEventDetails
stateEnteredEventDetails pName_
  = StateEnteredEventDetails'{_sInput = Nothing,
                              _sName = pName_}

-- | The string that contains the JSON input data for the state.
sInput :: Lens' StateEnteredEventDetails (Maybe Text)
sInput = lens _sInput (\ s a -> s{_sInput = a}) . mapping _Sensitive

-- | The name of the state.
sName :: Lens' StateEnteredEventDetails Text
sName = lens _sName (\ s a -> s{_sName = a})

instance FromJSON StateEnteredEventDetails where
        parseJSON
          = withObject "StateEnteredEventDetails"
              (\ x ->
                 StateEnteredEventDetails' <$>
                   (x .:? "input") <*> (x .: "name"))

instance Hashable StateEnteredEventDetails where

instance NFData StateEnteredEventDetails where
