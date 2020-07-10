{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateExitedEventDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.StateExitedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an exit from a state during an execution.
--
--
--
-- /See:/ 'stateExitedEventDetails' smart constructor.
data StateExitedEventDetails = StateExitedEventDetails'{_seedOutput
                                                        ::
                                                        !(Maybe
                                                            (Sensitive Text)),
                                                        _seedName :: !Text}
                                 deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'StateExitedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seedOutput' - The JSON output data of the state.
--
-- * 'seedName' - The name of the state. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
stateExitedEventDetails
    :: Text -- ^ 'seedName'
    -> StateExitedEventDetails
stateExitedEventDetails pName_
  = StateExitedEventDetails'{_seedOutput = Nothing,
                             _seedName = pName_}

-- | The JSON output data of the state.
seedOutput :: Lens' StateExitedEventDetails (Maybe Text)
seedOutput = lens _seedOutput (\ s a -> s{_seedOutput = a}) . mapping _Sensitive

-- | The name of the state. A name must /not/ contain:     * white space     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ ) To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
seedName :: Lens' StateExitedEventDetails Text
seedName = lens _seedName (\ s a -> s{_seedName = a})

instance FromJSON StateExitedEventDetails where
        parseJSON
          = withObject "StateExitedEventDetails"
              (\ x ->
                 StateExitedEventDetails' <$>
                   (x .:? "output") <*> (x .: "name"))

instance Hashable StateExitedEventDetails where

instance NFData StateExitedEventDetails where
