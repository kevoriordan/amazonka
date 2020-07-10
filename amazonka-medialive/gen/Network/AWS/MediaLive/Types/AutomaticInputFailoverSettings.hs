{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AutomaticInputFailoverSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputPreference
import Network.AWS.Prelude

-- | The settings for Automatic Input Failover.
--
-- /See:/ 'automaticInputFailoverSettings' smart constructor.
data AutomaticInputFailoverSettings = AutomaticInputFailoverSettings'{_aifsInputPreference
                                                                      ::
                                                                      !(Maybe
                                                                          InputPreference),
                                                                      _aifsSecondaryInputId
                                                                      :: !Text}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'AutomaticInputFailoverSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aifsInputPreference' - Input preference when deciding which input to make active when a previously failed input has recovered.
--
-- * 'aifsSecondaryInputId' - The input ID of the secondary input in the automatic input failover pair.
automaticInputFailoverSettings
    :: Text -- ^ 'aifsSecondaryInputId'
    -> AutomaticInputFailoverSettings
automaticInputFailoverSettings pSecondaryInputId_
  = AutomaticInputFailoverSettings'{_aifsInputPreference
                                      = Nothing,
                                    _aifsSecondaryInputId = pSecondaryInputId_}

-- | Input preference when deciding which input to make active when a previously failed input has recovered.
aifsInputPreference :: Lens' AutomaticInputFailoverSettings (Maybe InputPreference)
aifsInputPreference = lens _aifsInputPreference (\ s a -> s{_aifsInputPreference = a})

-- | The input ID of the secondary input in the automatic input failover pair.
aifsSecondaryInputId :: Lens' AutomaticInputFailoverSettings Text
aifsSecondaryInputId = lens _aifsSecondaryInputId (\ s a -> s{_aifsSecondaryInputId = a})

instance FromJSON AutomaticInputFailoverSettings
         where
        parseJSON
          = withObject "AutomaticInputFailoverSettings"
              (\ x ->
                 AutomaticInputFailoverSettings' <$>
                   (x .:? "inputPreference") <*>
                     (x .: "secondaryInputId"))

instance Hashable AutomaticInputFailoverSettings
         where

instance NFData AutomaticInputFailoverSettings where

instance ToJSON AutomaticInputFailoverSettings where
        toJSON AutomaticInputFailoverSettings'{..}
          = object
              (catMaybes
                 [("inputPreference" .=) <$> _aifsInputPreference,
                  Just ("secondaryInputId" .= _aifsSecondaryInputId)])
