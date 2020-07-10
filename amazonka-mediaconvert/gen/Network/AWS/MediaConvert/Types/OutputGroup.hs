{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.OutputGroup where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Output
import Network.AWS.MediaConvert.Types.OutputGroupSettings
import Network.AWS.Prelude

-- | Group of outputs
--
-- /See:/ 'outputGroup' smart constructor.
data OutputGroup = OutputGroup'{_ogOutputGroupSettings
                                :: !(Maybe OutputGroupSettings),
                                _ogOutputs :: !(Maybe [Output]),
                                _ogCustomName :: !(Maybe Text),
                                _ogName :: !(Maybe Text)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogOutputGroupSettings' - Output Group settings, including type
--
-- * 'ogOutputs' - This object holds groups of encoding settings, one group of settings per output.
--
-- * 'ogCustomName' - Use Custom Group Name (CustomName) to specify a name for the output group. This value is displayed on the console and can make your job settings JSON more human-readable. It does not affect your outputs. Use up to twelve characters that are either letters, numbers, spaces, or underscores.
--
-- * 'ogName' - Name of the output group
outputGroup
    :: OutputGroup
outputGroup
  = OutputGroup'{_ogOutputGroupSettings = Nothing,
                 _ogOutputs = Nothing, _ogCustomName = Nothing,
                 _ogName = Nothing}

-- | Output Group settings, including type
ogOutputGroupSettings :: Lens' OutputGroup (Maybe OutputGroupSettings)
ogOutputGroupSettings = lens _ogOutputGroupSettings (\ s a -> s{_ogOutputGroupSettings = a})

-- | This object holds groups of encoding settings, one group of settings per output.
ogOutputs :: Lens' OutputGroup [Output]
ogOutputs = lens _ogOutputs (\ s a -> s{_ogOutputs = a}) . _Default . _Coerce

-- | Use Custom Group Name (CustomName) to specify a name for the output group. This value is displayed on the console and can make your job settings JSON more human-readable. It does not affect your outputs. Use up to twelve characters that are either letters, numbers, spaces, or underscores.
ogCustomName :: Lens' OutputGroup (Maybe Text)
ogCustomName = lens _ogCustomName (\ s a -> s{_ogCustomName = a})

-- | Name of the output group
ogName :: Lens' OutputGroup (Maybe Text)
ogName = lens _ogName (\ s a -> s{_ogName = a})

instance FromJSON OutputGroup where
        parseJSON
          = withObject "OutputGroup"
              (\ x ->
                 OutputGroup' <$>
                   (x .:? "outputGroupSettings") <*>
                     (x .:? "outputs" .!= mempty)
                     <*> (x .:? "customName")
                     <*> (x .:? "name"))

instance Hashable OutputGroup where

instance NFData OutputGroup where

instance ToJSON OutputGroup where
        toJSON OutputGroup'{..}
          = object
              (catMaybes
                 [("outputGroupSettings" .=) <$>
                    _ogOutputGroupSettings,
                  ("outputs" .=) <$> _ogOutputs,
                  ("customName" .=) <$> _ogCustomName,
                  ("name" .=) <$> _ogName])
