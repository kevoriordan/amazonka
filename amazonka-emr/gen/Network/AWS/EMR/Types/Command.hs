{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Command
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Command where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An entity describing an executable that runs on a cluster.
--
--
--
-- /See:/ 'command' smart constructor.
data Command = Command'{_cArgs :: !(Maybe [Text]),
                        _cScriptPath :: !(Maybe Text),
                        _cName :: !(Maybe Text)}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cArgs' - Arguments for Amazon EMR to pass to the command for execution.
--
-- * 'cScriptPath' - The Amazon S3 location of the command script.
--
-- * 'cName' - The name of the command.
command
    :: Command
command
  = Command'{_cArgs = Nothing, _cScriptPath = Nothing,
             _cName = Nothing}

-- | Arguments for Amazon EMR to pass to the command for execution.
cArgs :: Lens' Command [Text]
cArgs = lens _cArgs (\ s a -> s{_cArgs = a}) . _Default . _Coerce

-- | The Amazon S3 location of the command script.
cScriptPath :: Lens' Command (Maybe Text)
cScriptPath = lens _cScriptPath (\ s a -> s{_cScriptPath = a})

-- | The name of the command.
cName :: Lens' Command (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "Args" .!= mempty) <*> (x .:? "ScriptPath")
                     <*> (x .:? "Name"))

instance Hashable Command where

instance NFData Command where
