{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Layer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.Layer where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
--
--
-- /See:/ 'layer' smart constructor.
data Layer = Layer'{_lARN :: !(Maybe Text),
                    _lCodeSize :: !(Maybe Integer)}
               deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Layer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lARN' - The Amazon Resource Name (ARN) of the function layer.
--
-- * 'lCodeSize' - The size of the layer archive in bytes.
layer
    :: Layer
layer = Layer'{_lARN = Nothing, _lCodeSize = Nothing}

-- | The Amazon Resource Name (ARN) of the function layer.
lARN :: Lens' Layer (Maybe Text)
lARN = lens _lARN (\ s a -> s{_lARN = a})

-- | The size of the layer archive in bytes.
lCodeSize :: Lens' Layer (Maybe Integer)
lCodeSize = lens _lCodeSize (\ s a -> s{_lCodeSize = a})

instance FromJSON Layer where
        parseJSON
          = withObject "Layer"
              (\ x ->
                 Layer' <$> (x .:? "Arn") <*> (x .:? "CodeSize"))

instance Hashable Layer where

instance NFData Layer where
