{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Framework
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Framework (
  Framework (
    ..
    , Keras
    , Mxnet
    , Onnx
    , Pytorch
    , Tensorflow
    , Tflite
    , Xgboost
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Framework = Framework' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Keras :: Framework
pattern Keras = Framework' "KERAS"

pattern Mxnet :: Framework
pattern Mxnet = Framework' "MXNET"

pattern Onnx :: Framework
pattern Onnx = Framework' "ONNX"

pattern Pytorch :: Framework
pattern Pytorch = Framework' "PYTORCH"

pattern Tensorflow :: Framework
pattern Tensorflow = Framework' "TENSORFLOW"

pattern Tflite :: Framework
pattern Tflite = Framework' "TFLITE"

pattern Xgboost :: Framework
pattern Xgboost = Framework' "XGBOOST"

{-# COMPLETE
  Keras,
  Mxnet,
  Onnx,
  Pytorch,
  Tensorflow,
  Tflite,
  Xgboost,
  Framework' #-}

instance FromText Framework where
    parser = (Framework' . mk) <$> takeText

instance ToText Framework where
    toText (Framework' ci) = original ci

-- | Represents an enum of /known/ $Framework.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Framework where
    toEnum i = case i of
        0 -> Keras
        1 -> Mxnet
        2 -> Onnx
        3 -> Pytorch
        4 -> Tensorflow
        5 -> Tflite
        6 -> Xgboost
        _ -> (error . showText) $ "Unknown index for Framework: " <> toText i
    fromEnum x = case x of
        Keras -> 0
        Mxnet -> 1
        Onnx -> 2
        Pytorch -> 3
        Tensorflow -> 4
        Tflite -> 5
        Xgboost -> 6
        Framework' name -> (error . showText) $ "Unknown Framework: " <> original name

-- | Represents the bounds of /known/ $Framework.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Framework where
    minBound = Keras
    maxBound = Xgboost

instance Hashable     Framework
instance NFData       Framework
instance ToByteString Framework
instance ToQuery      Framework
instance ToHeader     Framework

instance ToJSON Framework where
    toJSON = toJSONText

instance FromJSON Framework where
    parseJSON = parseJSONText "Framework"
