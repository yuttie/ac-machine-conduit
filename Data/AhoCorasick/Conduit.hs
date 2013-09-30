module Data.AhoCorasick.Conduit
  ( conduitACMachineText
  ) where

import           Data.AhoCorasick  (ACMachine, Match(..), root, step)
import           Data.Conduit      (Conduit, await)
import qualified Data.Conduit.List as CL
import           Data.Text         (Text)
import qualified Data.Text         as T


conduitACMachineText :: Monad m => ACMachine Char v -> Conduit Text m (Match v)
conduitACMachineText acm = go root 1 T.empty
  where
    go s i buf = case T.uncons buf of
        Nothing -> await >>= maybe (return ()) (go s i)
        Just (c, buf') -> do
            let (s', vs) = step acm c s
            CL.sourceList $ map toMatch vs
            let i' = i + 1
            i' `seq` go s' i' buf'
          where
            toMatch (l, v) = Match { matchPos = i - l + 1, matchValue = v }
