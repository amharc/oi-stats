module Award where

import Prelude
import Database.Persist.Class
import Database.Persist.Sql
import qualified Data.Text as T

data Award = LaureateI | LaureateII | LaureateIII | FinalistHonMen | Finalist
    deriving (Show, Read, Eq, Ord, Enum)

instance PersistField Award where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 i) = Right $ toEnum $ fromIntegral i
    fromPersistValue x = Left $ T.pack $ "Expected Award, received: " ++ show x

instance PersistFieldSql Award where
    sqlType _ = SqlInt32
