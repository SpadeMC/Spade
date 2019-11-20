module Minecraft.NBT (NBT (..)) where

import           Data.Int
import           Data.List     (intercalate)
import           Data.Map.Lazy (Map, foldrWithKey)

{-|
  Data type for a Minecraft Named Binary Tag value.
  The show instance converts it to SNBT.
  Reference: https://minecraft.gamepedia.com/NBT_format
-}
data NBT
  = NBTByte Int8
  | NBTShort Int16
  | NBTInt Int32
  | NBTLong Int64
  | NBTFloat Float
  | NBTDouble Double
  | NBTByteArray [Int8]
  | NBTString String
  | NBTList [NBT]
  | NBTCompound (Map String NBT)
  | NBTIntArray [Int32]
  | NBTLongArray [Int64]

instance Show NBT where
  show (NBTByte b)       = show b ++ "b"
  show (NBTShort s)      = show s ++ "s"
  show (NBTInt i)        = show i
  show (NBTLong l)       = show l ++ "l"
  show (NBTFloat f)      = show f ++ "f"
  show (NBTDouble d)     = show d ++ "d"
  show (NBTByteArray []) = "[B;]"
  show (NBTString s)     = show s
  show (NBTByteArray xs) = "[B;" ++ intercalate "," ((show . NBTByte) <$> xs) ++  "]"
  show (NBTList xs)      = "[" ++ intercalate "," (show <$> xs) ++ "]"
  show (NBTCompound m)   = "{" ++ intercalate "," (foldrWithKey (\k v acc -> (k ++ ":"  ++ show v) : acc) [] m) ++ "}"
  show (NBTIntArray xs)  = "[I;" ++ intercalate "," ((show . NBTInt) <$> xs) ++  "]"
  show (NBTLongArray xs) = "[L;" ++ intercalate "," ((show . NBTLong) <$> xs) ++  "]"
