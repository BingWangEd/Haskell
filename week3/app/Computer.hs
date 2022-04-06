-- module Computer ( Computer
--                 , mkComputer )
-- module Computer (Computer(..)) This would output the types as well.
-- export list. If you have an export list, you limit what you export. Otherwise you export everything

module Computer
where

data Key
  = Ctrl
  | Alt
  | Del
  | Option
  | Reset
  | PowerButton
  deriving Show

data Apple
  = IIc | IIe | GS
  deriving (Show, Eq)

data IBM
  = PC | PCJr | XT | AT
  deriving (Show, Eq)

data Commodore
  = C64 | C128
  deriving (Show, Eq)

data Computer
  = Apple
  | IBM
  | Commodore
  deriving Show

-- type classes
-- type class definition: define types
class Rebootable a where
  rebootKeys :: a -> [Key]
  -- anotherFunction :: a -> add
  -- aSpecialValue :: a

-- in instance you give definition for the value
instance Rebootable IBM where
  rebootKeys _ = [Ctrl, Alt, Del]

instance Rebootable Apple where
  rebootKeys _ = [Ctrl, Option, Reset]

instance Rebootable Commodore where
  rebootKeys C64 = [PowerButton]
  rebootKeys C128 = [Reset]

mkComputer :: Char -> Computer
mkComputer 'a' = Apple
mkComputer 'i' = IBM
mkComputer _ = Commodore

-- rebootKeys AT => [Ctrl,Alt,Del]