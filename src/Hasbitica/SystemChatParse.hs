{-# LANGUAGE OverloadedStrings #-}
module Hasbitica.SystemChatParse where

--import Hasbitica.LensStuff
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import Data.Text (Text,pack)
import Control.Applicative ((<|>))
import Control.Monad (void)

data SystemMsg = PlayerAttack 
               { _player :: Text
               , _damageDealt :: Rational
               , _enemy :: Text
               , _partyDamageReceived :: Rational
               }
               | BossDefeated 
               { _enemy :: Text
               }
               | SpellCast 
               { _player :: Text
               , _spell :: Text
               }
  deriving (Show)
{- examples:
`mesa12358 attacks The Basi-List for 5.6 damage, The Basi-List attacks party for 0.0 damage.`
`You defeated The Basi-List! Questing party members receive the rewards of victory.`
`Greggie casts Blessing for the party.`
-}

ident :: Parser a -> Parser Text
ident a = pack <$> AC.manyTill A.anyChar a

playerAttack :: Parser SystemMsg
playerAttack = do
  void $ A.string "`"
  p <- ident (A.string "attacks")
  boss <- ident (A.string " for ")
  dam <- A.rational
  void $ A.string " damage, "
  void $ A.string boss
  void $ A.string " attacks party for "
  pdam <- A.rational
  void $ A.string " damage.`"
  return $ PlayerAttack p dam boss pdam
               
  
bossDefeated :: Parser SystemMsg
bossDefeated = undefined
spellCast :: Parser SystemMsg
spellCast = undefined
systemMsg :: Parser SystemMsg
systemMsg = playerAttack <|> bossDefeated <|> spellCast
