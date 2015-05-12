{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
-- no indents here or else

module Parser (plex, Expression(..), Condition(..)) where

import Prelude hiding (head, tail)
import Data.Char
import Lexer (lexer, Token(..))
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

action_0 (12#) = happyShift action_11
action_0 (14#) = happyShift action_6
action_0 (15#) = happyShift action_7
action_0 (16#) = happyShift action_8
action_0 (20#) = happyShift action_12
action_0 (4#) = happyGoto action_9
action_0 (5#) = happyGoto action_2
action_0 (7#) = happyGoto action_3
action_0 (8#) = happyGoto action_4
action_0 (9#) = happyGoto action_10
action_0 x = happyTcHack x happyReduce_7

action_1 (14#) = happyShift action_6
action_1 (15#) = happyShift action_7
action_1 (16#) = happyShift action_8
action_1 (5#) = happyGoto action_2
action_1 (7#) = happyGoto action_3
action_1 (8#) = happyGoto action_4
action_1 (9#) = happyGoto action_5
action_1 x = happyTcHack x happyFail

action_2 x = happyTcHack x happyReduce_1

action_3 (14#) = happyShift action_6
action_3 (15#) = happyShift action_7
action_3 (16#) = happyShift action_8
action_3 (5#) = happyGoto action_19
action_3 (7#) = happyGoto action_3
action_3 (8#) = happyGoto action_4
action_3 (9#) = happyGoto action_5
action_3 x = happyTcHack x happyReduce_8

action_4 x = happyTcHack x happyReduce_13

action_5 x = happyTcHack x happyReduce_14

action_6 (15#) = happyShift action_18
action_6 x = happyTcHack x happyFail

action_7 x = happyTcHack x happyReduce_17

action_8 x = happyTcHack x happyReduce_15

action_9 (11#) = happyShift action_17
action_9 (23#) = happyAccept
action_9 x = happyTcHack x happyFail

action_10 (10#) = happyShift action_16
action_10 x = happyTcHack x happyReduce_14

action_11 (12#) = happyShift action_11
action_11 (14#) = happyShift action_6
action_11 (15#) = happyShift action_7
action_11 (16#) = happyShift action_8
action_11 (20#) = happyShift action_12
action_11 (4#) = happyGoto action_15
action_11 (5#) = happyGoto action_2
action_11 (7#) = happyGoto action_3
action_11 (8#) = happyGoto action_4
action_11 (9#) = happyGoto action_10
action_11 x = happyTcHack x happyReduce_7

action_12 (14#) = happyShift action_6
action_12 (15#) = happyShift action_7
action_12 (16#) = happyShift action_8
action_12 (6#) = happyGoto action_13
action_12 (7#) = happyGoto action_14
action_12 (8#) = happyGoto action_4
action_12 (9#) = happyGoto action_5
action_12 x = happyTcHack x happyFail

action_13 (22#) = happyShift action_27
action_13 x = happyTcHack x happyFail

action_14 (17#) = happyShift action_24
action_14 (18#) = happyShift action_25
action_14 (19#) = happyShift action_26
action_14 x = happyTcHack x happyFail

action_15 (11#) = happyShift action_17
action_15 (13#) = happyShift action_23
action_15 x = happyTcHack x happyFail

action_16 (14#) = happyShift action_6
action_16 (15#) = happyShift action_7
action_16 (16#) = happyShift action_8
action_16 (7#) = happyGoto action_22
action_16 (8#) = happyGoto action_4
action_16 (9#) = happyGoto action_5
action_16 x = happyTcHack x happyFail

action_17 (12#) = happyShift action_11
action_17 (14#) = happyShift action_6
action_17 (15#) = happyShift action_7
action_17 (16#) = happyShift action_8
action_17 (20#) = happyShift action_12
action_17 (4#) = happyGoto action_21
action_17 (5#) = happyGoto action_2
action_17 (7#) = happyGoto action_3
action_17 (8#) = happyGoto action_4
action_17 (9#) = happyGoto action_10
action_17 x = happyTcHack x happyReduce_7

action_18 (14#) = happyShift action_20
action_18 x = happyTcHack x happyFail

action_19 x = happyTcHack x happyReduce_9

action_20 x = happyTcHack x happyReduce_16

action_21 x = happyTcHack x happyReduce_3

action_22 x = happyTcHack x happyReduce_2

action_23 x = happyTcHack x happyReduce_6

action_24 (14#) = happyShift action_6
action_24 (15#) = happyShift action_7
action_24 (16#) = happyShift action_8
action_24 (7#) = happyGoto action_31
action_24 (8#) = happyGoto action_4
action_24 (9#) = happyGoto action_5
action_24 x = happyTcHack x happyFail

action_25 (14#) = happyShift action_6
action_25 (15#) = happyShift action_7
action_25 (16#) = happyShift action_8
action_25 (7#) = happyGoto action_30
action_25 (8#) = happyGoto action_4
action_25 (9#) = happyGoto action_5
action_25 x = happyTcHack x happyFail

action_26 (14#) = happyShift action_6
action_26 (15#) = happyShift action_7
action_26 (16#) = happyShift action_8
action_26 (7#) = happyGoto action_29
action_26 (8#) = happyGoto action_4
action_26 (9#) = happyGoto action_5
action_26 x = happyTcHack x happyFail

action_27 (12#) = happyShift action_11
action_27 (14#) = happyShift action_6
action_27 (15#) = happyShift action_7
action_27 (16#) = happyShift action_8
action_27 (20#) = happyShift action_12
action_27 (4#) = happyGoto action_28
action_27 (5#) = happyGoto action_2
action_27 (7#) = happyGoto action_3
action_27 (8#) = happyGoto action_4
action_27 (9#) = happyGoto action_10
action_27 x = happyTcHack x happyReduce_7

action_28 (11#) = happyShift action_17
action_28 (21#) = happyShift action_32
action_28 x = happyTcHack x happyReduce_4

action_29 x = happyTcHack x happyReduce_12

action_30 x = happyTcHack x happyReduce_11

action_31 x = happyTcHack x happyReduce_10

action_32 (12#) = happyShift action_11
action_32 (14#) = happyShift action_6
action_32 (15#) = happyShift action_7
action_32 (16#) = happyShift action_8
action_32 (20#) = happyShift action_12
action_32 (4#) = happyGoto action_33
action_32 (5#) = happyGoto action_2
action_32 (7#) = happyGoto action_3
action_32 (8#) = happyGoto action_4
action_32 (9#) = happyGoto action_10
action_32 x = happyTcHack x happyReduce_7

action_33 (11#) = happyShift action_17
action_33 x = happyTcHack x happyReduce_5

happyReduce_1 = happySpecReduce_1  4# happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (ComArgs (head happy_var_1) (tail happy_var_1)
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4# happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (Assign (fromLit happy_var_1) happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4# happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4# 4# happyReduction_4
happyReduction_4 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IfElse happy_var_2 happy_var_4 Nothing
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6# 4# happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IfElse happy_var_2 happy_var_4 (Just happy_var_6)
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  4# happyReduction_6
happyReduction_6 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  4# happyReduction_7
happyReduction_7  =  HappyAbsSyn4
		 (Empty
	)

happyReduce_8 = happySpecReduce_1  5# happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 ([(fromLit happy_var_1)]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  5# happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 ((fromLit happy_var_1) : happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6# happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6# happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  6# happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Eql happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7# happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  7# happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8# happyReduction_15
happyReduction_15 (HappyTerminal (TokInt happy_var_1))
	 =  HappyAbsSyn8
		 (IntLiteral happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9# happyReduction_16
happyReduction_16 _
	(HappyTerminal (TokWord happy_var_2))
	_
	 =  HappyAbsSyn9
		 (StrLiteral happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  9# happyReduction_17
happyReduction_17 (HappyTerminal (TokWord happy_var_1))
	 =  HappyAbsSyn9
		 (StrLiteral happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 23# 23# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokAssign -> cont 10#;
	TokSemi -> cont 11#;
	TokLP -> cont 12#;
	TokRP -> cont 13#;
	TokDQ -> cont 14#;
	TokWord happy_dollar_dollar -> cont 15#;
	TokInt happy_dollar_dollar -> cont 16#;
	TokGT -> cont 17#;
	TokLT -> cont 18#;
	TokEql -> cont 19#;
	TokIf -> cont 20#;
	TokElse -> cont 21#;
	TokThen -> cont 22#;
	_ -> happyError' (tk:tks)
	}

happyError_ 23# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


fromLit :: Expression -> String
fromLit e = case e of
                StrLiteral s -> s
                IntLiteral i -> show i

tail :: [a] -> [a]
tail [] = []
tail l = drop 1 l

head [] = "not a real command, or a real solution"
head l = take 1 l !! 0

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- todo seperate Expression into 2 datatypes: Expression and Constant

data Expression
    = ComArgs String [String]  -- if list empty, treat as var ref!
    | Assign String Expression
    | Seq Expression Expression
    | IfElse Condition Expression (Maybe Expression)  -- else clause optional
    | IntLiteral Int
    | StrLiteral String
    | Empty
    deriving Show

data Condition
    = Gt Expression Expression
    | Lt Expression Expression
    | Eql Expression Expression
      deriving Show

plex :: String -> Expression
plex = (parse . lexer)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 1#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Happy_GHC_Exts.Int# ->                    -- token number
         Happy_GHC_Exts.Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (1# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 1# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  1# tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action 1# 1# tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action 1# 1# tk (HappyState (action)) sts ( (HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
