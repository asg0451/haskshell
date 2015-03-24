module Main where
import           Control.Monad            (void)
import           Control.Monad.State.Lazy
import           Data.Maybe               (fromJust, fromMaybe)
import           Data.Monoid
import           Parser
import           System.Console.Readline  (readline)
import           System.IO
import qualified System.Posix.Process     as Proc

type Eval = StateT Env IO   -- partial type, takes an a value
type Env  = [(String, String)]
data Val  = Str String
          | Null
            deriving Show
instance Monoid Val where
    mempty = Null
    (Str a) `mappend` (Str b) = Str $ unlines [b,a] -- only commands give output right?
    (Str a) `mappend` Null = Str a
    Null    `mappend` x = x


-- are all data types strings like in bash?
-- for now, making everything strings.
eval :: Expression -> Eval Val
eval expr = case expr of
              IntLiteral i -> return $ Str $ show i
              StrLiteral s -> return $ Str s
              Assign v (IntLiteral i) -> do
                        modify $ \s -> (v, show i) : s
                        return $ Str $ show i
              Assign v (StrLiteral s) -> do
                        modify $ \state -> (v, s) : state
                        return $ Str s
              ComArgs c [] -> do     -- this is wrong. see TODO's
                        env <- get   -- add state to env for process
                        case lookup c env of
                          Just val -> return $ Str val
                          Nothing  -> return Null
              ComArgs c as -> do
                        env <- get   -- add state to env for process
                        let args = map (\a -> fromMaybe a (lookup a env)) as
                        liftIO $ Proc.forkProcess
                                   $ Proc.executeFile c True args $ Just env
                        return Null  -- return return value of command eventually
              Seq a b -> do ra <- eval a
                            rb <- eval b
                            return $ ra `mappend` rb
              IfElse c e1 (Just e2) -> do
                        b <- evalCond c
                        if b
                           then eval e1
                           else eval e2
              IfElse c e Nothing -> do
                        b <- evalCond c
                        if b
                           then eval e
                           else return Null

evalCond :: Condition -> Eval Bool
evalCond (Gt (IntLiteral a) (IntLiteral b)) = return $ a > b
evalCond (Gt (StrLiteral a) (StrLiteral b)) = return $ (length a) > (length b)

evalCond (Lt (IntLiteral a) (IntLiteral b)) = return $ a < b
evalCond (Lt (StrLiteral a) (StrLiteral b)) = return $ (length a) < (length b)

evalCond (Eql (IntLiteral a) (IntLiteral b)) = return $ a == b
evalCond (Eql (StrLiteral a) (StrLiteral b)) = return $ (length a) == (length b)
-- unfinished

main = void
       $ iterateM_ (\prev -> do line <- readline ">> "
                                case line of
                                  Just l -> do let ast = plex l
                                               out <- runStateT (eval ast) (fromJust prev)
                                               -- prev gets checked for Nothingness
                                               -- in iterateM_
                                               let laststate = snd out
                                               print ast
                                               print out
                                               return $ Just laststate
                                  Nothing -> return Nothing
                   ) $ Just []

iterateM_ :: (Maybe a -> IO (Maybe a)) -> Maybe a -> IO (Maybe b)
iterateM_ f = g
    where g x = case x of
                  Nothing -> return Nothing
                  Just z  -> f (Just z) >>= g



-- main = do
--   hSetBuffering stdin LineBuffering
--   l <- getLine -- readline ">> "
--   l2 <- getLine -- readline ">> "
--   let ast = plex l
--   let ast2 = plex l2
--   out <- runStateT ((eval ast) >> (eval ast2)) [] -- evalStateT suppresses state
--   putStrLn $ show ast
--   putStrLn $ show out
--
--   return ()
