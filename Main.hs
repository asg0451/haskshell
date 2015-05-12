module Main where
import           Control.Monad.State.Lazy
import           Data.Maybe               (fromJust, fromMaybe)
import           Parser
import           System.Console.Readline  (readline, addHistory)
import           System.Process hiding (proc)


type Eval = StateT Env IO   -- partial type, takes an a value
type Env  = [(String, String)]
data Val  = Str String
          | Null
            deriving Show

instance Monoid Val where  -- todo: make it so that failed commands have some authority
    mempty = Null
    (Str a) `mappend` (Str b) = Str $ unlines [b,a] -- only commands give output right?
    (Str a) `mappend` Null = Str a
    Null    `mappend` x = x


-- for now, making everything strings.
eval :: Expression -> Eval Val
eval expr = case expr of
              IntLiteral i -> return $ Str $ show i
              StrLiteral s -> return $ Str s
              Assign v (IntLiteral i) -> do
                        modify $ \s -> (v, show i) : s  -- add name, value pair to env
                        return $ Str $ show i
              Assign v (StrLiteral s) -> do
                        modify $ \st -> (v, s) : st
                        return $ Str s
              ComArgs c [] -> do     -- this is wrong. see TODO's
                        env <- get   -- add state to env for process
                        case lookup c env of
                          Just val -> return $ Str val
                          Nothing  -> return Null
              ComArgs c as -> do
                        env <- get   -- add state to env for process
                        let args = map (\a -> fromMaybe a (lookup a env)) as
                        (_, _, _, h) <- liftIO $ createProcess $ proc env c args
                        retVal <- liftIO $ waitForProcess h  -- synchronous
                        return $ Str $ show retVal
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
              Empty -> return Null


evalCond :: Condition -> Eval Bool
evalCond (Gt (IntLiteral a) (IntLiteral b)) = return $ a > b
evalCond (Gt (StrLiteral a) (StrLiteral b)) = return $ length a > length b

evalCond (Lt (IntLiteral a) (IntLiteral b)) = return $ a < b
evalCond (Lt (StrLiteral a) (StrLiteral b)) = return $ length a < length b

evalCond (Eql (IntLiteral a) (IntLiteral b)) = return $ a == b
evalCond (Eql (StrLiteral a) (StrLiteral b)) = return $ length a == length b
-- unfinished

main :: IO ()
main = void
       $ iterateM_ (\prev -> do line <- readline ">> "
                                case line of
                                 Just l -> do addHistory l  -- for readline
                                              let ast = plex l
                                              print ast
                                              out <- runStateT (eval ast) (fromJust prev)
                                              print out
                                              let laststate = snd out
                                              return $ Just laststate
                                 Nothing -> return Nothing
                   ) $ Just [("PATH", ".:/bin:/usr/bin"), ("test", "fish")] -- initial env is empty for now

iterateM_ :: (Maybe a -> IO (Maybe a)) -> Maybe a -> IO (Maybe b)
iterateM_ f = g
    where g x = case x of
                  Nothing -> return Nothing
                  Just z  -> f (Just z) >>= g

proc :: Env -> FilePath -> [String] -> CreateProcess
proc env cmd args = CreateProcess { cmdspec = RawCommand cmd args
                                  , cwd = Just "."
                                  , env = Just env
                                  , std_in = Inherit
                                  , std_out = Inherit
                                  , std_err = Inherit
                                  , close_fds = False
                                  , create_group = False
                                  , delegate_ctlc = False
                                  }
