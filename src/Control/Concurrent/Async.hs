
-- | A module for asynchronous task spawning. It doesn't operate on callbacks,
-- but rather a fork-join methodology. It mostly involves the composition of IO
-- actions and exposes a single type, 'Future', for the sake of joining an
-- asynchronously running task.
--
-- It might be worthwhile extending the functionality of the 'asyncAny' and
-- 'asyncEither' functions to respond differently to failure. Rather than
-- accepting the failure of the first value, they might instead continue to
-- wait until a value succeeds, and otherwise fail.
--
-- It might also be more correct to create a new exception type that
-- concatenates multiple exceptions together so that 'asyncAll' and 'asyncBoth'
-- can report all of the different failures that occurred, rather than just the
-- first one in the list.

module Control.Concurrent.Async (

    -- * Future type
      Future

    -- * Forking and joining tasks
    , runTask
    , runAsync
    , joinTask

    -- * Concatenating actions asynchronously
    , asyncAll
    , asyncAll_
    , asyncAny
    , asyncBoth
    , asyncEither

    ) where

import Control.Concurrent ( ThreadId, forkIO )
import Control.Concurrent.MVar
import Control.Exception ( try )
import Control.Monad ( (>=>), void )


-- | A forked task resulting from a call to 'runTask'. Can be synchronised with
-- the current thread with 'joinTask'.
data Future a = Future (MVar (Either IOError a))


-- | Forks a given action into a task and immediately returns with a 'Future'
-- which can be used to join the task at some point in the future.
runTask :: IO a -> IO (Future a)
runTask action = do
    mvar <- newEmptyMVar
    _ <- forkIO $ try action >>= putMVar mvar
    return $ Future mvar

-- | Forks a given action and immediately returns, ignoring the result.
runAsync :: IO a -> IO ()
runAsync = void . fork

-- | Blocks until the given future is filled, at which point it returns the
-- value contained in the future. It is safe to join in multiple places, as
-- this function will return immediately if the task has completed, and tasks
-- cannot be restarted.
--
-- If the task fails, then this function will fail as well (not block forever).
joinTask :: Future a -> IO a
joinTask (Future mvar) = readMVar mvar >>= either ioError return


-- | Given a list of IO actions, this function will compose them into a single
-- IO action that executes each element in the list asynchronously and then
-- returns a list containing the results of each. It is essentially a parallel
-- version of 'sequence'.
--
-- Note that the action will still block until all of the executions have
-- completed. Use 'runTask' or 'runAsync' to prevent this from happening.
--
-- If any of the actions fail, then the whole function will fail as well.
asyncAll :: [IO a] -> IO [a]
asyncAll = mapM runTask >=> mapM joinTask

-- | A variant of 'asyncAll' that does not collect the results of the actions.
-- It will still fail if any of the actions fail.
asyncAll_ :: [IO a] -> IO ()
asyncAll_ = mapM runTask >=> mapM_ joinTask

-- | Given a list of IO actions, this function will compose them into a single
-- IO action that executes each element in the list asynchronously and then
-- return the result of the first one to finish. The remaining actions will
-- complete asynchronously, but their values will be ignored.
--
-- Note that the action will still block until one of the executions has
-- completed. Use 'runTask' or 'runAsync' to prevent this from happening.
--
-- If the fastest action fails, then the whole function will fail as well.
asyncAny :: [IO a] -> IO a
asyncAny tasks = do
    mvar <- newEmptyMVar
    mapM_ (fork . (try >=> tryPutMVar mvar)) tasks
    joinTask $ Future mvar

-- | Given two IO actions, this function will compose them into a single IO
-- action that executes both asynchronously and then returns a pair containing
-- the results of each. Equivalent to asyncAll in general functionality, but
-- allows actions of different types to be composed.
--
-- Note that it actually runs the first action on the current thread, rather
-- than spawning a new thread for it. This doesn't affect the semantics.
asyncBoth :: IO a -> IO b -> IO (a, b)
asyncBoth a b = do
    bT <- runTask b
    a' <- a
    b' <- joinTask bT
    return (a', b')

-- | Given two IO actions, this function will compose them into a single IO
-- action that executes both asynchronously and then returns the result of
-- which one completed first. Equivalent to asyncAny in general functionality,
-- but allows actions of different types to be composed.
asyncEither :: IO a -> IO b -> IO (Either a b)
asyncEither a b = do
    mvar <- newEmptyMVar
    let async v f = fork $ try v >>= tryPutMVar mvar . either Left (Right . f)
    _ <- async a Left
    _ <- async b Right
    joinTask $ Future mvar


-- | A helper function which ignores the return values of the input.
fork :: IO a -> IO ThreadId
fork = forkIO . void
