data SegTree m = Empty
    | Node m (Int, Int) (SegTree m) (SegTree m)
    deriving (Show)

buildTree :: Monoid m => (Int -> m) -> (Int, Int) -> SegTree m
buildTree get (l, r)
    | l > r = Empty
    | l == r = 
        let v = get l
        in Node v (l, r) Empty Empty
    | otherwise =
        let mid = (l + r) `div` 2
            left = buildTree get (l, mid)
            right = buildTree get (mid + 1, r)
            v = getNodeValue left <> getNodeValue right
        in Node v (l,r) left right

query :: Monoid m => (Int, Int) -> SegTree m -> m
query (l, r) (Node m (start, end) sub1 sub2)
    | l > end || r < start = mempty
    | l <= start && r >= end = m
    | otherwise = query (l, r) sub1 <> query (l, r) sub2
query _ _ = mempty

getNodeValue :: Monoid m => SegTree m -> m
getNodeValue (Node m _ _ _) = m
getNodeValue Empty = mempty

update :: Monoid m => Int -> m -> SegTree m -> SegTree m
update i v (Node _ (start, end) sub1 sub2) 
    | start == end = Node v (start, end) sub1 sub2 
    | i <= mid = 
        let new_sub1 = update i v sub1 
        in Node (getNodeValue new_sub1 <> getNodeValue sub2) (start, end) new_sub1 sub2 
    | otherwise =
        let new_sub2 = update i v sub2 
        in Node (getNodeValue sub1 <> getNodeValue new_sub2) (start, end) sub1 new_sub2 
    where
        mid = (start + end) `div` 2
update _ _ _ = Empty

newtype Val = Val Int
    deriving (Show)

fromVal :: Val -> Int
fromVal (Val x) = x

instance Semigroup Val where
    (<>) (Val a) (Val b) = Val (a + b)

instance Monoid Val where
    mempty = Val 0

main :: IO ()
main = do
    [n, q] <- map read . words <$> getLine
    arr <- map read . words <$> getLine
    
    let get i = Val (arr !! i)
        tree = buildTree get (0, n - 1)

    processStaticQueries q tree

processDynamicQueries :: Int -> SegTree Val -> IO ()
processDynamicQueries 0 _ = return ()
processDynamicQueriesQueries q tree = do 
    input <- words <$> getLine
    case input of
        ("1":k:u:_) -> do
            let k' = read k - 1
            let u' = Val $ read u
            let newTree = update k' u' tree
            processDynamicQueries (q - 1) newTree
        ("2":a:b:_) -> do
            let a' = read a - 1
            let b' = read b - 1
            print (fromVal $ query (a', b') tree)
            processDynamicQueries (q - 1) tree
        _ -> error "Invalid input"

processStaticQueries :: Int -> SegTree Val -> IO ()
processStaticQueries 0 _ = return ()
processStaticQueries q tree = do
    [a, b] <- map read . words <$> getLine
    print (fromVal $ query (a - 1, b - 1) tree)
    processStaticQueries (q - 1) tree
