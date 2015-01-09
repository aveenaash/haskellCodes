import Data.List
-- import Test.QuickCheck

data SumTree = Null | Node Int SumTree SumTree| Leaf Int
              deriving (Show, Eq)

unbalancedTree= Node 1 (Node 2 (Leaf 4) (Node 5 (Leaf 20) (Leaf 30))) (Node 3 Null Null)
tree1= Node 1 (Node 2 (Leaf 4) (Node 5 (Leaf 20) (Leaf 30))) (Node 3 (Leaf 6) Null)
tree2= Node 15 (Node 9 (Leaf 4) (Leaf 5)) (Node 6 (Leaf 6 ) Null)


sumOfNodes :: SumTree -> Int
sumOfNodes Null = 0
sumOfNodes (Leaf i)=i
sumOfNodes (Node n ltree rtree)= sumOfNodes(ltree)+sumOfNodes(rtree)

checkTree ::SumTree->Bool
checkTree Null=True
checkTree (Leaf _)=True
checkTree (Node n lTree rTree)=((sumOfNodes(lTree)+sumOfNodes(rTree))==n) && checkTree(lTree) && checkTree(rTree)

depthOfTree:: SumTree->Int
depthOfTree Null=0
depthOfTree (Leaf _)=1
depthOfTree (Node n lTree rTree)= 1+ max (depthOfTree lTree) (depthOfTree rTree)

nodeCount :: SumTree ->Int
nodeCount Null=0
nodeCount (Leaf _)=1
nodeCount (Node n lTree rTree)=1 + nodeCount lTree + nodeCount rTree

-- A non-empty binary tree T is balanced if:
-- 1) Left subtree of T is balanced
-- 2) Right subtree of T is balanced
-- 3) The difference between heights of left subtree and right subtree is not more than 1.
balanced :: SumTree -> Bool
balanced Null = True
balanced (Leaf _)= True
balanced (Node n lTree rTree)= (abs ((depthOfTree lTree)-(depthOfTree rTree))<=1) && balanced lTree && balanced rTree

reflect :: SumTree->SumTree
reflect Null=Null
reflect (Leaf i)= Leaf i
reflect (Node n lTree rTree)= Node n (reflect rTree) (reflect lTree)

-- Write a separate function total which will sum all of the leaves in the tree, 
-- and use it to make a quickcheck of the top node total in the tree.(This assumes that there is no value in node
-- and values are only in leaves)

tTotal :: SumTree-> Int
tTotal Null=0
tTotal (Leaf i)=i
tTotal (Node n lTree rTree)= tTotal(lTree)+tTotal(rTree)

--testTreeTotal::

-- Implementation of Set Intersection
intersection :: Eq t => [t] -> [t] -> [t]
intersection a b = [x | x <- a, x `elem` b]

listIntersection :: Eq t => [[t]] -> [t]
listIntersection (x:xs) = foldl intersection x xs

listIntersectionOne (x:xs) = foldl intersect x xs

testIntersection :: Eq t => [[t]]->Bool
testIntersection xs = (listIntersection xs)==(listIntersectionOne xs)

-- checkSetIntersectionVersion= print "Test listIntersection :: listIntersectionOne ::" ++ quickCheck (testIntersection :: Eq t => [[t]]->Bool)
                                

