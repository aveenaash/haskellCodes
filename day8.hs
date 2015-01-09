import Prelude hiding (Maybe (..))

data Maybe a= Nothing | Just a
               deriving (Show, Eq)

data SumTree = Node (Maybe Integer) (Maybe SumTree) (Maybe SumTree)
              deriving (Show, Eq)

unbalancedTree= Node (Just 1)(Just(Node (Just 2) (Just(Node (Just 4) Nothing Nothing)) (Just(Node (Just 5) (Just(Node (Just 20) Nothing Nothing)) (Just(Node (Just 30) Nothing Nothing)))))) (Just(Node (Just 3) Nothing Nothing))
tree1 = Node (Just 1)(Just(Node (Just 2) (Just(Node (Just 4) Nothing Nothing)) (Just(Node (Just 5) (Just(Node (Just 20) Nothing Nothing)) (Just(Node (Just 30) Nothing Nothing)))))) (Just(Node (Just 3) (Just(Node (Just 6) Nothing Nothing)) Nothing))
tree2= Node (Just 15) (Just (Node (Just 9) (Just (Node (Just 4) Nothing Nothing)) (Just(Node (Just 5) Nothing Nothing)))) (Just(Node (Just 6) (Just(Node (Just 6 ) Nothing Nothing)) Nothing))

depthOfTree:: SumTree->Int
depthOfTree (Node Nothing Nothing Nothing) = 0 
depthOfTree (Node (Just a) Nothing Nothing)=1
depthOfTree (Node (Just a) (Just b) Nothing)=2
depthOfTree (Node (Just a) (Just lTree) (Just rTree))= 1+ max (depthOfTree lTree) (depthOfTree rTree)

balanced :: SumTree -> Bool
balanced (Node Nothing Nothing Nothing) = True
balanced (Node (Just n) Nothing Nothing)= True
balanced (Node (Just n) (Just m) Nothing)=True
balanced (Node (Just n) (Just lTree) (Just rTree))= (abs ((depthOfTree lTree)-(depthOfTree rTree))<=1) && balanced lTree && balanced rTree

nodeCount :: SumTree ->Int
nodeCount (Node Nothing Nothing Nothing)=0
nodeCount (Node (Just n) Nothing Nothing)=1
nodeCount (Node (Just n) (Just m) Nothing)=2
nodeCount (Node (Just n) (Just lTree) (Just rTree))=1 + nodeCount lTree + nodeCount rTree



main= do print $ depthOfTree tree1
         print $ balanced unbalancedTree
         print $ balanced tree1
         print $ nodeCount tree2
         print $ nodeCount tree1
