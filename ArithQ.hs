module ArithQ where
import BinaryTreeHs.BinTree as Bt

--the operation types that must be orderd
data OppOrdered = Sub | Div | Log | Exp deriving (Eq,Show)
--the operation types that cannot be orderd
data OppNoOrdered = Add | Mult deriving (Eq,Show)

--union of the above two classes/data types represents all operations, for convienence
data Operation = Order OppOrdered | NoOrder OppNoOrdered deriving (Eq,Show)

--Equation class representing any equation with the above operations
data Equation a s = Constant a | Variable s | Group OppNoOrdered (Bt.BinaryTree (Equation a s)) | Line OppOrdered [(Equation a s)] deriving (Show)

same :: (Eq a,Eq s) => Equation a s -> Equation a s -> Bool
same (Constant x) (Constant y) = x == y
same (Variable x) (Variable y) = x == y
same (Group opp tree) (Group opp2 tree2) = False
same (Line order []) (Line order2 []) = order == order2
same (Line order (v:arr)) (Line order2 (v2:arr2)) = (same v v2) && (same (Line order arr) (Line order2 arr2))
same _ _ = False
--this function determines the next function representing performing the given function more than once
nextOpp :: Operation -> Operation
nextOpp (NoOrder Add) = (NoOrder Mult)
nextOpp (NoOrder Mult) = (Order Exp)
nextOpp x = x

--this function orders equations such that:
--	Variables are ordered
--	Constants are ordered, but automaticaly ordered above variables
--	Other Equations are UNORDERED, but ordered bellow variables
--	Comparing equations will ALLWAYS return true in this sense they are supposed to be placed at -infinit
equationLess :: (Ord a,Ord s) => Equation a s -> Equation a s -> Bool
equationLess (Constant _) (Variable _) = False
equationLess (Constant x) (Constant y) = x < y
equationLess (Variable x) (Variable y) = x < y
equationLess _ _ = True 

--tells when two equations are equal such that:
--	two constants are equal if their values are equal
--	two variables are equal if their values are equal
equationEqu :: (Eq a,Eq s) => Equation a s -> Equation a s -> Bool
equationEqu (Constant x) (Constant y) = x == y
equationEqu (Variable x) (Variable y) = x == y
equationEqu _ _ = False

--default behavior for adding new elements to a group, what to do when equal elements are found 
--equationEquOpp :: Equation a s -> Equation a s

