data Tree a =
	Empty |
	Node a (Tree a) (Tree a)

type Entry = (String, String)
type PhoneBook = (Tree Entry)

name = fst
phone = snd

find :: String -> PhoneBook -> String
find _ Empty = ""
find name (Node (n, p) left right)
	| name == n = p
	| name < n = find name left
	| otherwise = find name right

insert :: Entry -> PhoneBook -> PhoneBook
insert entry Empty = Node entry Empty Empty
insert entry phoneBook@(Node (n, p) left right)
	| name entry == n = phoneBook
	| name entry < n = Node (n, p) (insert entry left) right
	| otherwise = Node (n, p) left (insert entry right)

remove :: String -> PhoneBook -> PhoneBook
remove _ Empty = Empty
remove n (Node root left right)
	| n < name root = Node root (remove n left) right
	| n > name root = Node root left (remove n right)
	|otherwise = case left of
					Empty -> right
					_ -> case right of
							Empty -> left
							_ -> Node minVal left (remove (name minVal) right)
									where minVal = findMin right

findMin :: (Tree a) -> a
findMin (Node root Empty _) = root
findMin (Node _ left _) = findMin left

linearize :: (Show a) => (Tree a) -> [a]
linearize Empty = []
linearize (Node root left right) = linearize left ++ [root] ++ linearize right

leaves :: (Tree a) -> [a]
leaves Empty = []
leaves (Node root Empty Empty) = [root]
leaves (Node _ left right) = leaves left ++ leaves right

height :: (Tree a) -> Integer
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

instance (Show a) => Show (Tree a) where
	show Empty = "[]"
	show (Node root left right) = "[" ++ show root ++ " " ++ show left ++ " " ++ show right ++ "]"

x1 = ("ivan1", "42")
x2 = ("ivan2", "44")
x3 = ("ivan3", "12")
x4 = ("ivan4", "17")
x5 = ("ivan5", "24")
x6 = ("ivan6", "33")

--a useful example of $ usage
samplePhoneBook = insert x6 $ 
				  insert x5 $ 
				  insert x4 $ 
				  insert x3 $ 
				  insert x2 $ 
				  insert x1 Empty


