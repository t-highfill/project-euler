
data Suit = Diamonds | Hearts | Clubs | Spades deriving (Show, Eq)

data CardType = Jack | Queen | King | Ace | Basic Int deriving (Show, Eq)

data Card = Card {cardType :: CardType, suit :: Suit} deriving (Eq, Show)

type Deck = [Card]

data HandRank = High CardType | OnePair CardType | TwoPair CardType | ThreeKind CardType
				| Straight CardType | Flush Suit | FullHouse CardType CardType
				| FourKind CardType | StraightFlush CardType Suit | RoyalFlush Suit

value :: CardType -> Int
value (Basic v) = v
value Jack = 11
value Queen = 12
value King = 13
value Ace = 14

rank :: HandRank -> Int
rank (High _) = 1
rank (OnePair _) = 2
rank (TwoPair _) = 3
rank (ThreeKind _) = 4
rank (Straight _) = 5
rank (Flush _) = 6
rank (FullHouse _ _) = 7
rank (FourKind _) = 8
rank (StraightFlush _) = 9
rank (RoyalFlush _) = 10

instance Ord CardType where
	compare t1 t2 = compare (value t1) (value t2)

instance Ord HandRank where
	compare (High t1) (High t2) = compare t1 t2
	compare (OnePair t1) (OnePair t2) = compare t1 t2
	compare (TwoPair t1) (TwoPair t2) = compare t1 t2
	compare (ThreeKind t1) (ThreeKind t2) = compare t1 t2
	compare (Straight t1) (Straight t2) = compare t1 t2
	compare (FourKind t1) (FourKind t2) = compare t1 t2
	compare (Flush _) (Flush _) = EQ
	compare (RoyalFlush _) (RoyalFlush _) = EQ
	compare (FullHouse p1 t1) (FullHouse p2 t2) = compare (t1, p1) (t2, p2)
	compare (StraightFlush t1 _) (StraightFlush t2 _) = compare t1 t2
	compare hr1 hr2 = compare (rank hr1) (rank hr2)