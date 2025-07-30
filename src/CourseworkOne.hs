module CourseworkOne where

import Halatro.Constants
import Halatro.Types
import Data.List
import Data.Ord
import Data.Function (on)

-- NOTE: I use excessive brackets in some instances to aid my understanding of the function application order

--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

contains :: Hand -> HandType -> Bool
contains h ht
    | ht == None                 = null h
    | ht == HighCard             = isHighCard h
    | ht == Pair                 = isPair ranks
    | ht == TwoPair              = isTwoPair ranks
    | ht == ThreeOfAKind         = threeFourOfAKind ranks 3
    | ht == FourOfAKind          = threeFourOfAKind ranks 4
    | ht == Straight             = isStraight ranks
    | ht == Flush                = isFlush suits
    | ht == FullHouse            = isFullHouse ranks
    | ht == StraightFlush        = isStraightFlush h
    | ht == RoyalFlush           = isRoyalFlush h
    | otherwise = False
    where
        ranks = map rank h
        suits = map suit h

-------- To verify the presence of a high card --------
isHighCard :: [Card] -> Bool
isHighCard [] = False
isHighCard hand =
        -- Checks if the hand is not all of the other types
        not (isPair ranks || isTwoPair ranks || threeFourOfAKind ranks 3 ||
            threeFourOfAKind ranks 4 || isStraight ranks || isFlush suits ||
            isFullHouse ranks || isStraightFlush hand || isRoyalFlush hand)
    where
        ranks = map rank hand
        suits = map suit hand


-------- To verify the presence of a pair --------
isPair :: [Rank] -> Bool
isPair [] = False
isPair (x:xs)
    | x `elem` xs = True
    | otherwise = isPair xs

-------- To verify presence of a two pair --------
isTwoPair :: [Rank] -> Bool
isTwoPair ranks =
        -- If groupedRankLengths contains 2 groups of two or more elements, or 1 group of exactly 4 elements, then there is a two pair
        length (filter (>=2) (groupedRankLengths ranks)) == 2 || elem 4 (groupedRankLengths ranks)

-------- To verify presence of 3 or 4 of a kind --------
threeFourOfAKind :: [Rank] -> Int -> Bool
threeFourOfAKind ranks noOfAKind =
        -- Checks if there are exactly 1 group of either three elements or four elements (depending on 'noOfAKind' value)
        any (>=noOfAKind) (groupedRankLengths ranks)

-------- To verify presence of straight --------
isStraight :: [Rank] -> Bool
isStraight [] = False
-- Simply calls an enumerate to check if the hand contains sequential ranks
isStraight ranks = sortedRankList == [(head sortedRankList)..(nextRank $ nextRank $ nextRank $ nextRank (head sortedRankList))]
    where
        sortedRankList = sort ranks

-- Implement my own nextRank method to allow successing from Ace -> Two (to avoid errors)
nextRank :: Rank -> Rank
nextRank Ace = Two
nextRank rankType = succ rankType

-------- To verify presence of flush --------
isFlush :: [Suit] -> Bool
isFlush [] = False
isFlush (x:xs) = (x:xs) == [x,x,x,x,x]

-------- To verify presence of full house --------
isFullHouse :: [Rank] -> Bool
isFullHouse ranks =
        -- If groupedRankLengths contains exactly 1 group of three elements and 1 group of 2 elements, then there is a full house
        elem 3 (groupedRankLengths ranks) && elem 2 (groupedRankLengths ranks)

-------- To verify presence of straight flush --------
isStraightFlush :: [Card] -> Bool
isStraightFlush hand = isStraight (map rank hand) && isFlush (map suit hand)

-------- To verify presence of royal flush --------
isRoyalFlush :: [Card] -> Bool
isRoyalFlush hand = (sort (map rank hand) == [Ten, Jack, Queen, King, Ace]) && isFlush (map suit hand)

-- Sorts the list of ranks, then groups them into sublists of repeats, and returns a list containing the lengths of the sublists
groupedRankLengths :: [Rank] -> [Int]
groupedRankLengths ranks = map length (group (sort ranks))

--------------------------------------------------------------------------------
-- Part 2: identify the highest value hand type in a played hand

-- Since pattern matching is done from top to bottom, prioritise matches to higher value
-- hand types first before the lower ones therefore guaranteeing the highest match
bestHandType :: Hand -> HandType
bestHandType h
    | isRoyalFlush h            = RoyalFlush
    | isStraightFlush h         = StraightFlush
    | threeFourOfAKind ranks 4  = FourOfAKind
    | isFullHouse ranks         = FullHouse
    | isFlush suits             = Flush
    | isStraight ranks          = Straight
    | threeFourOfAKind ranks 3  = ThreeOfAKind
    | isTwoPair ranks           = TwoPair
    | isPair ranks              = Pair
    | isHighCard h              = HighCard
    | otherwise                 = None
    where
        ranks = map rank h
        suits = map suit h

--------------------------------------------------------------------------------
-- Part 3: score a played hand

-- As long as the hand type is not "None" or "HighCard", filter for all the cards that, when removed,
-- invalidate the hand type. Put all these hands into a list - these are the cards that are scored
whichCardsScore :: Hand -> [Card]
whichCardsScore h
    | ht == None                = []
    | ht == HighCard            = [maximum h]
    | otherwise   = filter (\card -> not (contains (delete card h) ht)) h
    where
        ht = bestHandType h

-- Add all the rank scores of the (scoring) cards in the hand, add the base chips and multiplier
scoreHand :: Hand -> Int
scoreHand h = (foldr ((+) . rankScore . rank) 0 (whichCardsScore h) + chips) * mult
    where
        -- Get the chips and the multiplier for the best hand type of the hand
        (chips, mult) = handTypeValues (bestHandType h)

--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

{- 
   This function ceates a "handAndScores" list of tuples (x,y) with the subset card set as x, and the score generated for that card-set as y
   It then works out the maximum score that was calculated from the best subset card-set and looks for the tuple(s) which matches the score
   If there were multiple max scoring card sets, it takes the one with the highest rank cards, and returns the cards which score in that hand 
   If the hand that scores is <5 then other low ranked "filler" cards are added.
-}

highestScoringHand :: [Card] -> Hand
highestScoringHand cards
    | length cards <= 5 = cards
    | otherwise =
        if length bestHand /= 5
            then
                bestHand ++ take (5 - length bestHand) (sort (cards \\ bestHand)) -- Adds additional "filling" cards to make use of playing "discards"
        else
            bestHand
    where
        -- Works out the tuple list with the card subsets and their scores, then find the max
        subHands = filter (\hand -> length hand <=5) (subsetsFunc cards)
        subHandScores = map (scoreHand . whichCardsScore) subHands
        handAndScores = zip subHands subHandScores

        maxY = maximum subHandScores -- To accommodate high card type

        -- Filter for the card subset with the maximum score
        maxHandScores = filter (\(_,y) -> y == maxY) handAndScores

        -- Filter out the cards that score from the best card-subset
        bestHand = whichCardsScore (fst (maximumBy (comparing fst) maxHandScores))

-- Function that generates a set of all subsets of the given card-set
subsetsFunc :: [Card] -> [[Card]]
subsetsFunc []  = [[]]
subsetsFunc (x:xs) = subsetsFunc xs ++ map (x:) (subsetsFunc xs)
--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

simpleAI :: [Move] -> [Card] -> Move
simpleAI _ cards = Move Play (take 5 (sortBy (comparing Down) cards))

-- Plays the best hand without discards
sensibleAI :: [Move] -> [Card] -> Move
sensibleAI _ cards = Move Play (highestScoringHand cards)

myAI :: [Move] -> [Card] -> Move
myAI = mainAI

-- AI based on prioritising flushes, if full house or four of a kind whilst working towards a flush then play, If out of discards then revert to sensible AI.
mainAI :: [Move] -> [Card] -> Move
mainAI moves cards
    | length popularSuit == 5                               = Move Play cardsToKeep
    | length popularSuit > 5                                = Move Play (take 5 (sortBy (comparing Down) cardsToKeep))
    | fhCheck                                               = Move Play (extractFH ranksGrouped)
    | discardCheck moves                                    = sensibleAI moves cards
    | 4 `elem` lengthsOfGroupedRanks                        = Move Play (head (filter (\card -> length card == 4) (ranksGrouped)))
    | otherwise                                             = flushAI cards cardsToKeep
    where
        -- To check for four of a kind or full house, groups ranks, filters for best rank groups for full house and processes
        ranksGrouped = filter (\card -> length card >= 2) (groupBy ((==) `on` rank) (sortBy (comparing rank) cards))
        lengthsOfGroupedRanks = map length ranksGrouped
        fhCheck = ((length ranksGrouped) >=2) && (any (>=3) lengthsOfGroupedRanks)

        -- To check for flushes
        popularSuit = popularSuitCheck (map suit cards)
        cardsToKeep = filter (\card -> suit card == (head popularSuit)) cards

-- To get the best full house from the groups of ranks, take 3 from any group >=3 and combine with the next best group of 2
extractFH :: [[Card]] -> [Card]
extractFH ranksGrouped = take 3 (firstGroup) ++ take 2 (head (filter (\g -> length g >=2) (delete firstGroup ranksGrouped)))
    where
        firstGroup = head (filter (\g -> length g >=3) ranksGrouped)

-- To discard the 5 least ranked cards without the current flush being processed
flushAI :: [Card] -> [Card] -> Move
flushAI cards cardsToKeep = Move Discard (take 5 (sortBy (comparing Down) (cards \\ cardsToKeep)))

-- To find the most frequent (and best ranked) suit of cards
popularSuitCheck :: [Suit] -> [Suit]
popularSuitCheck suits = maximumBy (comparing length) (group (sortBy (comparing Down) suits))

-- To check if discard limit reached
discardCheck :: [Move] -> Bool
discardCheck moves = (length (filter (\move -> returnPOD move == Discard) moves)) == 3

returnPOD :: Move -> PlayOrDiscard
returnPOD (Move x _) = x
