from logic import *

AKnight = Symbol("A is a Knight")
AKnave = Symbol("A is a Knave")

BKnight = Symbol("B is a Knight")
BKnave = Symbol("B is a Knave")

CKnight = Symbol("C is a Knight")
CKnave = Symbol("C is a Knave")

ARule = And (Or(AKnight, AKnave), Not(And(AKnight,AKnave))) # Either Knight or knave, not both
BRule = And (Or(BKnight, BKnave), Not(And(BKnight,BKnave))) # Either Knight or knave, not both
CRule = And (Or(CKnight, CKnave), Not(And(CKnight,CKnave))) # Either Knight or knave, not both

# Puzzle 0
# A says "I am both a knight and a knave."
knowledge0 = And(
    Not(And(AKnight, AKnave)),  # A cannot be both a knight and a knave
    Biconditional(AKnight, And(AKnight, AKnave)), # If A is a knight, "I am both a knight and a knave" is true, otherwise it's false
    ARule
)

# Puzzle 1
# A says "We are both knaves."
# B says nothing.
knowledge1 = And(
    Implication(AKnight, And(AKnave, BKnave)), # If A is a knight, then they are both knaves
    Implication(AKnave, Not(And(AKnave, BKnave))), # If A is a knave, then A must be a knave and B must be a Knight
    ARule,
    BRule
)

# Puzzle 2
# A says "We are the same kind."
# B says "We are of different kinds."
knowledge2 = And(
    Biconditional(AKnight, Biconditional(AKnight, BKnight)),  # If A is a knight, then A and B have to both be knights
    Biconditional(BKnight, Not(Biconditional(AKnight, BKnight))),  # If B is a knight, then A and B cannot both be knights or cannot be both knaves
    ARule,
    BRule
)

# Puzzle 3
# A says either "I am a knight." or "I am a knave.", but you don't know which.
# B says "A said 'I am a knave'."
# B says "C is a knave."
# C says "A is a knight."
knowledge3 = And(
    Implication(AKnight, Or(AKnight, AKnave)), # "I am a knight" could be a knight or knave
    Implication(BKnave, Implication(AKnight, Or(AKnight, AKnave))),   # If B is a Knave, then that it implies A said 'I am a knight'
    Implication(BKnave, Implication(CKnight, And(AKnight, BKnave))),  # If B is a Knave, then that also implies C is a Knight and thus A is a knight
    Implication(CKnight, And(AKnight, BKnave)), # If C is a Knight, then A must be a Knight and B must be a Knave
    ARule,
    BRule,
    CRule
)


def main():
    symbols = [AKnight, AKnave, BKnight, BKnave, CKnight, CKnave]
    puzzles = [
        ("Puzzle 0", knowledge0),
        ("Puzzle 1", knowledge1),
        ("Puzzle 2", knowledge2),
        ("Puzzle 3", knowledge3)
    ]
    for puzzle, knowledge in puzzles:
        print(puzzle)
        if len(knowledge.conjuncts) == 0:
            print("    Not yet implemented.")
        else:
            for symbol in symbols:
                if model_check(knowledge, symbol):
                    print(f"    {symbol}")


if __name__ == "__main__":
    main()
