"""
Tic Tac Toe Player
"""
# This is the solution file that I tested

import math
import random
from copy import deepcopy


X = "X"
O = "O"
EMPTY = None


def initial_state():
  """
  Returns starting state of the board.
  """
  return [[EMPTY, EMPTY, EMPTY],
          [EMPTY, EMPTY, EMPTY],
          [EMPTY, EMPTY, EMPTY]]


def player(board):
    """
    Returns player who has the next turn on a board.
    """
    # Scan board for entries and determine next player:

    X_count = 0
    O_count = 0
    EMPTY_count = 0

    for row in board:
      X_count += row.count(X)
      O_count += row.count(O)
      EMPTY_count += row.count(EMPTY)

    # If X has more squares than O, its O's turn:
    if X_count > O_count:
      return O
    # Otherwise it is X's turn:
    else:
      return X


def actions(board):
    """
    Returns set of all possible actions (i, j) available on the board.
    i represents the board row, j the board column, both 0, 1 or 2
    The actions are are represented as the tuple (i, j) where the piece can be placed.
    """

    possible_actions = set() 
    for i in range(3):
        for j in range(3):
            if board[i][j] == EMPTY:
                possible_actions.add((i, j))
    return possible_actions




def result(board, action):
    """
    Returns the board that results from making move (i, j) on the board.
    """

    i = action[0]
    j = action[1]

    # Check move is valid:
    if i not in [0, 1, 2] or j not in [0, 1, 2]:
      print('\nFunction given an invalid board position for action')
      return
    elif board[i][j] != EMPTY:
      print('\nResult function tried to perform invalid action on occupaied tile')
      return

    # Make a deep copy of the board and update with the current player's move:
    board_copy = deepcopy(board)
    board_copy[i][j] = player(board)

    return board_copy


def winner(board):
    """
    Returns the winner of the game, if there is one.
    otherwise return None
    """

    # Check rows: (moves from bottom row to top)
    for j in range(3):
        if board[0][j] == board[1][j] == board[2][j] and board[0][j] != EMPTY: 
            return board[0][j]

    # Check columns: (moves from left to right)
    for i in range(3):
        if board[i][0] == board[i][1] == board[i][2] and board[i][0] != EMPTY:
            return board[0][j]

    # Check diagonals:
    if board[0][0] == board[1][1] == board[2][2] and board[0][0] != EMPTY: # a) bottom right to top left
        return board[0][0]
    if board[0][2] == board[1][1] == board[2][0] and board[0][2] != EMPTY: # b) top right to bottom left
        return board[0][2]

    # Otherwise no current winner, return None
    return None



def terminal(board):
    """
    Returns True if game is over, False otherwise.
    """ 
    # Game is over if it is a winning board or all tiles are full (no actions):

    if winner(board) or not actions(board):
      return True
    else:
      return False


def utility(board):
    """
    Returns 1 if X has won the game, -1 if O has won, 0 otherwise.
    """

    if winner(board) == 'X':
      return 1
    elif winner(board) == 'O':
      return -1
    else:
      return 0

actions_explored = 0


def minimax(board):
    """
    Returns the optimal action for the current player on the board.
    'X' Player is trying to maximise the score, 'O' Player is trying to minimise it
    """

    global actions_explored

    # Check if the game is over
    if terminal(board):
        return None

    # Determine the current player
    current_player = player(board)

    # Initialize best_score and best_action determined by the player
    if current_player == X:
        best_score = -math.inf # negative infinity
    else:
        best_score = math.inf  # infinity

    best_action = None

    # Explore all possible actions
    for action in actions(board):
        actions_explored += 1                            
        new_board = result(board, action)                # get a copy of the board with that most recent move 
        score = minimax_value(new_board, current_player) # recursively go down the tree & calculate best score per branch

        if current_player == X:
            if score > best_score: # a) Updates the max player, 'X'
                best_score = score
                best_action = action
        else:                    
            if score < best_score: # b) Updates the min player, 'O'
                best_score = score
                best_action = action

    return best_action

"""
Helper function to go through the tree and replace score with best score according to the 
current board position and the current player ('X' being max, 'O' being min)
"""
def minimax_value(board, turn):
    # Check if you ever reach the end of the game
    if terminal(board):         # If there are no more actions to take..
        return utility(board)   # then return the score

    # Initialize best_score given current tree level explored
    if turn == X:
        best_score = -math.inf # negative infinity
    else:
        best_score = math.inf  # infinity

    # Go through the tree to find the best possible actions to take
    for action in actions(board):
        new_board = result(board, action)
        new_turn = player(new_board)
        score = minimax_value(new_board, new_turn) # recursively go down the tree

        if turn == X:
            best_score = max(best_score, score)  # return the best max score on that branch of the tree
        else:
            best_score = min(best_score, score)  # return the best min score on that branch of the tree

    return best_score # return the best score for that player given the entire tree