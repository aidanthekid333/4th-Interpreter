import Data.List
import Data.Char
import Data.Maybe 
import qualified Data.Map.Strict as M
import Debug.Trace

--Token data type structure
data Token = 
        Val Float 
    |   Word String
    deriving ( Eq, Show )

--ForthState data type structure
data ForthState = ForthState { 
    stack :: [Float],
    names :: M.Map String AstNode 
} deriving ( Show )


--AstNode data type structure
data AstNode =
        -- a single token 
        Terminal Token 

        -- an if node. contains two branches: one for true and one for false. 
    |   If { ifTrue :: AstNode, ifFalse :: AstNode }

        -- a while node. contains only a child node for the body of the loop. 
    |   While AstNode

        -- a funcion definition. contains the name of the function and the function definition
    |   Function {name :: String, body :: AstNode}

        -- a list of nodes. Represents a sequence of instructions like "1 1 + 2 *"
    |   Expression [AstNode]
    deriving (Show)






doAdd :: ForthState -> ForthState
doAdd state = 
    let ( state', b, a  ) = fsPop2 state 
    in fsPush ( a + b ) state' 
-- we need to pop 2 values so we can add them.
-- we will pop 2 values in all the below operations. 
-- you can streamline this by defining a helper function "binary_op" if you want.
-- it can take a function with type Int -> Int -> Int and apply it to the top 
-- two values on the stack, pushing the result. 

-- apply the - operation: pop 2 values, subtract them, push the result. 1 2 - -> -1
doSub :: ForthState -> ForthState
doSub state =
    let ( state', b, a  ) = fsPop2 state 
    in fsPush ( a - b ) state' 

-- apply the * operation: pop 2 values, multiply them, push the result. 3 4 * -> 12
doMul :: ForthState -> ForthState
doMul state =
    let ( state', b, a  ) = fsPop2 state 
    in fsPush ( a * b ) state' 

-- apply the / operation: pop 2 values, divide them, push the result. 4 2 / -> 2
doDiv :: ForthState -> ForthState
doDiv state =
    let ( state', b, a  ) = fsPop2 state 
    in fsPush ( a / b ) state' 

-- apply the swap operation. pop 2 values, re-push them in reverse order. 1 2 swap -> 2 1 
doSwap :: ForthState -> ForthState 
doSwap state =
    let ( state', b, a  ) = fsPop2 state 
    in fsPush a $ fsPush b state' 

-- apply the drop operation. pop 1 value. 1 2 3 -> 1 2 
-- does nothing if stack is empty 
doDrop :: ForthState -> ForthState
doDrop state =
    let (state', _) = fsPop state
    in state' 

-- apply the rot operation. rotates the top three right: 1 2 3 -> 3 1 2 
-- does nothing if stack is empty or size 1
-- same as swap if stack has size 2 
doRot :: ForthState -> ForthState 
doRot state =
    let (state', a, b, c) = fsPop3 state
    in fsPush b $ fsPush c $ fsPush a state'

    -- duplicate the top value on the stack. 1 -> 1 1 
doDup :: ForthState -> ForthState
doDup state = fsPush (fsTop state) state


doGreater :: ForthState -> ForthState
doGreater state = 
    let(state', b, a) = fsPop2 state
    in
        if a > b
        then (fsPush 1 state')
        else (fsPush 0 state')

doLessThen :: ForthState -> ForthState
doLessThen state =
    let(state', b, a) = fsPop2 state
    in
        if a < b
        then (fsPush 1 state')
        else (fsPush 0 state')


doGreaterEquals :: ForthState -> ForthState
doGreaterEquals state =
    let(state', b, a) = fsPop2 state
    in
        if a >= b
        then (fsPush 1 state')
        else (fsPush 0 state')


doLessThenEquals :: ForthState -> ForthState
doLessThenEquals state =
    let(state', b, a) = fsPop2 state
    in
        if a <= b
        then (fsPush 1 state')
        else (fsPush 0 state')


doEquals :: ForthState -> ForthState
doEquals state =
    let(state', b, a) = fsPop2 state
    in
        if a == b
        then (fsPush 1 state')
        else (fsPush 0 state')


doNotEquals :: ForthState -> ForthState
doNotEquals state =
    let(state', b, a) = fsPop2 state
    in
        if a /= b
        then (fsPush 1 state')
        else (fsPush 0 state')

doFunction :: String -> ForthState -> ForthState
doFunction name state =
    case M.lookup name (names state) of
    Nothing -> error "unrecognized token"
    Just functionNode -> doNode functionNode state


-- performs the operation identified by the string. for example, doOp state "+"
-- will perform the "+" operation, meaning that it will pop two values, sum them,
-- and push the result. 
doOp :: String -> ForthState -> ForthState
-- here's how we turn the strings into their corresponding operation. 
doOp "+" = doAdd
doOp "-" = doSub
doOp "*" = doMul
doOp "/" = doDiv 
doOp "swap" = doSwap 
doOp "drop" = doDrop 
doOp "rot" = doRot 
doOp "dup" = doDup
doOp ">" = doGreater
doOp "<" = doLessThen
doOp ">=" = doGreaterEquals
doOp "<=" = doLessThenEquals
doOp "==" = doEquals
doOp "/=" = doNotEquals 

-- if we go through all the definitions without finding our operation, 
-- it's not supported 
doOp op = doFunction op 

-- execute an AstNode
doNode :: AstNode -> ForthState -> ForthState

-- if we execute a terminal that's an if-statement, we need to determine whether
-- the top of the stack is "true" (/= 0.0)
doNode If { ifTrue = trueBranch, ifFalse = falseBranch } state = 
    if fsTop state /= 0
        then doNode trueBranch state
        else doNode falseBranch state

doNode ( While loopBody ) state = 
    if fsTop state /= 0
        then trace ("Doing While Loop... \nLoop Body: " ++ show loopBody)
            doNode (While loopBody) (doNode loopBody state)
        else state

-- doing a terminal changes depending on whether it's a word or a number. 
-- if it's a number, push it...
doNode ( Terminal ( Val v ) ) state = fsPush v state

-- ...if it's a word, execute the operation
doNode ( Terminal ( Word o ) ) state = doOp o state

-- "doing" an empty expression does nothing
doNode ( Expression [] ) state = state 

-- "doing" a non-empty expression tries to execute every node in the expression
doNode ( Expression ( first:rest ) ) state = (doNode (Expression rest)) (doNode first state)

doNode Function { name = name, body = body} state = ForthState {stack = stack state, names = M.insert name body (names state)}



parseExpression' :: [AstNode] -> [Token] -> [String] -> ( [AstNode], [Token], Maybe Token )

-- if there are no more tokens, we need to check if we have terminators.
-- if we were expecting a terminator and there isn't one, that's an error. 
parseExpression' alreadyParsed [] terminators = 
    -- this is the base case: nothing to parse
    if null terminators then trace("Already Parsed: " ++ show alreadyParsed) $ (alreadyParsed, [], Nothing ) 
    -- error case 
    else error ( "ended expression without finding one of: " ++ intercalate ", " terminators ++ " Already Parsed: " ++ show alreadyParsed)

-- if tokens remain, keep parsing
parseExpression' alreadyParsed ( token:tokens ) terminators 
    -- found a terminator: stop parsing and return. 
    | token `elem` map Word terminators = trace ("Found terminator " ++ show token) $ (alreadyParsed, tokens, Just token )

    -- found an if-statement: remove the "if" token, parse the true and false branches, and 
    -- then parse whatever is after the if-statement.
    | token == Word "if" =
        let(trueNode, falseNode, tokens') = parseIf tokens
        in parseExpression' ((If{ifTrue = trueNode, ifFalse = falseNode}):alreadyParsed) tokens' terminators
            
    -- found a while-statement: remove the "while", parse the body, then parse whatever is after
    | token == Word "while" =
        let (whileNode, tokens') = parseWhile tokens
        in parseExpression' (While whileNode: alreadyParsed) tokens' terminators

    | token == Word ":" =
        let (functionNode, tokens') = parseFunction tokens
        in trace("Parced Function... \nFunction Body: " ++ show functionNode ++"\nRemaining Tokens: " ++ show tokens') $ parseExpression' (functionNode:alreadyParsed) tokens' terminators
        
    -- no special word found. We are parsing a list of operations. Keep doing this until 
    -- there aren't any. 
    | otherwise = parseExpression' (Terminal token : alreadyParsed) tokens terminators


-- takes the result of parseExpression' and wraps it in an Expression constructor
parseExpression :: [Token] -> AstNode
parseExpression tokens = 
    let (astNode, _, _) = parseExpression' [] tokens [] 
    in Expression (reverse astNode)

-- we just saw an "if". now we have to build an "If" AstNode.
-- returns the two branches and the remaining tokens. 
-- ( ifTrue, ifFalse, remainingTokens ). 
parseIf :: [Token] -> ( AstNode, AstNode, [Token] ) 
parseIf tokens = 
    let ( ifTrue, tokens', terminator ) = parseExpression' [] tokens [ "else", ";" ]
    in 
        if terminator == Just (Word "else")
            then 
                let (ifFalse, tokens'') = parseElse tokens'
                in (Expression (reverse ifTrue), ifFalse, tokens'')
            else (Expression (reverse ifTrue), Expression [], tokens')
       

-- we just saw an "else". now finish the ifFalse part of the If node. This one only needs to 
-- return the "false" branch of the if statement, which is why there is only one [AstNode] in 
-- the return value. 
parseElse :: [Token] -> (  AstNode, [Token] )
parseElse tokens = 
    let (elseNode, tokens', _) = parseExpression' [] tokens [";"] 
    in (Expression (reverse elseNode), tokens')

-- parsing a while loop is similar to parsing an if statement. 
parseWhile :: [Token] -> ( AstNode, [Token] )
-- if we reach the end of our tokens without closing the loop, that's an error 
parseWhile [] = error "while without closing semicolon."
-- otherwise, parse the loop body until reaching the ";" 
parseWhile tokens = 
    let (whileNode, tokens', _) = parseExpression' [] tokens [";"]
    in (Expression (reverse whileNode), tokens')


parseFunction :: [Token] -> (AstNode, [Token])
parseFunction (Word name:tokens) = 
    let (functionNode, tokens', _) = parseExpression' [] (tokens) [";"]
    in trace("Parsing Function... \nTokens: " ++ show tokens) $ (Function{name = name, body = Expression (reverse functionNode)}, tokens')

    
-- create a new interpreter
fsNew :: ForthState
fsNew = ForthState { stack = [], names = M.empty}

-- push a new value onto the stack
fsPush :: Float -> ForthState -> ForthState
fsPush i state = ForthState { stack = i : stack state, names = names state }

-- remove a value from the stack, or print an error if nothing is there.
-- returns the value removed and the new state 
fsPop :: ForthState -> ( ForthState, Float )
fsPop state = 
    let top = head $ stack state 
        new_stack = tail $ stack state  
    in  
        ( ForthState { stack = new_stack, names = names state }, top )

-- remove two values from the stack. return the new stack and the two items.
fsPop2 :: ForthState -> ( ForthState, Float, Float )
fsPop2 state = 
    let (state', a) = fsPop state
        (state'', b) = fsPop state'
    in
        (state'', a, b)

-- remove three values from the stack. return the new stack and the three items. 
fsPop3 :: ForthState -> ( ForthState, Float, Float, Float )
fsPop3 state = 
    let (state', a, b) = fsPop2 state
        (state'', c) = fsPop state'
    in (state'', a, b, c)

-- return the value on top of the stack 
fsTop :: ForthState -> Float 
fsTop state = head $ stack state

-- Takes a single word and turns it into a token. So "2" becomes "I 2" and 
-- "+" becomes "Op +"
lexToken :: String -> Token
lexToken t = 
    let firstChar = ord . head in 
    if firstChar t >= ord '0' && firstChar t <= ord '9' then 
        Val $ read t 
    else 
        Word t 

-- Takes a whole program and turns it into a list of tokens. Calls "lexToken"
tokenize :: String -> [Token]
tokenize code = map lexToken $ words code

-- removes comments from a token stream. comments are between /' and '/
-- arguments:
--  * the first bool tells us whether we are in a comment or not. starts false.
--  * the first token list is the tokens that are not inside of comments. starts empty.
--  * the last list are the remaining tokens 
removeComments :: Bool -> [Token] -> [Token] -> [Token]

-- if the first argument is 'true', we're inside a comment. but the [] means no more tokens.
removeComments True _ [] = error "ended comment while it's still open. need closing '/ ."  

-- if we finish all the tokens and are not in a comment, there's nothing else to do
-- DO NOT REVERSE THIS TIME BECAUSE WE STILL NEED TO PARSE 
removeComments False nonComments [] = trace("Valid Tokens: " ++ show nonComments) reverse nonComments

-- if we're in a comment and we find '/, we close the comment and continue 
removeComments True nonComments (Word "'/":tail ) = removeComments False nonComments tail

-- if we're in a comment, ignore whatever token comes next 
removeComments True nonComments ( _:tail ) = removeComments True nonComments tail

-- if we're not in a comment and we find /', start the comment 
removeComments False nonComments (Word "/'":tail ) = removeComments True nonComments tail

-- if we're not in a comment, add the token to the nonComment tokens 
removeComments False nonComments ( head:tail ) = removeComments False (head:nonComments) tail


main :: IO ()
main = do
    -- get all the code passed to STDIN as a giant string 
    code <- getContents

    -- convert it into a list of tokens
    let tokens = removeComments False [] ( tokenize code ) 

    -- parse the ast 
    let ast = parseExpression tokens

    -- if tokens are left after we are done parsing, there's a problem
    print ast 

    putStrLn ""

    print $ reverse $ stack $ doNode ast fsNew 

