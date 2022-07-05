import Data.List (sort, nub)
--ghc algo.hs
-- ./algo.exe
--((p)|(q&r))->((p|q)&(p|r))

--retorna as strings contidas entre parenteses dados
sliceSubFormulas :: [a] -> [(Int, Int)] -> [[a]]
sliceSubFormulas formula matchingParenthesis =[take ((snd x)-(fst x)-1) (drop (fst x+1) formula) | x <- matchingParenthesis]

getMatchingParenthesis :: String -> [(Int, Int)]
getMatchingParenthesis = aux 0 []
  where
    aux _ _ [] = []
    aux currentIndex parenthesisStack ('(' : remainingString) = aux (currentIndex + 1) (currentIndex : parenthesisStack) remainingString
    aux currentIndex (lastOpenParenthesis:openParenthesis) (')' : remainingString) = (lastOpenParenthesis, currentIndex) : aux (currentIndex + 1) openParenthesis remainingString
    aux currentIndex parenthesisStack (c : remainingString) = aux (currentIndex + 1) parenthesisStack remainingString


addExternalParenthesis :: String -> String
addExternalParenthesis [] = []
addExternalParenthesis (x:xs) = if x/='(' then "("++x:xs++")" else x:xs
--retorna todos as subfórmulas que sejam internas a outras  
getInternalSubFormulas :: String -> [(Int, Int)]
getInternalSubFormulas formula = nub (concat (filter (\x -> (length x /= 0)) (allNested (sort (getMatchingParenthesis formula)))))

--retorna subformulas em parenteses não aninhados (util p separar a string na stringToFormula)
getExternalSubFormulas :: String -> [(Int, Int)] -> [(Int, Int)]
getExternalSubFormulas formula matchingParenthesis= [x | x<-matchingParenthesis, x `notElem` getInternalSubFormulas formula]

--retorna todos os índices de parênteses internos
allNested :: (Ord a1, Ord a2) => [(a1, a2)] -> [[(a1, a2)]]
allNested matchingParenthesis = [(generateNestedParenthesisList (fst x) (snd x) matchingParenthesis) | x<-matchingParenthesis] 

--retorna todos os índices de parênteses internos a uma determinada subfórmula
generateNestedParenthesisList :: (Ord a1, Ord a2) => a1 -> a2 -> [(a1, a2)] -> [(a1, a2)]
generateNestedParenthesisList start end matchingParenthesis = [ x | x <- (sort matchingParenthesis), (fst x) > start, (snd x) < end]

charFound :: String ->Integer-> String
charFound str x
  |(length str <4) = ""
  |(((str!!0) == 'v') && ((str!!3) == '&') && (length str == 5)) = show (x+1)++".v:"++ [str!!2]++"("++show(x`div`2)++");"++show(x+2)++".v:"++[str!!4]++"("++show(x`div`2)++")"
  |(((str!!0) == 'f') && ((str!!3) == '&') && (length str == 5)) =  show (x+1)++".f:"++ [str!!2]++"("++show(x`div`2)++")/"++show(x+2)++".f:"++[str!!4]++"("++show(x`div`2)++")"
  |(str!!0 == 'v')&& (length str>5) && (pegaSimbulo str 0 =='&') =  show (x+1)++".v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)++"("++show(x`div`2)++");"++show (x+2)++".v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)++"("++show(x`div`2)++")\n"++charFound ("v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)) (2*(x+1))++"\n"++charFound ("v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)) (2*(x+2))
  |(str!!0 == 'f')&& (length str>5) && (pegaSimbulo str 0 =='&') =  show (x+1)++".f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)++"("++show(x`div`2)++")/"++show (x+2)++".f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)++"("++show(x`div`2)++")\n"++charFound ("f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)) (2*(x+1))++"\n"++charFound ("f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)) (2*(x+2))
  |(str!!0 == 'v')&& (length str>5) && (pegaSimbulo str 0 =='|') =  show (x+1)++".v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)++"("++show(x`div`2)++")/"++show (x+2)++".v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)++"("++show(x`div`2)++")\n"++charFound ("v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)) (2*(x+1))++"\n"++charFound ("v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)) (2*(x+2))
  |(str!!0 == 'f')&& (length str>5) && (pegaSimbulo str 0 =='|') =  show (x+1)++".f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)++"("++show(x`div`2)++");"++show (x+2)++".f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)++"("++show(x`div`2)++")\n"++charFound ("f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)) (2*(x+1))++"\n"++charFound ("f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)) (2*(x+2))
  |(str!!0 == 'v')&& (length str>5) && (pegaSimbulo str 0 =='-') =  show (x+1)++".f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)++"("++show(x`div`2)++")/"++show (x+2)++".v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)++"("++show(x`div`2)++")\n"++charFound ("f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)) (2*(x+1))++"\n"++charFound ("v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)) (2*(x+2))
  |(str!!0 == 'f')&& (length str>5) && (pegaSimbulo str 0 =='-') =  show (x+1)++".v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)++"("++show(x`div`2)++");"++show (x+2)++".f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)++"("++show(x`div`2)++")\n"++charFound ("v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0)) (2*(x+1))++"\n"++charFound ("f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)) (2*(x+2))
  |(((str!!0) == 'f') && ((str!!2) == '~') && (length str == 4)) =  show (x+1)++".v:"++ [str!!3]++"("++show(x`div`2)++")"
  |(((str!!0) == 'v') && ((str!!2) == '~') && (length str == 4)) =  show (x+1)++".f:"++ [str!!3]++"("++show(x`div`2)++")"
  |(((str!!0) == 'f') && ((str!!2) == '~') && (length str > 4)) =  show (x+1)++".v:"++ slice 4 (length str-2) str++"("++show(x`div`2)++")\n"++ charFound ("v:"++ slice 4 (length str-2) str) (2*(x+1))
  |(((str!!0) == 'v') && ((str!!2) == '~') && (length str > 4)) =  show (x+1)++".f:"++ slice 4 (length str-2) str++"("++show(x`div`2)++")\n"++ charFound ("f:"++ slice 4 (length str-2) str) (2*(x+1))
  |(((str!!0) == 'v') && ((str!!3) == '|') && (length str == 5)) = show (x+1)++".v:"++ [str!!2]++"("++show(x`div`2)++")/"++show (x+2)++".v:"++[str!!4]++"("++show(x`div`2)++")"
  |(((str!!0) == 'f') && ((str!!3) == '|') && (length str == 5)) = show (x+1)++".f:"++ [str!!2]++"("++show(x`div`2)++");"++show (x+2)++".f:"++[str!!4]++"("++show(x`div`2)++")" 
  |(((str!!0) == 'v') && ((str!!3) == '-') && (length str == 6)) =  show (x+1)++".f:"++ [str!!2]++"("++show(x`div`2)++")/"++show (x+2)++".v:"++[str!!5]++"("++show(x`div`2)++")" 
  |(((str!!0) == 'f') && ((str!!3) == '-') && (length str == 6)) =  show (x+1)++".v:"++ [str!!2]++"("++show(x`div`2)++");"++show (x+2)++".f:"++[str!!5]++"("++show(x`div`2)++")"   
pegaSimbulo :: String ->Integer -> Char 
pegaSimbulo (x:xs) a
  |null xs=x
  |x=='('=pegaSimbulo xs (a+1)
  |x==')'=pegaSimbulo xs (a-1)
  |a==0 && x/='v'&&x/='f' &&x/=':'=x 
  |x/='('||x/=')'=pegaSimbulo xs a

valora formula= "f:"++formula

slice from to xs = take (to - from + 1) (drop from xs)
guardaV str x 
  |x==length str=[]
  |(str!!x=='v')&&(str!!(x+1)==':')&&(str!!(x+3)=='(')&&(str!!(x+2)/='~')=[str!!(x+2)]:guardaV str (x+1)
  |otherwise=guardaV str (x+1)
guardaF str x 
  |x==length str=[]
  |(str!!x=='f')&&(str!!(x+1)==':')&&(str!!(x+3)=='(')&&(str!!(x+2)/='~')=[str!!(x+2)]:guardaF str (x+1)
  |otherwise=guardaF str (x+1)
vetorFinal vetorF vetorV= [x | x<-vetorV, x `notElem` vetorF]
deduz vetorFinal
  |(length vetorFinal==0)="valido"
  |otherwise="invalido"

converte str x
  |(str!!x/='.')=[str!!x]++converte str (x+1)
  |otherwise=""
divide :: [Char] -> Int -> [[Char]]
divide str x
  |(x==length str)=[]
  |(str!!x==';')=converte str (x+1):divide str (x+1)
  |otherwise =divide str (x+1)
divideR str x
  |(x==length str)=[]
  |(str!!x=='/')=converte str (x+1):divideR str (x+1)
  |otherwise =divideR str (x+1)

estrutura :: (Num a,Num b, Read a, Read b) => [String] -> [String] -> [[(a, b)]]
estrutura xs zs=[[((read x)-1,read x)|x<-xs],[((read z)-1,read z)|z<-zs]]

main :: IO ()
main = do 
    putStrLn "Digite a fórmula:"
    formula <- getLine
    let format= addExternalParenthesis formula
    let val= valora formula
    let par=getMatchingParenthesis formula
    let sub= sliceSubFormulas format par
    let subs= getExternalSubFormulas formula par
    let inter=getInternalSubFormulas formula
    let resp= charFound val 0
    let externosub= sliceSubFormulas formula subs
    let test= sliceSubFormulas formula inter
    let final= [x | x<-sub, x `notElem` test]
    let teste= pegaSimbulo formula 0
    let vetorV= guardaV resp 0
    let vetorF= guardaF resp 0
    let vFinal= vetorFinal vetorF vetorV++vetorFinal vetorV vetorF
    let resposta=deduz vFinal
    let div=divide resp 0
    let divr=divideR resp 0
    let struct=estrutura div divr
    print "simbulo externo"
    print teste
    print "todas subformulas"
    print sub
    print "external subformulas parenteses"
    print subs
    print "external subformulas "
    print externosub
    print "internal subformulas parenteses"
    print inter
    print "subformulas"
    print test
    print "subformulas not elem"
    print final
    print "subformulas parenteses"
    print par
    print "subformulas parenteses"
    print vetorV
    print vetorF
    print vFinal
    print resposta
    putStr resp
    print div
    print struct
