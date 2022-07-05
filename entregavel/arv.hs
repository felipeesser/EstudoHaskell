import Data.List (sort, nub, group)
--ghc arv.hs
-- ./arv.exe
--((p)|(q&r))->((p|q)&(p|r))
--(a)->((a)->(b->a))
--(b)->((a)&(b|a))

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

--retorna todos as subfórmulas que sejam internas a outras  
getInternalSubFormulas :: String -> [(Int, Int)]
getInternalSubFormulas formula = nub (concat (filter (\x -> (length x /= 0)) (allNested (sort (getMatchingParenthesis formula)))))

--retorna subformulas em parenteses não aninhados
getExternalSubFormulas :: String -> [(Int, Int)] -> [(Int, Int)]
getExternalSubFormulas formula matchingParenthesis= [x | x<-matchingParenthesis, x `notElem` getInternalSubFormulas formula]

--retorna todos os índices de parênteses internos
allNested :: (Ord a1, Ord a2) => [(a1, a2)] -> [[(a1, a2)]]
allNested matchingParenthesis = [(generateNestedParenthesisList (fst x) (snd x) matchingParenthesis) | x<-matchingParenthesis]

--retorna todos os índices de parênteses internos a uma determinada subfórmula
generateNestedParenthesisList :: (Ord a1, Ord a2) => a1 -> a2 -> [(a1, a2)] -> [(a1, a2)]
generateNestedParenthesisList start end matchingParenthesis = [ x | x <- (sort matchingParenthesis), (fst x) > start, (snd x) < end]


-- aplica as regras a uma formula
charFound str
  |(((str!!0) == 'v') && ((str!!3) == '&') && (length str == 5)) = [";","v:"++[str!!2],"v:"++[str!!4]]
  |(((str!!0) == 'f') && ((str!!3) == '&') && (length str == 5)) = ["/","f:"++[str!!2],"f:"++[str!!4]]
  |(str!!0 == 'v')&& (length str>5) && (pegaSimbulo str 0 =='&') = [";","v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0),"v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)]
  |(str!!0 == 'f')&& (length str>5) && (pegaSimbulo str 0 =='&') = ["/","f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0),"f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)]
  |(((str!!0) == 'v') && ((str!!3) == '|') && (length str == 5)) = ["/","v:"++[str!!2],"v:"++[str!!4]]
  |(((str!!0) == 'f') && ((str!!3) == '|') && (length str == 5)) = [";","f:"++[str!!2],"f:"++[str!!4]]
  |(str!!0 == 'v')&& (length str>5) && (pegaSimbulo str 0 =='|') = ["/","v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0),"v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)]
  |(str!!0 == 'f')&& (length str>5) && (pegaSimbulo str 0 =='|') = [";","f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0),"f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)]
  |(((str!!0) == 'v') && ((str!!3) == '-') && (length str == 6)) = ["/","f:"++[str!!2],"v:"++[str!!5]]
  |(((str!!0) == 'f') && ((str!!3) == '-') && (length str == 6)) = [";","v:"++[str!!2],"f:"++[str!!5]]
  |(str!!0 == 'v')&& (length str>5) && (pegaSimbulo str 0 =='-') = ["/","f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0),"v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)]
  |(str!!0 == 'f')&& (length str>5) && (pegaSimbulo str 0 =='-') = [";","v:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!0),"f:"++(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))!!1)]
  |(((str!!0) == 'f') && ((str!!2) == '~') && (length str == 4)) = ["","","v:"++ [str!!3]]
  |(((str!!0) == 'v') && ((str!!2) == '~') && (length str == 4)) = ["","","f:"++ [str!!3]]
  |(((str!!0) == 'f') && ((str!!2) == '~') && (length str > 4)) = ["","","v:"++ slice 4 (length str-2) str]
  |(((str!!0) == 'v') && ((str!!2) == '~') && (length str > 4)) = ["","","f:"++ slice 4 (length str-2) str]
-- encontra o operador mais externo de uma formula
pegaSimbulo :: String ->Integer -> Char
pegaSimbulo (x:xs) a
  |null xs=x
  |x=='('=pegaSimbulo xs (a+1)
  |x==')'=pegaSimbulo xs (a-1)
  |a==0 && x/='v'&&x/='f' &&x/=':'=x
  |x/='('||x/=')'=pegaSimbulo xs a

valora formula= "f:"++formula

slice from to xs = take (to - from + 1) (drop from xs)
guardaF lista x
  |(x== length lista)=[]
  |(length (lista!!x!!0)==3)&&(lista!!x!!0!!0=='f')=[lista!!x!!0!!2]:guardaF lista (x+1)
  |otherwise=guardaF lista (x+1)
guardaV lista x
  |(x== length lista)=[]
  |(length (lista!!x!!0)==3)&&(lista!!x!!0!!0=='v')=[lista!!x!!0!!2]:guardaV lista (x+1)
  |otherwise=guardaV lista (x+1)
deduz vetor1 vetor2
  |(length ([x | x<-vetor1, x `elem` vetor2]++[x | x<-vetor2, x `elem` vetor1])>0)=[]
  |otherwise= [["v:"++x] | x<-vetor1, x `notElem` vetor2]++[["f:"++x] | x<-vetor2, x `notElem` vetor1]
valida lista x bool final
  |(x==length lista||(not bool))=(lista,[bool],final)
  |(length (deduz (guardaV (lista!!x) 0) (guardaF (lista!!x) 0))==0)=valida lista (x+1) True (deduz (guardaV (lista!!x) 0) (guardaF (lista!!x) 0))
  |(length (deduz (guardaV (lista!!x) 0) (guardaF (lista!!x) 0))>0)=valida lista (x+1) False (deduz (guardaV (lista!!x) 0) (guardaF (lista!!x) 0))
-- sinaliza que esta resolvida e coloca o resultado no fim de um ramo 
acha2 lista elem1 elem2 count x
  |(count==length lista)=[]
  |(x==count&&elem1/=[""])=[lista!!x!!0,"R"]:elem1:elem2:acha2 lista elem1 elem2 (count+1) x 
  |(x==count&&elem1==[""])=[lista!!x!!0,"R"]:elem2:acha2 lista elem1 elem2 (count+1) x 
  |otherwise =lista!!count:acha2 lista elem1 elem2 (count+1) x
--formula a ser resolvida esta no fim do ramo e gera simbulo ;
achouFim elem1 elem2 lista x z bool
    |(z==length lista)=[]
    |(verifica (lista!!z) 0 && bool)=[acha2 (lista!!z) elem1 elem2 0 x]++achouFim elem1 elem2 lista x (z+1) False  
    |(bool)  = [lista!!z]++achouFim elem1 elem2 lista x (z+1) True
    |(not bool)= [lista!!z]++achouFim elem1 elem2 lista x (z+1) False
-- sinaliza que esta resolvida e coloca o resultado no fim de todos os ramos que possuam o mesmo nó a ser resolvido
acha3 lista elem1 elem2 count x primeiroelem adiciona
  |(count==length lista && elem1/=[""] && adiciona)=[elem1,elem2]
  |(count==length lista && ((elem1==[""] && adiciona)||(elem1/=[""] && not adiciona)))=[]
  |(x==count&& elem1/=[""] && lista!!x!!0==primeiroelem)=[primeiroelem,"R"]:acha3 lista elem1 elem2 (count+1) x primeiroelem True
  |(x==count&& elem1==[""]&& lista!!x!!0==primeiroelem)=[primeiroelem,"R"]:elem2:acha3 lista elem1 elem2 (count+1) x primeiroelem True 
  |otherwise =lista!!count:acha3 lista elem1 elem2 (count+1) x primeiroelem adiciona
--formula a ser resolvida esta antes do fim do ramo e gera simbulo ;
achouMeio elem1 elem2 lista x z primeiroelem
    |(z==length lista)=[]
    |(verifica (lista!!z) 0)=[acha3 (lista!!z) elem1 elem2 0 x primeiroelem False]++achouMeio elem1 elem2 lista x (z+1) primeiroelem 
    |otherwise= [lista!!z]++achouMeio elem1 elem2 lista x (z+1) primeiroelem 
-- formula a ser resolvida esta no fim do ramo e gera simbulo /
transforma lista x count
  |(count==length lista)=[]
  |(x==count)=[lista!!x!!0,"R"]:transforma lista x (count+1)
  |otherwise=[lista!!count]++transforma lista x (count+1)
acha4 lista elem1 elem2 count x antecessor
  |(count==length lista)=[(transforma lista x 0)++[elem2]]
  |(x==count)=(antecessor++[[lista!!x!!0,"R"],elem1]):acha4 lista elem1 elem2 (count+1) x  antecessor
  |otherwise =acha4 lista elem1 elem2 (count+1) x (antecessor++[lista!!count])
achouFim2 elem1 elem2 lista x z bool
    |(z==length lista)=[]
    |(verifica (lista!!z) 0 && bool)=(acha4 (lista!!z) elem1 elem2 0 x [])++achouFim2 elem1 elem2 lista x (z+1) False  
    |(bool)  = [lista!!z]++achouFim2 elem1 elem2 lista x (z+1) True
    |(not bool)= [lista!!z]++achouFim2 elem1 elem2 lista x (z+1) False
-- formula a ser resolvida esta antes do fim do ramo e gera simbulo /
acha5 lista elem1 elem2 count x=[(transforma lista x 0)++[elem1]]++[(transforma lista x 0)++[elem2]]
achouMeio2 elem1 elem2 lista x z
    |(z==length lista)=[]
    |(verifica (lista!!z) 0)=acha5 (lista!!z) elem1 elem2 0 x++achouMeio2 elem1 elem2 lista x (z+1)  
    |otherwise  = [lista!!z]++achouMeio2 elem1 elem2 lista x (z+1) 
-- diz se o ramo está completo ou incompleto/False-> completo True->incompleto 
verifica lista x 
    |(x==length lista)=False 
    |(lista!!x!!1=="N")=True 
    |(lista!!x!!1=="R")=False||verifica lista (x+1)
-- altera a arvore conforme a resolucao de formulas
atualiza lista x y
    |(x==length (lista!!y))=atualiza lista 0 (y+1)
    |(x/=(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0==";")&&(length ((charFound (lista!!y!!x!!0))!!1)>3&&length ((charFound (lista!!y!!x!!0))!!2)>3)=achouMeio [(charFound (lista!!y!!x!!0))!!1,"N"] [(charFound (lista!!y!!x!!0))!!2,"N"] lista x 0 (lista!!y!!x!!0) 
    |(x/=(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0==";")&&(length ((charFound (lista!!y!!x!!0))!!1)==3&&length ((charFound (lista!!y!!x!!0))!!2)==3)=achouMeio [(charFound (lista!!y!!x!!0))!!1,"R"] [(charFound (lista!!y!!x!!0))!!2,"R"] lista x 0 (lista!!y!!x!!0) 
    |(x/=(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0==";")&&(length ((charFound (lista!!y!!x!!0))!!1)==3&&length ((charFound (lista!!y!!x!!0))!!2)>3)=achouMeio [(charFound (lista!!y!!x!!0))!!1,"R"] [(charFound (lista!!y!!x!!0))!!2,"N"] lista x 0 (lista!!y!!x!!0)  
    |(x/=(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0==";")&&(length ((charFound (lista!!y!!x!!0))!!1)>3&&length ((charFound (lista!!y!!x!!0))!!2)==3)=achouMeio [(charFound (lista!!y!!x!!0))!!1,"N"] [(charFound (lista!!y!!x!!0))!!2,"R"] lista x 0 (lista!!y!!x!!0)   
    |(x==(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0==";")&&(length ((charFound (lista!!y!!x!!0))!!1)>3&&length ((charFound (lista!!y!!x!!0))!!2)>3)=achouFim [(charFound (lista!!y!!x!!0))!!1,"N"] [(charFound (lista!!y!!x!!0))!!2,"N"] lista x 0 True
    |(x==(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0==";")&&(length ((charFound (lista!!y!!x!!0))!!1)==3&&length ((charFound (lista!!y!!x!!0))!!2)==3)=achouFim [(charFound (lista!!y!!x!!0))!!1,"R"] [(charFound (lista!!y!!x!!0))!!2,"R"] lista x 0 True  
    |(x==(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0==";")&&(length ((charFound (lista!!y!!x!!0))!!1)==3&&length ((charFound (lista!!y!!x!!0))!!2)>3)=achouFim [(charFound (lista!!y!!x!!0))!!1,"R"] [(charFound (lista!!y!!x!!0))!!2,"N"] lista x 0 True  
    |(x==(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0==";")&&(length ((charFound (lista!!y!!x!!0))!!1)>3&&length ((charFound (lista!!y!!x!!0))!!2)==3)=achouFim [(charFound (lista!!y!!x!!0))!!1,"N"] [(charFound (lista!!y!!x!!0))!!2,"R"] lista x 0 True 
    |(x==(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="/")&&(length ((charFound (lista!!y!!x!!0))!!1)==3&&length ((charFound (lista!!y!!x!!0))!!2)==3)=achouFim2 [(charFound (lista!!y!!x!!0))!!1,"R"] [(charFound (lista!!y!!x!!0))!!2,"R"] lista x 0 True
    |(x==(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="/")&&(length ((charFound (lista!!y!!x!!0))!!1)>3&&length ((charFound (lista!!y!!x!!0))!!2)==3)=achouFim2 [(charFound (lista!!y!!x!!0))!!1,"N"] [(charFound (lista!!y!!x!!0))!!2,"R"] lista x 0 True
    |(x==(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="/")&&(length ((charFound (lista!!y!!x!!0))!!1)==3&&length ((charFound (lista!!y!!x!!0))!!2)>3)=achouFim2 [(charFound (lista!!y!!x!!0))!!1,"R"] [(charFound (lista!!y!!x!!0))!!2,"N"] lista x 0 True
    |(x==(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="/")&&(length ((charFound (lista!!y!!x!!0))!!1)>3&&length ((charFound (lista!!y!!x!!0))!!2)>3)=achouFim2 [(charFound (lista!!y!!x!!0))!!1,"N"] [(charFound (lista!!y!!x!!0))!!2,"N"] lista x 0 True
    |(x/=(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="/")&&(length ((charFound (lista!!y!!x!!0))!!1)>3&&length ((charFound (lista!!y!!x!!0))!!2)>3)=achouMeio2 [(charFound (lista!!y!!x!!0))!!1,"N"] [(charFound (lista!!y!!x!!0))!!2,"N"] lista x 0 
    |(x/=(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="/")&&(length ((charFound (lista!!y!!x!!0))!!1)>3&&length ((charFound (lista!!y!!x!!0))!!2)==3)=achouMeio2 [(charFound (lista!!y!!x!!0))!!1,"N"] [(charFound (lista!!y!!x!!0))!!2,"R"] lista x 0 
    |(x/=(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="/")&&(length ((charFound (lista!!y!!x!!0))!!1)==3&&length ((charFound (lista!!y!!x!!0))!!2)==3)=achouMeio2 [(charFound (lista!!y!!x!!0))!!1,"R"] [(charFound (lista!!y!!x!!0))!!2,"R"] lista x 0 
    |(x/=(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="/")&&(length ((charFound (lista!!y!!x!!0))!!1)==3&&length ((charFound (lista!!y!!x!!0))!!2)>3)=achouMeio2 [(charFound (lista!!y!!x!!0))!!1,"R"] [(charFound (lista!!y!!x!!0))!!2,"N"] lista x 0 
    |(x/=(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="")&&(length ((charFound (lista!!y!!x!!0))!!2)>3)=achouMeio [""] [(charFound (lista!!y!!x!!0))!!2,"N"] lista x 0 (lista!!y!!x!!0) 
    |(x/=(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="")&&(length ((charFound (lista!!y!!x!!0))!!2)==3)=achouMeio [""] [(charFound (lista!!y!!x!!0))!!2,"R"] lista x 0 (lista!!y!!x!!0) 
    |(x==(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="")&&(length ((charFound (lista!!y!!x!!0))!!2)>3)=achouFim [""] [(charFound (lista!!y!!x!!0))!!2,"N"] lista x 0 True
    |(x==(length (lista!!y)-1))&&(lista!!y!!x!!1=="N")&& ((charFound (lista!!y!!x!!0))!!0=="")&&(length ((charFound (lista!!y!!x!!0))!!2)==3)=achouFim [""] [(charFound (lista!!y!!x!!0))!!2,"R"] lista x 0 True
    |(x<length (lista!!y))= atualiza lista (x+1) y
-- verifica se todos os nos de todos os ramos foram resolvidos
percorre lista x y
  |(y==length lista)=False
  |(x==length (lista!!y))=False||percorre lista 0 (y+1)
  |(lista!!y!!x!!1=="N")=True 
  |(lista!!y!!x!!1=="R")=False||percorre lista (x+1) y
-- gera, a partir de uma formula inicial, uma tupla que contem a arvore de refutacao, a conclusao e os simbulos proposicionais de um ramo aberto
tableaux lista
  |(percorre(atualiza lista 0 0) 0 0)=tableaux (atualiza lista 0 0)
  |otherwise= valida (atualiza lista 0 0) 0 True []

get3h (_,_,a)=a
get2h (_,a,_)=a
get1h (a,_,_)=a
-- imprime a arvore, a conclusao e os simbulos proposicionais de um ramo aberto
imprime tupla= do
  print (get1h tupla)
  putStr ([if x then "Tautologia\n" else "Invalido\n"|x<-get2h tupla]!!0)
  print [head x|x<-group (sort (get3h tupla))]
main :: IO ()
main = do
    let teste1=tableaux [[["f:((p)|(q&r))->((p|q)&(p|r))","N"]]]
    let teste2=tableaux [[["f:(a)->((a)->(b->a))","N"]]]
    let teste3=tableaux [[["f:(b)->((a)&(b|a))","N"]]]
    let teste4=tableaux [[["f:((p)|(q&r))->(~((p|q)&(p|r)))","N"]]] 
    putStrLn "teste1:"
    imprime teste1
    putStrLn ""
    putStrLn "teste2:"
    imprime teste2
    putStrLn ""
    putStrLn "teste3:"
    imprime teste3
    putStrLn ""
    putStrLn "teste4:"
    imprime teste4
    putStrLn ""
    putStrLn "Digite a fórmula:"
    formula <- getLine
    let val= valora formula
    let resp= tableaux[[[val,"N"]]]
    putStrLn "Resposta:"
    imprime resp


