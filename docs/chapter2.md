# Capítulo 2
## Um simples programa em Lisp

> *Certum quod factum.*

> (Só se sabe ao certo o que se constrói.)

> -Giovanni Battista Vico (1668-1744)

> Historiografista real italiano

Você nunca irá torna-se proficiente em uma linguagem estrangeira estudando listas de vocabulários.
Em vez disso, você deve escutar e falar (ou ler e escrever) a linguagem para ganhar proficiência.
O mesmo vale para o aprendizado de linguagens de computador.

Esse capítulo mostra como combinar as funções básicas e formulários especiais do Lisp em um programa completo.
Se você aprender como fazer isso, será fácil adquirir o vocabulário restante do Lisp (como descrito no capítulo 3).

## 2.1 Gramática para um subconjunto de inglês

O programa que desenvolveremos neste capítulo gera sentenças aleatórias em inglês.
Aqui está uma gramática simples para uma pequena parte do inglês:

> *Sentence* => *Noun-Phrase + Verb-Phrase*  
> *Noun-Phrase* => *Article + Noun*  
> *Verb-Phrase* => *Verb + Noun-Phrase*  
> *Article* => *the, a,...*  
> *Noun* => *man, ball, woman, table...*  
> *Verb* => *hit, took, saw, liked...*  

Tradução livre das palavras acima:

* **Sentence** = Sentença
* **Noun-Phrase** = Frase substantiva
* **Verb-Phrase** = Frase verbal
* **Article** = Artigo
* **Noun** = Substantivo
* **Verb** = Verbo
* **the** = a/o/as/os
* **a** = um/uma
* **man, ball, woman, table** = homem, bola, mulher, mesa
* **hit, took, saw, liked** = acertar, pegou, viu, gostou

Para ser técnico, esta descrição é chamada de "gramática de estrutura de frase sem contexto", e o paradigma subjacente é
chamado "sintaxe generativa".
A ideia é que onde quer que desejamos uma sentença, nós podemos gerar uma frase substantiva seguida por uma frase verbal.
Onde quer que uma frase substantiva tenha sido especificada, geramos um artigo seguido por um substantivo.
Onde quer que um artigo tenha sido especificado, nós geramos "the", "a", ou qualquer outro artigo.
O formalismo é "livre de contexto" porque as regras se aplicam em qualquer lugar, independente das palavras ao redor, e a abordagem é "generativa"
porque as regras como um todo definem o conjunto completo de sentenças em um idioma (e por contraste o conjunto de não sentenças também).
A seguir, mostramos a derivação de uma única frase usando as regras:

* Para obter uma *Sentença*, acrescente uma *Frase substantiva* e uma *Frase verbal*
  * Para obter uma *Frase substantiva*, adicione um *Artigo* e um *Substantivo*
    * Escolha *"the"* para o *Artigo*
	* Escolha *"man"* para o *Substantivo*
  * A *Frase substantiva* resultante é *"the man"*
  * Para obter uma *Frase verbal*, acrescente um *Verbo* e uma *Frase substantiva*
    * Escolha *"hit"* para o *Verbo*
    * Para obter uma *Frase substantiva*, adicione um *Artigo* e um *Substantivo*
      * Escolha *"the"* para o *Artigo*
      * Escolha *"ball"* para o *Substantivo*
    * A *Frase substantiva* resultante é *"the ball"*
  * A *Frase verbal* resultante é *"hit the ball"*
* A *Sentença* resultante é *"The man hit the ball"*

## 2.2 Uma solução direta

Nós desenvolveremos um programa que gera sentenças aleatórias a partir de uma gramática de estrutura de frases.
A abordagem mais direta é representar cada regra gramatical por uma função em Lisp separada:

```lisp
(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))
```
Cada uma das definições de funções possui uma lista de parâmetros vazia, `()`.
Isso significa que as funções não aceitam argumentos.
Isso é incomum, porque, estritamente falando, uma função sem argumentos sempre retornaria a mesma coisa, portanto seria
melhor o uso de uma constante.
Contudo, essas funções usam a função `random` (como veremos em breve), e assim podem retornar resultados diferentes 
mesmo sem argumentos.
Entretanto, elas não são funções no sentindo matemático, mas elas ainda são chamadas de funções no Lisp, porque retornam
um valor.

Tudo o que resta agora é definir a função `one-of`.
Ela aceita uma lista de opções possíveis como argumento, escolhe uma delas aleatoriamente e retorna uma lista com o elemento escolhido.
Essa última parte é para que todas as funções da gramática retornem uma lista de palavras.
Dessa maneira, nós podemos aplicar livremente `append` a qualquer categoria. 

```lisp
(defun one-of (set)
  "Escolhe um elemento do conjunto, e constrói uma lista com o mesmo."
  (list (random-elt set)))

(defun random-elt (choices)
  "Escolhe um elemnto da lista em aleatório."
  (elt choices (random (length choices))))
```

Há duas novas funções aqui, `elt` e `random`.
`elt` escolhe e retorna um elemento de uma lista.
O primeiro argumento é a lista, e o segundo é a posição na lista.
A parte confusa é que as posições começam em 0, então `(elt choices 0)` é o primeiro elemento da lista, 
e `(elt choices 1)` é o segundo.
Pense no número da posição como sendo sempre o próximo elemento da lista, então partindo do zero, 
o zero é o primeiro elemento da lista, o um o segundo, o dois o terceiro, e assim por diante.
A expressão `(random n)` retorna um número inteiro de 0 a n-1, então `(random 4)` deve retornar 0, 1, 2, ou 3.

Agora podemos testar o programa gerando algumas sentenças aleatórias, juntamente com uma frase substantiva e uma frase verbal:

```lisp
> (sentence) => (THE WOMAN HIT THE BALL)

> (sentence) => (THE WOMAN HIT THE MAN)

> (sentence) =>(THE BALL SAW THE WOMAN)

> (sentence) => (THE BALL SAW THE TABLE)

> (noun-phrase) => (THE MAN)

> (verb-phrase) => (LIKED THE WOMAN)

> (trace sentence noun-phrase verb-phrase article noun verb) =>
(SENTENCE NOUN-PHRASE VERB-PHRASE ARTICLE NOUN VERB)

> (sentence) =>
(1 ENTER SENTENCE)
  (1 ENTER NOUN-PHRASE)
    (1 ENTER ARTICLE)
    (1 EXIT ARTICLE: (THE))
    (1 ENTER NOUN)
    (1 EXIT NOUN: (MAN))
  (1 EXIT NOUN-PHRASE: (THE MAN))
  (1 ENTER VERB-PHRASE)
    (1 ENTER VERB)
    (1 EXIT VERB: (HIT))
    (1 ENTER NOUN-PHRASE)
      (1 ENTER ARTICLE)
      (1 EXIT ARTICLE: (THE))
      (1 ENTER NOUN)
      (1 EXIT NOUN: (BALL))
    (1 EXIT NOUN-PHRASE: (THE BALL))
  (1 EXIT VERB-PHRASE: (HIT THE BALL))
(1 EXIT SENTENCE: (THE MAN HIT THE BALL))
(THE MAN HIT THE BALL)
```

O programa funciona bem e o rastreamento se parece exatamente como a derivação mostrada anteriormente, mas as definições do Lisp 
são um pouco mais difíceis de ler do que as regras gramaticais originais.
Esse problema será agravado à medida que considerarmos regras mais complexas.
Suponha que desejássemos permitir que as frases substantivas fossem modificadas por um número indefinido de adjetivos e um
número indefinido de frases preposicionais.
Na notação gramatical, podemos ter as seguintes regras:

> *Noun-Phrase => Article + Adj\* + Noun + PP\*  
> Adj\* => 0&#x0338;, Adj + Adj\*  
> PP\* => 0&#x0338;, PP + PP\*  
> PP => Prep + Noun-Phrase  
> Adj => big, little, blue, green, ...  
> Prep => to, in, by, with, ...*

Nessa notação, 0&#x0338; indica a escolha de nada, a vírgula indica a escolha de várias alternativas, e o asterisco não
é nada em especial no Lisp, é apenas parte do nome do símbolo.
No entanto, a convenção usada aqui é que nomes que terminam com um asterisco indicam zero ou mais repetições do nome
subjacente.
Isso é, *PP\** indica zero ou mais repetições de *PP*.
<a id="tfn02-1"></a>
Isso é conhecido como notação "Kleene star" (pronuncia-se "clean-E"), "Kleene" por conta do matemático Stephen Cole Kleene.

O problema é que as regras para *Adj\** e *PP\** contém opções que deveríamos representar como algum tipo de
condicional no Lisp. Por exemplo:

```lisp
(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))
```

Eu escolhi duas implementações diferentes para `Adj*` e `PP*`; qualquer abordagem funcionaria em qualquer função.
No entanto, temos que ter cuidado; aqui estão duas abordagens que não funcionariam:

```lisp
(defun Adj* ()
  "Warning - incorrect definition of Adjectives.
   pt-br: Aviso - definição incorreta de adjetivos."
  (one-of '(nil (append (Adj) (Adj*)))))
(defun Adj* ()
  "Warning - incorrect definition of Adjectives.
   pt-br: Aviso - definição incorreta de adjetivos."
  (one-of (list nil (append (Adj) (Adj*)))))
```

A primeira definição é errada porque retornaria a expressão literal 
`((append (Adj) (Adj*)))` enquanto o esperado é uma lista de palavras.
A segunda definição causaria uma recursão infinita, porque computando o valor de 
`(Adj*)` sempre envolve uma chamada recursiva para `(Adj*)`. 
O ponto é que o que começou como funções simples agora está se tornando bastante complexo.
Pra entendê-las, precisamos de saber muitas convenções do Lisp como: 
`defun, (), case, if`, `quote`, e as regras para a ordem da avaliação, 
quando, idealmente, a implementação de uma regra gramatical deve usar apenas 
convenções *linguísticas*.
Se quiséssemos desenvolver uma gramática maior, o problema poderia piorar, porque o 
escritor de regras pode precisar depender cada vez mais do Lisp.

## 2.3 Uma solução baseada em regras

Uma implementação alternativa desse programa se concentraria em facilitar a escrita de regras gramaticais e 
deixaria a preocupação com o modo como elas serão processadas para depois.
Vejamos de novo as regras gramaticais originais:

> *Sentence => Noun-Phrase + Verb-Phrase  
> Noun-Phrase => Article + Noun  
> Verb-Phrase => Verb + Noun-Phrase  
> Article => the, a, ...  
> Noun => man, ball, woman, table...  
> Verb => hit, took, saw, liked...*

Cada regra consiste de uma seta com um símbolo do lado esquerdo e alguma outra coisa do lado direito.
A complicação é que podem ser dois tipos de lados direito: uma lista concatenada de símbolos, como em
"*Noun-Phrase => Article + Noun*", ou uma lista de palavras alternativas, como em "*Noun => man, ball, woman, table...*"
Podemos considerar essas possibilidades, decidindo que todas as regras terão uma lista de possibilidades no
lado direito, e que uma lista concatenada, for example "*Article + Noun*" será representada como uma lista em Lisp,
por exemplo, `(Article Noun)`.
A lista de regras pode ser representada da seguinte maneira:

```lisp
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "Uma gramática para um subconjunto trivial de inglês.")

(defvar *grammar* *simple-grammar*
  "A gramática utilizada por generate. Inicialmente é
  *simple-grammar*, mas podemos mudar para outas gramáticas.")
```

Note que a versão do Lisp das regras imita a versão original.
Em particular, incluo o símbolo "->", mesmo que não sirva a nenhum propósito real; é puramente decorativo.

Ambos os formulários especiais `defvar` e `defparameter` introduzem variáveis especiais e atribuem um valor a elas; a
diferença é que uma *variável*, como `*grammar*`, é frequentemente alterada durante a execução do programa.
Um *parâmetro*, como `*simple-grammar*`, por outro lado, irá normalmente permanecerá constante.
Uma alteração de um parâmetro é considerada uma alteração *no* programa, não uma alteração *pelo* programa. 

Uma vez que a lista de regras foi definida, ela pode ser usada para buscar reescritas possíveis de um determinado 
símbolo de categoria.
A função `assoc` é designada para esse tipo de tarefa.
Ela aceita dois argumentos, uma "chave" e uma lista de listas, e retorna o primeiro elemento da lista de listas que
começar com a chave.
Se não houver, ela retorna `nil`.
Aqui está um exemplo:

```lisp
> (assoc 'noun *grammar*) => (NOUN -> MAN BALL WOMAN TABLE)
```

Embora as regras sejam simplesmente implementadas como listas, é uma boa idéia impor uma camada de abstração definindo
funções para operar com as regras.
Precisaremos de três funções: uma para obter o lado direito de uma regra, uma para o lado esquerdo, e uma para procurar
todas as possíveis reescritas (dos lados direitos) de uma categoria.

```lisp
(defun rule-lhs (rule)
  "O lado esquerdo de uma regra."
  (first rule))

(defun rule-rhs (rule)
  "O lado direito de uma regra."
  (rest (rest rule)))

(defun rewrites (category)
  "Retorne uma lista das possíveis reescritas para esta categoria."
  (rule-rhs (assoc category *grammar*)))
```

Definir essas funções facilitará a leitura dos programas que a utilizam, e também 
e também facilita a alteração da representação das regras, se decidirmos mudá-la.

Estamos agora prontos para resolver o problema principal: definir uma função que gere sentenças (ou frases
substantivas ou qualquer outra categoria).
Iremos chamar essa função de `generate`.
Ela terá que lidar com três casos:
(1) No caso mais simples, `generate` recebe um símbolo que possui um conjunto de regras de reescrita associadas a ele.
Nós escolhemos um deles aleatoriamente e depois geramos a partir disso.
(2) Se o símbolo não tem regras de reescrita possíveis, ele deve ser um símbolo terminal, qual é uma palavra, e não uma
categoria gramatical, e queremos deixá-lo em paz.
Na verdade, retornamos uma lista com a palavra de entrada, porque, como no programa anterior, queremos que todos os
resultados sejam listas de palavras.
(3) Em alguns casos, quando o símbolo tem reescritas, escolheremos um que seja uma lista de símbolos, e tentamos gerar a
partir disso.
Portanto, `generate` deve também aceitar uma lista como entrada, nessa caso, deve gerar cada elemento da lista, e em
seguida anexá-los todos juntos.
A seguir, a primeira condição em `generate` lida com esse caso, enquanto a segunda lida com o caso (1) e a terceira lida
com o caso (2). 
Observe que usamos a função `mappend` da seção 1.7 (page 18).

```lisp
(defun generate (phrase)
  "Gere uma sentença ou frase aleatória."
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))
```

Como muitos dos programas deste livro, essa função é curta, mas densa de informações: o ofício da programação inclui
sabe o que *não* escrever, bem como o que escrever.

Esse estilo de programação é chamado programação *orientado a dados*, porque os dados (a lista de reescritas associadas
com uma categoria) direcionam o que o programa faz a seguir.
É um estilo natural e fácil de usar no Lisp, levando programas concisos e extensíveis, porque sempre é possível
adicionar um novo dado com uma nova associação sem precisar modificar o programa original.

Aqui está alguns exemplos de uso de `generate`:

```lisp
> (generate 'sentence) => (THE TABLE SAW THE BALL)

> (generate 'sentence) => (THE WOMAN HIT A TABLE)

> (generate 'noun-phrase) => (THE MAN)

> (generate 'verb-phrase) (TOOK A TABLE)
```

Há muitas possíveis maneiras de escrever `generate`.
A seguir uma versão usando `if` ao invés de `cond`:

```lisp
(defun generate (phrase)
  "Generate a random sentence or phrase"
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((choices (rewrites phrase)))
        (if (null choices)
            (list phrase)
            (generate (random-elt choices))))))
```

Esta versão usa o formulário especial `let`, qual introduz uma nova variável (nesse caso, `choices`) e também liga a variável a um valor.
Neste caso, a introdução da variável nos evita de chamar a função `rewrites` duas vezes, como foi feito na versão `cond`
de `generate`.
O fomulários geral de um `let` é:

```lisp
    `(let` ((*var value*)...)
        *body-containing-vars*)
```

`let` é a maneira mais comum de introduzir variáveis que não são parâmetros de funções.
É preciso resistir à tentação de usar uma variável sem introduzi-la:

```lisp
(defun generate (phrase)
  (setf choices ...)         ;; errado!
  ... choices ...)
```
Isso é errado porque o símbolo `choices` agora está se referindo a uma variável especial ou global, que pode ser compartilhada ou alterada por outras funções.
Portanto, a função `generate` não é confiável, porque não há garantia que `choices` irá manter o mesmo valor desde o momento em que é definida até o momento em que é referenciada novamente.

Com `let` nós introduzimos uma variável nova que ninguém mais pode acessar; portanto, é garantido manter o valor adequado.

&#9635; **Exercício 2.1 [m]** Escreva uma versão de `generate`que use `cond` mas sem chamar `rewrites` duas vezes.

&#9635; **Exercício 2.2 [m]** Escreva uma versão de `generate` que diferencia explicitamente entre símbolos terminais
(aqueles sem regras de reescrita) e símbolos não terminais.

## 2.4 Dois caminhos a seguir

As duas versões dos programas precedentes representam duas abordagens alternativas que surgem repentinamente no
desenvolvimento de programas:
(1) Use o mapeamento mais direto da descrição do problema diretamente no código Lisp. 
(2) Use a notação mais natural disponível para resolver o problema e depois se preocupe em escrever um 
intérprete para essa notação.

A abordagem (2) envolve um passo extra, e assim é mais trabalho para pequenos problemas.
No entanto, programas que usam esta abordagem geralmente são mais fácies de modificar e expandir.
Isso é especialmente verdade em um domínio em que há muitos dados para contabilizar.
A gramática da linguagem natural é um desses domínios, na verdade, a maioria dos problemas 
de IA se encaixa nessa descrição.
A ideia por trás  da abordagem (2) é trabalhar com o problema o máximo possível em seus próprios termos,
e minimizar a parte da solução escrita diretamente no Lisp.

Felizmente, é muito fácil no Lisp projetar novas notações, e por efeito, novas linguagens de programação. 
Assim, Lisp encoraja a construção de programas mais robustos.
Ao longo desse livro, estaremos cientes das duas abordagens.
O leitor pode perceber que, na maioria dos casos, escolhemos o segundo.

## 2.5 Alterando a gramática sem alterar o programa

Mostramos a utilidade da abordagem (2) definindo uma nova gramática que inclui adjetivos, frases preposicionais, nomes próprios, e pronomes.
Nós podemos então aplicar a função `generate` sem modificações a esta nova gramática. 


```lisp
(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

> (generate 'sentence)
(A TABLE ON A TABLE IN THE BLUE ADIABATIC MAN SAW ROBIN
 WITH A LITTLE WOMAN)

> (generate 'sentence)
(TERRY SAW A ADIABATIC TABLE ON THE GREEN BALL BY THAT WITH KIM
 IN THESE BY A GREEN WOMAN BY A LITTLE ADIABATIC TABLE IN ROBIN
 ON LEE)

> (generate 'sentence)
(THE GREEN TABLE HIT IT WITH HE)
```

Observe o problema de concordância de casos para pronomes:
o programa gerou "with he", embora "with him" seja a forma gramatical correta.
Além disso, é claro que o programa não distingue a produção sensata da tola.

## 2.6 Using the Same Data for Several Programs

Another advantage of representing information in a declarative form-as rules or facts rather than as Lisp functions-is that it can be easier to use the information for multiple purposes.
Suppose we wanted a function that would generate not just the list of words in a sentence but a representation of the complete syntax of a sentence.
For example, instead of the list `(a woman took a ball)`, we want to get the nested list:

```lisp
(SENTENCE (NOUN-PHRASE (ARTICLE A) (NOUN WOMAN))
          (VERB-PHRASE (VERB TOOK)
                       (NOUN-PHRASE (ARTICLE A) (NOUN BALL))))
```

This corresponds to the tree that linguists draw as in figure 2.1.

![Figure 2.1: Sentence Parse Tree](images/chapter2/f02-01.jpg)
**Figure 2.1: Sentence Parse Tree**

Using the "straightforward functions" approach we would be stuck; we'd have to rewrite every function to generate the additional structure.
With the "new notation" approach we could keep the grammar as it is and just write one new function: a version of `generate` that produces nested lists.
The two changes are to `cons` the category onto the front of each rewrite, and then not to `append` together the results but rather just list them with `mapcar`:

```lisp
(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))
```

Here are some examples:

```lisp
> (generate-tree 'Sentence)
(SENTENCE (NOUN-PHRASE (ARTICLE A)
                       (ADJ*)
                       (NOUN WOMAN)
                       (PP*))
      (VERB-PHRASE (VERB HIT)
                       (NOUN-PHRASE (PRONOUN HE))
                       (PP*)))

> (generate-tree 'Sentence)
(SENTENCE (NOUN-PHRASE (ARTICLE A)
                       (NOUN WOMAN))
          (VERB-PHRASE (VERB TOOK)
                       (NOUN-PHRASE (ARTICLE A) (NOUN BALL))))
```

As another example of the one-data/multiple-program approach, we can develop a function to generate all possible rewrites of a phrase.
The function `generate-all` returns a list of phrases rather than just one, and we define an auxiliary function, `combine-all`, to manage the combination of results.
Also, there are four cases instead of three, because we have to check for nil explicitly.
Still, the complete program is quite simple:

```lisp
(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))
```

We can now use `generate-all` to test our original little grammar.
Note that a serious drawback of `generate-all` is that it can't deal with recursive grammar rules like 'Adj\* => Adj + Adj\*' that appear in `*bigger-grammar*,` since these lead to an infinite number of outputs.
But it works fine for finite languages, like the language generated by `*simple-grammar*`:

```lisp
> (generate-all 'Article)

((THE) (A))

> (generate-all 'Noun)

((MAN) (BALL) (WOMAN) (TABLE))

> (generate-all 'noun-phrase)
((A MAN) (A BALL) (A WOMAN) (A TABLE)
 (THE MAN) (THE BALL) (THE WOMAN) (THE TABLE))

> (length (generate-all 'sentence))
256
```

There are 256 sentences because every sentence in this language has the form Article-Noun-Verb-Article-Noun, and there are two articles, four nouns and four verbs (2 x 4 x 4 x 2 x 4 = 256).

## 2.7 Exercises

&#9635; **Exercise  2.3 [h]** Write a trivial grammar for some other language.
This can be a natural language other than English, or perhaps a subset of a computer language.

&#9635; **Exercise  2.4 [m]** One way of describing `combine-all` is that it calculates the cross-product of the function `append` on the argument lists.
Write the higher-order function `cross-product`, and define `combine-all` in terms of it.

The moral is to make your code as general as possible, because you never know what you may want to do with it next.

## 2.8 Answers

### Answer 2.1

```lisp
  (defun generate (phrase)
  "Gere uma sentença ou frase aleatória."
  (let ((choices nil))
    (cond ((listp phrase)
        (mappend #'generate phrase))
       ((setf choices (rewrites phrase))
        (generate (random-elt choices)))
       (t (list phrase)))))
```

### Answer 2.2

```lisp
(defun generate (phrase)
  "Gere uma sentença ou frase aleatória."
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((non-terminal-p phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun non-terminal-p (category)
  "Verdadeiro se essa for uma categoria na gramática."
  (not (null (rewrites category))))
```

### Answer 2.4

```lisp
(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y))
                       xlist))
           ylist))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x"
  (cross-product #'append xlist ylist))
```

Now we can use the `cross-product` in other ways as well:

```
> (cross-product #'+ '(1 2 3) '(10 20 30))
(11 12 13
 21 22 23
 31 32 33)

> (cross-product #'list '(a b c d e f g h)
                        '(1 2 3 4 5 6 7 8))
((A 1) (B 1) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)
 (A 2) (B 2) (C 2) (D 2) (E 2) (F 2) (G 2) (H 2)
 (A 3) (B 3) (C 3) (D 3) (E 3) (F 3) (G 3) (H 3)
 (A 4) (B 4) (C 4) (D 4) (E 4) (F 4) (G 4) (H 4)
 (A 5) (B 5) (C 5) (D 5) (E 5) (F 5) (G 5) (H 5)
 (A 6) (B 6) (C 6) (D 6) (E 6) (F 6) (G 6) (H 6)
 (A 7) (B 7) (C 7) (D 7) (E 7) (F 7) (G 7) (H 7)
 (A 8) (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8))
```

----------------------
<a id="fn02-1"></a>
[1](#tfn02-1) Em breve, veremos a notação "Kleene plus", em que *PP+* indica uma ou mais repetições de *PP*.
	
