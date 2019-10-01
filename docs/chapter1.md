# Capítulo 1

## Introdução ao Lisp 

>Você acha que sabe quando você aprende, tem mais certeza quando você pode escrever, ainda mais quando você pode
>ensinar, mas certo quando você pode programar.
> - Alan Perlis, Cientista da computação da Universidade de Yale

Este capítulo é para pessoas com pouca ou nenhuma experiência em Lisp. Leitores que sentem confiantes em sua habilidade
de programação em Lisp, podem rapidamente dar uma olhada no capítulo ou ignorá-lo completamente. Este capítulo
necessariamente se move rapidamente, então aqueles com pouca experiência em programação, ou qualquer leitor que ache
este capítulo difícil, deve procurar um texto introdutório suplementar. Minhas recomendações estão no prefácio.

Computadores permitem realizar computações. Um programa de processamento de palavras lida com palavras, enquanto uma
calculadora lida com números, mas os princípios são os mesmos. Em ambos os casos, você fornece a entrada (palavras ou
números) e especifica as operações (como excluir uma palavra ou adicionar dois números) para produzir um resultado (um
documento ou resultado de um cálculo).

Vamos nos referir a qualquer coisa que possa ser representada na memória de um computador como um objeto computacional,
ou apenas um objeto. Assim, palavras, parágrafos e números podem ser objetos. E como as operações (exclusão e adição)
devem estar representadas em algum lugar da memória do computador, elas também são objetos.

Normalmente, a distinção entre um “usuário” de computador e um “programador” de computador é que o usuário fornece novas
entradas, ou dados (palavras ou números), enquanto o programador define novas operações, ou programas, e também novos
tipos de dados. Todo objeto novo, seja um dado ou uma operação, deve ser definido em termos de objetos previamente
definidos. A má notícia é que pode ser muito tedioso obter essas definições corretamente. A boa notícia e que cada novo
objeto, pode, por sua vez, ser usado na definição de objetos futuros. Assim, mesmo programas complexos podem ser
construídos a partir de objetos menores e mais simples. Este livro cobre uma série de problemas típicos de IA, mostrando
como cada problema pode ser dividido em partes gerenciáveis, e também como cada peça pode ser descrita na linguagem de
programação Common Lisp. O ideal é que os leitores aprendam o suficiente estudando esses exemplos para atacar novos
problemas de IA com estilo, graça e sucesso.

Vamos considerar um exemplo simples de uma computação: encontrar a soma de dois números, digamos, 2 e 2. Se tivéssemos
uma calculadora à mão, digitaríamos "2 + 2 =" e veríamos a resposta exibida. Em uma calculadora usando a notação
polonesa reversa, teríamos que digitar “2 2 +” para ver a mesma resposta. Em Lisp, assim como na calculadora, o usuário
realiza um diálogo interativo com o computador, digitando uma expressão e vendo o computador imprimir o valor dessa
expressão. Esse modo interativo é diferente de muitas outras linguagens de programação que oferecem apenas um modo de
lote, em que um programa inteiro é compilado e executado antes que qualquer saída possa ser vista.

Em uma calculadora de bolso, nós começamos invertendo o botão liga/desliga. O programa Lisp também deve ser iniciado,
mas os detalhes variam de um computador para outro, então não posso explicar como Lisp funcionará para você. Assumindo
que conseguimos iniciar o Lisp, provavelmente veremos algum tipo de prompt. No meu computador, o Lisp digita “`>`” para
indicar que está pronto para aceitar a próxima computação. Então nos deparamos com uma tela que se parece com isso:

```lisp
> 
```

Podemos agora digitar nossa computação e ver o resultado exibido. Acontece que a convenção de Lisp para expressões
aritméticas é um pouco diferente: uma computação consiste em uma lista de parênteses com o nome da operação primeiro
seguido por qualquer número de operandos, ou argumentos. Isso é chamado de notação de prefixo.

```lisp
> (+ 2 2)
4
> 
```

Vemos que Lisp imprimiu a resposta 4, e depois outro prompt ">", para indicar que está pronto para o próximo cálculo. Ao
longo deste livro, todas as expressões Lisp serão exibidas na fonte `typewriter`. O texto na mesma linha do primeiro
prompt ">" é digitado pelo usuário e o texto seguinte é impresso pelo computador. Normalmente, a entrada digitada pelo
programador estará em letras `lowercase`, enquanto a saída que é impressa pelo computador, estará em letras
`UPPERCASE`. Claro, com símbolos como + e 4, não há diferença.

Para economizar espaço na página, a saída será mostrada às vezes na mesma linha da entrada, separada por uma seta `=>`,
que pode ser lida como "avaliada para", e também pode ser considerada como return ou ENTER que o usuário pressiona para
completar a entrada:

```lisp
> (+ 2 2) => 4
```

Uma vantagem da notação de prefixo entre parênteses, é que os parênteses marcam claramente o início e o fim de uma
expressão. Se quisermos, poderíamos fornecer mais de dois argumentos para `+`, e ainda adicionar todos eles:

```lisp
> (+ 1 2 3 4 5 6 7 8 9 10) => 55 
```

Desta vez tentamos (9000 + 900 + 90 + 9) - (5000 + 500 + 50 + 5):

```lisp
> (- (+ 9000  900  90  9 ) (+ 5000  500  50  5 )) => 4444
```

Este exemplo mostra que as expressões podem ser aninhadas. Os argumentos para a função `-` são listas de parênteses,
enquanto os argumentos para cada função `+` são átomos. A notação de Lisp pode parecer incomum em comparação à notação
matemática padrão, mas há vantagens nessa notação; como as expressões de Lisp podem consistir de uma função seguida por
qualquer número de argumentos, não precisamos continuar repetindo `+`, mais importante que a notação é a regra para
avaliação. Em Lisp, as listas são avaliadas primeiro avaliando todos os argumentos, e em seguida, aplicando a função ao
argumentos, calculando assim o resultado. Essa regra é muito mais simples que a regra para avaliar expressões
matemáticas normais, onde há muitas convenções a serem lembradas, como fazer multiplicações e divisões antes de somas e
diferenças. Entretanto, veremos abaixo que a regra real de avaliação Lisp é uma pouco mais complicada, mas não muito.

Às vezes, os programadores que estão familiarizados com outras linguagens têm preconceitos que dificultam o aprendizado
de Lisp. Para eles, vale a pena destacar três pontos aqui. Primeiro, muitas outras linguagens fazem uma distinção entre
declarações e expressões. Uma expressão, como `2 + 2`, tem um valor, mas não tem uma declaração, como `x = 2 + 2`. As
declarações têm efeitos, mas não retornam valores. Em Lisp, não existe tal distinção: toda expressão retorna um valor. É
verdade que algumas expressões têm efeitos, mas mesmo essas expressões também retornam valores.

Em segundo lugar, as regras lexicais do Lisp são muito mais simples do que as regras de outras linguagens. Em
particular, há menos caracteres de pontuação: apenas parênteses, marcas de aspas (podem ser simples, dupla ou
invertida), espaços, e a vírgula, quais servem para separar símbolos um do outro. Assim, enquanto a declaração `y=a*x+3`
é analisada como sete tokens separados em outras linguagens, em Lisp ela seria tratada como um único símbolo. Para obter
uma lista de tokens, teríamos que inserir espaços: `(y = a * x + 3)`[1](#fn01-1)

Terceiro, enquanto muitas linguagens usam ponto e vírgula para demilitar instruções, o Lisp não precisa de ponto e
vírgula, já que as expressões são delimitadas por parênteses. O Lisp escolhe usar ponto e vírgula para outro propósito:
marcar o início de um comentário, que dura até o final da linha:

```lisp
> (+ 2 2) ;  este é um comentário 
4
```

## Computação Simbólica

Tudo o que fizemos até agora foi manipular números da mesma maneira que uma simples calculadora de bolso faria, Lisp é
mais útil que uma calculadora por dois motivos principais. Primeiro, ele nos permite manipular objetos além de números
e, segundo, permite definir novos objetos que podem ser úteis em cálculos subsequentes. vamos examinar essas duas
propriedades importantes por sua vez.

Além de números, Lisp pode representar caracteres (letras), sequências de caracteres e símbolos arbitrários, onde somos
livres para interpretar esses símbolos como se referindo a coisas fora do mundo da matemática. O Lisp também pode criar
objetos que não são átomos combinando vários objetos em uma lista. Esta capacidade é fundamental e bem suportada na
linguagem; na verdade, o nome Lisp é abreviação de List Processing (Processamento de lista).

Aqui está um exemplo de computação em listas:

```lisp
> (append '(Pat Kim) '(Robin Sandy)) => (PAT KIM ROBIN SANDY)
```

Esta expressão acrescenta duas listas de nomes. A regra para avaliar essa expressão é igual a regra para cálculos
numéricos: avalie todos os argumentos (no caso acima, '(Pat Kim) e '(Robin Sandy)) e depois aplique a função (no caso
acima, append) ao valor dos argumentos.

A parte incomum é a marca de aspas `(')` antes dos argumentos (Pat Kim) e (Robin Sandy), a aspas simples serve para
bloquear a avaliação da seguinte expressão, retornando-a literalmente. Se apenas tivéssemos a expressão `(Pat Kim)`, ela
seria avaliada considerando `Pat` como uma função e aplicando-a ao valor da expressão `Kim`. Não é isso que tínhamos em
mente, se não existir uma função definida chamada ´Pat`, receberemos um erro. A marca de aspas instrui ao Lisp a tratar a
lista como um fragmento de dados e não como uma chamada de função.

```lisp
> '(Pat Kim) (PAT KIM)
```

Em outras linguagens de computador, as citações geralmente vêm em pares: uma para marcar o início e outra para marcar o
fim. Em Lisp, uma aspa simples é usada para marcar o começo de uma expressão. Como sempre sabemos qual é o fim da
expressão, um final de um átomo, ou como nos parênteses correspondentes de uma lista não precisamos de um sinal de
pontuação explícito para nos informar onde a expressão termina. As citações podem ser usadas em listas, com em `'(Pat
kim)`, em símbolos como em `'Robin` e de fato em qualquer outra coisa, aqui estão alguns exemplos:

```lisp
> 'John => JOHN

> '(John Q Public) => (JOHN Q PUBLIC)

> '2 => 2

> 2 => 2

> '(+  2 2) => (+  2 2)

> (+  2 2) => 4

> John => *Error: JOHN is not a bound variable*

> (John Q Public) => *Error: JOHN is not a function*
```

Note que `'2` avalia para `2` porque é uma expressão entre aspas, e `2` avalia para `2` porque os números avaliam a si
mesmos, temos o mesmo resultado, porém, motivos diferentes. Em contraste, `'Jonh` avalia para `Jonh` porque é uma
expressão precedida de aspa simples, enquanto a avaliação `John` leva a um erro, porque avaliar um símbolo significa
obter o valor do símbolo e nenhum valor foi atribuído a `John`.

Computações simbólicas podem ser aninhadas e até misturadas com computações numéricas. A expressão a seguir cria uma
lista de nomes de uma maneira um pouco diferente do que vimos anteriormente, usando a função embutida do Common Lisp
`list`. Então, vemos como encontrar o número de elementos na lista, usando a função interna `length`:

```lisp
> (append '(Pat Kim) (list '(John Q Public) 'Sandy))
(PAT KIM (JOHN Q PUBLIC) SANDY)

> (length (append '(Pat Kim) (list '(John Q Public) 'Sandy)))
4
```

Há quatro pontos importantes a serem feitos sobre os símbolos:

* Primeiro, é importante lembrar que o Lisp não atribui nenhum significado externo aos objetos que manipula. Por
  exemplo, pensamos naturalmente em `(Robin Sandy)` como uma lista de dois primeiros nomes e `(John Q Public)` como uma
  lista do primeiro nome de uma pessoa, inicial do meio e o sobrenome. Lisp não tem tais preconceitos. Para o Lisp,
  ambos `Robin`e `xyzzy` são símbolos perfeitamente bons.

* Segundo, para fazer as computações acima, é preciso que haja já definido as funções `append`, `length`e `+`, que são
  funções já definidas por padrão no Common Lisp. Aprender uma linguagem envolve lembrar itens de vocabulário (ou saber
  onde procurá-los), bem como aprender as regras básicas para formar expressões e determinar o que elas
  significam. Common Lisp fornece mais de 700 funções embutidas. Em algum momento, o leito deve folhear um texto de
  referência para ver o que está lá, mas a maioria das funções importantes é apresentada na parte I deste livro.

* Em terceiro lugar, observe que os símbolos no Common Lisp não diferenciam maiúsculas de minúsculas. Com isso quero
  dizer que as entradas `John`, `john` e `jOhN` todos se referem ao mesmo símbolo, que normalmente é impresso como
  `JOHN`.[2](#fn01-2)

* Quarto, observe que uma grande variedade de caracteres é permitida em símbolos: números, letras e outros sinais de
  pontuação `+` ou `!`, as regras exatas para o que constitui um símbolo são um pouco complicadas, mas a convenção
  normal é usar símbolos consistindo principalmente de letras, com palavras separadas por um `-` e talvez com um número
  no final. Alguns programadores são mais liberais ao nomear variáveis e incluem caracteres como `?!$/<=>`. Por exemplo,
  uma função para converter dólares em iene (no inglês: yen, moeda usada no Japão) poderia ser nomeada com símbolo
  `$-to-yen` ou `$->yen` em Lisp, enquanto alguém usaria algo como `DollarsToYen`, `dollars_to_yen` ou `dol2yen`em
  Pascal ou C. Existem algumas exceções a essas convenções de nomenclatura, que serão tratadas à medida que surgirem.

## 1.2 Variáveis

Vimos alguns dos conceitos básicos da computação simbólica. Agora nós veremos a cateterística (talvez) mais importante 
de uma linguagem de programação: A capacidade para definir novos objetos em termos de outros, e também a capacidade de
nomear esses objetos para uso futuro. Aqui, símbolos novamente desempenham um papel importante, são usados para nomear
as variáveis. Uma variável pode ter um valor, que pode ser qualquer objeto Lisp. Uma maneira de dar um valor para uma
variável é com `setf`:

```lisp
> (setf p '(John Q Public)) => (JOHN Q PUBLIC)
> p => (JOHN Q PUBLIC)
> (setf x 10) => 10
> (+ x x) => 20
> (+ x (length p)) => 13
```
Após a atribuição do valor `'(John Q Public))` para a variável nomeada `p`, podemos referir ao valor com o nome
`p`. Semelhantemente, após a atribuição do valor `10` para a variável nomeada de `x`, podemos referir a ambos
com os nomes `x` e `p`.

Símbolos também são usados para nomear funções em Common Lisp. Cada símbolo pode ser usado como o nome de uma
variável ou uma função, ou ambos, embora seja raro (e potencialmente confuso) ter símbolos nomeando a ambos.
Um exemplo, `append` e `length` são símbolos que nomeiam funções, mas não possuem valores como variáveis, e `pi` não
nomeia uma função, mas é uma variável cujo valor é 3.1415926535897936 (ou valor aproximado).

## 1.3 Formulários especiais

0 leitor atento notará que `setf` viola a regra de avaliação. Dissemos anteriormente que funções como `+`, `-` e
`append` trabalham primeiramente avaliando todos os seus argumentos, e em seguida aplicando os resultados da avaliação
para a função. Mas `setf` não segue esta regra, porque `setf` não é uma função de verdade. Em vez de ser uma função,
`setf` é parte da sintaxe básica do Lisp. Além da sintaxe de átomos e funções, Lisp tem um pequeno número de expressões
sintáticas, que são conhecidas como formulários especiais. Servem com o mesmo propósito das declarações em outras 
linguagens de programação, e também há outras marcações sintáticas semelhantes, tais como `if` e `loop`. Há duas 
principais diferenças entre a sintaxe de Lisp e outras linguagens. Primeiro, a sintaxe de formulários Lisp são sempre 
listas "enfeitiçadas" onde o primeiro elemento é um símbolo privilegiado. `setf` é um desses símbolos, assim 
`(setf x 10)` é um formulário especial. Segundo, formulários especiais são expressões que retornam um valor, isto é 
contrastante com as declarações na maioria das linguagens, que possuem um efeito, mas não retornam um valor.

Ao avaliar uma expressão como `(setf x (+ 1 2))`, definimos a variável nomeada pelo símbolo `x` para o valor de 
`(+ 1 2)`, que é 3. Se `setf` fosse uma função normal, iria ser avaliado tanto o símbolo `x` e a expressão 
`(+ 1 2)`, para em seguida fazer alguma coisa com esses dois valores não é isso o que queremos que aconteça. 
`setf` é chamado de fomulário especial porque faz algo especial: se `setf` não existisse, seria impossível escrever uma
função que atribui um valor para uma variável. A filosofia do Lisp é prover um pequeno número de formulários especiais
para fazer coisas que seriam difíceis ou impossíveis com funções, e em seguida, espera que o usuário escreva todo o
resto que necessitar como funções. 

O termo "formulário especial" (special forms) é usado confusamente para se referir tanto para símbolos como `setf` e
expressões que começam com eles, como `(setf x 3)`. No livro *Common LISPcraft*, Wilensky resolve a ambiguidade chamando 
`setf` de função especial (special function), e reservando o termo formulário especial para `(setf x 3)`.
Esta terminologia implica que `setf` é apenas mais uma função, porém uma função especial em que o seu primeiro argumento
não é avaliado. Tal visão fazia sentido nos dias em que Lisp foi principalmente uma linguagem interpretada. A visão
moderna é que `setf` não deve ser considerado algum tipo de função anormal, mas sim um marcador de sintaxe especial que
serão tratados especialmente pelo compilador. Assim, o fomulário especial `(setf x (+ 2 1))` deve ser considerado o
equivalente na linguagem de programação C: `x = 2 + 1`. Quando há risco de confusão, chamaremos `setf` de operador de
formulário especial (special form operator) e `(setf x 3)` de expressão de formulário especial (special form expression).

A marcação de aspas é apenas uma abreviação para outro formulário especial. A expressão `'x` é equivalente a
`(quote *x*)`, uma expressão de formulário especial que é avaliada para `x`. Os operadores de formulário especial usados
neste capítulo são:

| []()            |                                              |
|-----------------|----------------------------------------------|
| `defun`         | define uma função                            |
| `defparameter`  | define uma variável especial                  |
| `setf`          | altera variáveis ou campos para novo valor           |
| `let`           | liga variáveis locais                 |
| `case`          | escolher uma das varias alternativas         |
| `if`            | fazer uma coisa ou outra, dependendo de um teste |
| `function (#')` | referir a uma função                         |
| `quote (')`     | introduzir dados constantes                    |

## 1.4 Listas

Até agora vimos duas funções que operam em listas: `append` e `length`. Uma vez que listas são importantes, vamos olhar
para mais funções de processamento de listas:

```lisp
> p => (JOHN Q PUBLIC)

> (first p) JOHN

> (rest p) => (Q PUBLIC)

> (second p) => Q

> (third p) => PUBLIC

> (fourth p) => NIL

> (length p) => 3
```

As funções `first`, `second`, `third` e `fourth` são apropriadamente nomeadas: `first` retorna o primeiro elemento da
lista, `second` retorna o segundo elemento, e assim por diante. A função `rest` não é tão óbvia; o seu nome significa "o
resto da lista após o primeiro elemento". O símbolo `nil` e o formulário `()` são completamente sinônimos; ambos são
representações de lista vazia. `nil` é também usado para denotar o valor "false" em Lisp. Assim, acima `(fourth p)` é
`nil` porque não há um quarto elemento de `p`. Note que listas não precisam ser compostas apenas de átomos, mas podem
conter sublistas como elementos.

```lisp
> (setf x '((1st element) 2 (element 3) ((4)) 5))
((1ST ELEMENT) 2 (ELEMENT 3) ((4)) 5)

> (length x) => 5

> (first x) => (1ST ELEMENT)

> (second x) => 2

> (third x) => (ELEMENT 3)

> (fourth x) => ((4))

> (first (fourth x)) => (4)

> (first (first (fourth x))) => 4

> (fifth x) => 5

> (first x) => (1ST ELEMENT)

> (second (first x)) => ELEMENT
```

Até agora temos visto como acessar partes de listas. Também é possível construir novas listas, veja os exemplos abaixo:

```lisp
> p => (JOHN Q PUBLIC)

> (cons 'Mr p) => (MR JOHN Q PUBLIC)

> (cons (first p) (rest p)) => (JOHN Q PUBLIC)

> (setf town (list 'Anytown 'USA)) => (ANYTOWN USA)

> (list p 'of town 'may 'have 'already 'won!) =>
((JOHN Q PUBLIC) OF (ANYTOWN USA) MAY HAVE ALREADY WON!)

> (append p '(of) town '(may have already won!)) =>
(JOHN Q PUBLIC OF ANYTOWN USA MAY HAVE ALREADY WON!)

> p => (JOHN Q PUBLIC)
```
A função `cons` significa construct (construir).
<a id="tfn01-3"></a>
Ela pede como argumentos um elemento e uma lista[3](#fn01-3), então constrói uma nova lista cujo `first` é o elemento e
o `rest` é a lista original. `list` leva qualquer número de elementos como argumentos e retornam uma nova lista contendo
os elementos. Nós já vimos `append`, que é semelhante a `list`, qual pede como argumentos qualquer número de listas,
unindo todas, formando uma grande lista. Assim, os argumentos de `append` devem ser listas, enquanto os argumentos de
`list` podem ser listas ou átomos. É importante notar que essas funções criam novas listas, elas não modificam as
antigas. Quando dizemos `(append p q)`, o efeito é criar uma nova lista que começa com os mesmos elementos que estavam
em `p`, contudo `p` permanece inalterada.

Agora vamos nos afastar de funções para listas, e considerar um simples problema: dado um nome de uma pessoa sob a forma
de uma lista, como poderíamos extrair o nome da família? Para `(JOHN Q PUBLIC)` nós poderíamos usar a função `third`,
mas não funcionaria para alguém sem nome do meio. Há uma função chamada `last` em Common Lisp; talvez pode funcionar,
vamos experimentar:

```lisp
> (last p) => (PUBLIC)

> (first (last p)) => PUBLIC
```

<a id="tfn01-4"></a>
Acontece que `last` perversamente retorna uma lista com o último elemento, em vez do último elemento em si.[4](#fn01-4) 
Assim nós precisamos combinar `first` e `last` para obter o último elemento realmente. Gostaríamos de ser capaz de
salvar o trabalho que fizemos, e dar-lhe uma boa descrição, chamando-o como `last-name`. Nós poderíamos usar `setf` para
salvar o último nome de `p `, mas isso não nos ajudaria obter qualquer outro último nome. Ao invés disso, queremos
definir uma nova função que calcula o último nome de qualquer nome que é representado como uma lista. Na próxima seção
veremos exatamente isso.

## 1.5 Definindo novas funções

O formulário especial `defun` serve para "definir funções". 
Abaixo é usado para definir uma nova função chamada `last-name`:

```lisp
(defun last-name (name)
  "Select the last name from a name represented as a list."
  (first (last name)))
```

Nós damos para a nova função o nome `last-name`. Tem uma lista de parâmetros consistindo de um único parâmetro:
`name`. Isso significa que a função leva um argumento. O qual nos referimos como `name`. Também temos uma cadeia 
de caracteres de documentação (documentation string) que informa o que a função faz; isso não é usado em qualquer
computação, mas as String de documentação são ferramentas cruciais para depuração e compreensão de sistemas grandes.
O corpo da definição é `(first (last name))`, qual é o que usamos para obter o último nome de `p`. A diferença é que
aqui nós queremos obter o último nome de qualquer nome passado como o argumento `name`, não apenas do nome específico `p`.

Em geral, a definição da função leva o seguinte formulário (onde a "documentation string" é opcional, e todas as outras
partes são requeridas:

```lisp
(defun *function-name* (*parameter...*)
      "*documentation string*"
      *function-body...*)
```

O nome da função deve ser um símbolo, os parâmetros são geralmente símbolos (com algumas complicações para serem
explicadas mais tarde), e o corpo da função consiste de uma ou mais expressões que serão avaliadas quando a função é
chamada. A última expressão é retornada como o valor da chamada da função. 

Umas vez que definimos `last-name`, podemos usá-lo como qualquer outra função Lisp: 

```lisp
> (last-name p) => PUBLIC

> (last-name '(Rear Admiral Grace Murray Hopper)) => HOPPER

> (last-name '(Rex Morgan MD)) => MD

> (last-name '(Spot)) => SPOT

> (last-name '(Aristotle)) => ARISTOTLE
```

Os últimos três exemplos apontam uma limitação inerente da empresa de programação. 
Quando nos dizemos `(defun last-name...)` nós não estamos realmente definindo o que significa para a pessoa ter um
último nome; estamos apenas definindo uma operação em uma representação de nomes em termos de listas. 

Nossas intuições dizem que "MD" é um título, "Spot" é um primeiro nome de um cachorro, e "Aristotle" vivia antes do
conceito de sobrenome ter sido inventado. Nós não estamos representando esses casos em nossa operação. No entanto, 
podemos sempre mudar a definição de `last-name` para incorporar esses casos problemáticos.

Nós podemos também definir a função `first-name`.
Mesmo que a definição seja trivial (isto é o mesmo que a função `first`), é ainda boa prática definir `first-name`
explicitamente. Então nós podemos usar a função `first-name` quando estivermos lidando com nomes, e `first` quando
estivermos lidando com listas arbitrárias. O computador irá executar a mesma operação em cada caso, mas nós como
programadores (e leitores de programas) buscamos ser o menos confuso possível. Outra vantagem de definir uma função
específica como `first-name` é que se decidirmos mudar a representação dos nomes só precisaremos mudar a definição
de `first-name`. É uma tarefa muito mais fácil do que ficar caçando um grande programa e alterar cada pedaço de código onde
estamos usando `first` para referir a nomes, deixando outros usos de `first` em paz.

```lisp 
(defun first-name (name)
  "Select the first name from a name represented as a list."
  (first name))

> p => (JOHN Q PUBLIC)`

> (first-name p) => JOHN`

> (first-name '(Wilma Flintstone)) => WILMA`

> (setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot) 
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet))) => 

((JOHN Q PUBLIC) (MALCOLM X) (ADMIRAL GRACE MURRAY HOPPER)
 (SPOT) (ARISTOTLE) (A A MILNE) (Z Z TOP) (SIR LARRY OLIVIER)
 (MISS SCARLET))

> (first-name (first names)) => JOHN
```

Nesta última expressão nós usamos a função `first` para pegar o primeiro elemento de uma lista de nomes, e então 
usamos a função `first-name` para pegar o primeiro nome desse elemento. Poderíamos também ter dito 
`(first (first names))` ou mesmo `(first (first-name names))` e ainda conseguiríamos `JOHN`, mas não estaríamos
representando com exatidão o que está sendo considerado um nome ou o que está sendo considerado uma lista de nomes.

## 1.6 Usando funções

One good thing about defining a list of names, as we did above, is that it makes it easier to test our functions.
Consider the following expression, which can be used to test the `last-name` function:

```lisp
> (mapcar #'last-name names)
(PUBLIC X HOPPER SPOT ARISTOTLE MILNE TOP OLIVIER SCARLET)
```

The funny `#'` notation maps from the name of a function to the function itself.
This is analogous to `'x` notation.
The built-in function `mapcar` is passed two arguments, a function and a list.
It returns a list built by calling the function on every element of the input list.
In other words, the `mapcar` call above is equivalent to:

```lisp
(list (last-name (first names))
      (last-name (second names))
      (last-name (third names))
```

`mapcar`'s name comes from the fact that it "maps" the function across each of the arguments.
The `car` part of the name refers to the Lisp function `car`, an old name for `first`.
`cdr` is the old name for `rest`.
The names stand for "contents of the address register" and "contents of the decrement register," the instructions that were used in the first implementation of Lisp on the IBM 704.
I'm sure you'll agree that `first` and `rest` are much better names, and they will be used instead of `car` and `cdr` whenever we are talking about lists.
However, we will continue to use `car` and `cdr` on occasion when we are considering a pair of values that are not considered as a list.
Beware that some programmers still use `car` and `cdr` for lists as well.

Here are some more examples of `mapcar`:

```lisp
> (mapcar #'- '(1 2 3 4)) => (-1 -2 -3 -4)

> (mapcar #'+ '(1 2 3 4) '(10 20 30 40)) => (11 22 33 44)
```

This last example shows that `mapcar` can be passed three arguments, in which case the first argument should be a binary function, which will be applied to corresponding elements of the other two lists.
In general, `mapcar` expects an *n*-ary function as its first argument, followed by *n* lists.
It first applies the function to the argument list obtained by collecting the first element of each list.
Then it applies the function to the second element of each list, and so on, until one of the lists is exhausted.
It returns a list of all the function values it has computed.

Now that we understand `mapcar`, let's use it to test the `first-name` function:

```lisp
> (mapcar #'first-name names)
(JOHN MALCOLM ADMIRAL SPOT ARISTOTLE A Z SIR MISS)
```

We might be disappointed with these results.
Suppose we wanted a version of `first-name` which ignored titles like Admiral and Miss, and got to the "real" first name.
We could proceed as follows:

```lisp
(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")
```

We've introduced another new special form, `defparameter`, which defines a parameter-a variable that does not change over the course of a computation, but that might change when we think of new things to add (like the French Mme or the military Lt.).
The `defparameter` form both gives a value to the variable and makes it possible to use the variable in subsequent function definitions.
In this example we have exercised the option of providing a documentation string that describes the variable.
It is a widely used convention among Lisp programmers to mark special variables by spelling their names with asterisks on either end.
This is just a convention; in Lisp, the asterisk is just another character that has no particular meaning.

<a id="tfn01-5"></a>
We next give a new definition for `first-name`, which supersedes the previous definition.[5](#fn01-5)
This definition says that if the first word of the name is a member of the list of titles, then we want to ignore that word and return the `first-name` of the rest of the words in the name.
Otherwise, we use the first word, just as before.
Another built-in function, `member`, tests to see if its first argument is an element of the list passed as the second argument.

The special form `if` has the form `(if *test then-part else-part*)`.
There are many special forms for performing conditional tests in Lisp; `if` is the most appropriate for this example.
An `if` form is evaluated by first evaluating the *test* expression.
If it is true, the *then-part* is evaluated and returned as the value of the `if` form; otherwise the *else-part* is evaluated and returned.
While some languages insist that the value of a conditional test must be either `true` or `false`, Lisp is much more forgiving.
The test may legally evaluate to any value at all.
Only the value `nil` is considered false; all other values are considered true.
*(ed. note: is `false` considered `true`?)*
In the definition of `first-name` below, the function `member` will return a non-nil (hence true) value if the first element of the name is in the list of titles, and will return `nil` (hence false) if it is not.
Although all non-nil values are considered true, by convention the constant `t` is usually used to represent truth.

```lisp
(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))
```

When we map the new `first-name` over the list of names, the results are more encouraging.
In addition, the function gets the "right" result for `'(Madam Major General Paula Jones)` by dropping off titles one at a time.

```lisp
> (mapcar #'first-name names)
(JOHN MALCOLM GRACE SPOT ARISTOTLE A Z LARRY SCARLET)

> (first-name '(Madam Major General Paula Jones))
PAULA
```

We can see how this works by *tracing* the execution of `first-name`, and seeing the values passed to and returned from the function.
The special forms `trace` and `untrace` are used for this purpose.

```lisp
> (trace first-name)
(FIRST-NAME)

> (first-name '(John Q Public))
(1 ENTER FIRST-NAME: (JOHN Q PUBLIC))
(1 EXIT FIRST-NAME: JOHN)
JOHN
```

When `first-name` is called, the definition is entered with the single argument, `name,` taking on the value `(JOHN Q PUBLIC)`.
The value returned is `JOHN`.
Trace prints two lines indicating entry and exit from the function, and then Lisp, as usual, prints the final result, `JOHN`.

The next example is more complicated.
The function `first-name` is used four times.
First, it is entered with `name` bound to `(Madam Major General Paula Jones).`
The first element of this list is `Madam,` and since this is a member of the list of titles, the result is computed by calling `first-name` again on the rest of the name-`(Major General Paula Jones).`
This process repeats two more times, and we finally enter `first-name` with name bound to (`Paula Jones`).
Since `Paula` is not a title, it becomes the result of this call to `first-name,` and thus the result of all four calls, as trace shows.
Once we are happy with the workings of `first-name,` the special form `untrace` turns off tracing.

```lisp
> (first-name '(Madam Major General Paula Jones)) =>
(1 ENTER FIRST-NAME: (MADAM MAJOR GENERAL PAULA JONES))
  (2 ENTER FIRST-NAME: (MAJOR GENERAL PAULA JONES))
    (3 ENTER FIRST-NAME: (GENERAL PAULA JONES))
      (4 ENTER FIRST-NAME: (PAULA JONES))
      (4 EXIT FIRST-NAME: PAULA)
    (3 EXIT FIRST-NAME: PAULA)
  (2 EXIT FIRST-NAME: PAULA)
(1 EXIT FIRST-NAME: PAULA)
PAULA

> (untrace first-name) => (FIRST-NAME)

> (first-name '(Mr Blue Jeans)) => BLUE
```

The function `first-name` is said to be *recursive* because its definition includes a call to itself.
Programmers who are new to the concept of recursion sometimes find it mysterious.
But recursive functions are really no different from nonrecursive ones.
Any function is required to return the correct value for the given input(s).
Another way to look at this requirement is to break it into two parts: a function must return a value, and it must not return any incorrect values.
This two-part requirement is equivalent to the first one, but it makes it easier to think about and design function definitions.

Next I show an abstract description of the `first-name` problem, to emphasize the design of the function and the fact that recursive solutions are not tied to Lisp in any way:

```lisp
function first-name(name):
  if *the first element of name is a title*
    then *do something complicated to get the first-name*
    else *return the first element of the name*
```

This breaks up the problem into two cases.
In the second case, we return an answer, and it is in fact the correct answer.
We have not yet specified what to do in the first case.
But we do know that it has something to do with the rest of the name after the first element, and that what we want is to extract the first name out of those elements.
The leap of faith is to go ahead and use `first-name`, even though it has not been fully defined yet:

```lisp
function first-name(name):
  if *the first element of name is a title*
    then *return the* first-name *of the rest of the name*
    else *return the first element of the name*
```

Now the first case in `first-name` is recursive, and the second case remains unchanged.
We already agreed that the second case returns the correct answer, and the first case only returns what `first-name` returns.
So `first-name` as a whole can only return correct answers.
Thus, we're halfway to showing that the function is correct; the other half is to show that it eventually returns some answer.
But every recursive call chops off the first element and looks at the rest, so for an *n*-element list there can be at most *n* recursive calls.
This completes the demonstration that the function is correct.
Programmers who learn to think this way find recursion to be a valuable tool rather than a confusing mystery.

## 1.7 Higher-Order Functions

Functions in Lisp can not only be "called," or applied to arguments, they can also be manipulated just like any other kind of object.
A function that takes another function as an argument is called a *higher-order function.*
`mapcar` is an example.
To demonstrate the higher-order-function style of programming, we will define a new function called `mappend.` It takes two arguments, a function and a list.
`mappend` maps the function over each element of the list and appends together all the results.
The first definition follows immediately from the description and the fact that the function `apply` can be used to apply a function to a list of arguments.

```lisp
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))
```

Now we experiment a little to see how `apply` and `mappend` work.
The first example applies the addition function to a list of four numbers.

```lisp
> (apply #'+ '(1 2 3 4)) => 10
```

The next example applies append to a list of two arguments, where each argument is a list.
If the arguments were not lists, it would be an error.

```lisp
> (apply #'append '((1 2 3) (a b c))) => (1 2 3 A B C)
```

Now we define a new function, `self-and-double`, and apply it to a variety of arguments.

```lisp
> (defun self-and-double (x) (list x (+ x x)))

> (self-and-double 3) => (3 6)

> (apply #'self-and-double '(3)) => (3 6)
```

If we had tried to apply `self-and-double` to a list of more than one argument, or to a list that did not contain a number, it would be an error, just as it would be an error to evaluate (`self-and-double 3 4`) or (`self-and-double 'Kim`).
Now let's return to the mapping functions:

```lisp
> (mapcar #'self-and-double '(1 10 300)) => ((1 2) (10 20) (300 600))

> (mappend #'self-and-double '(1 10 300)) => (1 2 10 20 300 600)
```

When `mapcar` is passed a function and a list of three arguments, it always returns a list of three values.
Each value is the result of calling the function on the respective argument.
In contrast, when `mappend` is called, it returns one big list, which is equal to all the values that `mapcar` would generate appended together.
It would be an error to call `mappend` with a function that didn't return lists, because `append` expects to see lists as its arguments.

Now consider the following problem: given a list of elements, return a list consisting of all the numbers in the original list and the negation of those numbers.
For example, given the list (`testing 1 2 3 test`), return (`1 -1 2 -2 3 -3`).
This problem can be solved very easily using `mappend` as a component:

```lisp
(defun numbers-and-negations (input)
  "Given a list, return only the numbers and their negations."
  (mappend #'number-and-negation input))

(defun number-and-negation (x)
  "If x is a number, return a list of x and -x."
  (if (numberp x)
      (list x (- x))
      nil))

> (numbers-and-negations '(testing 1 2 3 test)) => (1 -1 2 -2 3 -3)
```

The alternate definition of `mappend` shown in the following doesn't make use of `mapcar;` instead it builds up the list one element at a time:

```lisp
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))
```

`funcall` is similar to `apply;` it too takes a function as its first argument and applies the function to a list of arguments, but in the case of `funcall`, the arguments are listed separately:

```lisp
> (funcall #'+ 2 3) => 5

> (apply #' + '(2 3)) => 5

> (funcall #' + '(2 3)) => *Error: (2 3) is not a number.*
```

These are equivalent to `(+  2 3), (+ 2 3)`,and`(+ '(2 3))`, respectively.

So far, every function we have used has been either predefined in Common Lisp or introduced with a `defun`, which pairs a function with a name.
It is also possible to introduce a function without giving it a name, using the special syntax `lambda`.

The name *lambda* comes from the mathematician Alonzo Church's notation for functions (Church 1941).
Lisp usually prefers expressive names over terse Greek letters, but lambda is an exception.
A better name would be `make-function`.
Lambda derives from the notation in Russell and Whitehead's *Principia Mathematica,* which used a caret over bound variables: *x&#x302;*(*x + x*).

Church wanted a one-dimensional string, so he moved the caret in front: *^x*(*x + x*).
The caret looked funny with nothing below it, so Church switched to the closest thing, an uppercase lambda, *&Lambda;x*(*x + x*).
The &Lambda; was easily confused with other symbols, so eventually the lowercase lambda was substituted: *&lambda;x*(*x + x*).
John McCarthy was a student of Church's at Princeton, so when McCarthy invented Lisp in 1958, he adopted the lambda notation.
There were no Greek letters on the keypunches of that era, so McCarthy used (`lambda (x) (+ x x)`), and it has survived to this day.
In general, the form of a lambda expression is

```lisp
(lambda (*parameters...*) *body...*)
```

A lambda expression is just a nonatomic *name* for a function, just as `append` is an atomic name for a built-in function.
As such, it is appropriate for use in the first position of a function call, but if we want to get at the actual function, rather than its name, we still have to use the `#'` notation.
For example:

```lisp
> ((lambda (x) (+ x 2)) 4) => 6

> (funcall #'(lambda (x) (+ x 2)) 4) => 6
```

To understand the distinction we have to be clear on how expressions are evaluated in Lisp.
The normal rule for evaluation states that symbols are evaluated by looking up the value of the variable that the symbol refers to.
So the `x` in `(+ x 2)` is evaluated by looking up the value of the variable named `x`.
A list is evaluated in one of two ways.
If the first element of the list is a special form operator, then the list is evaluated according to the syntax rule for that special form.
Otherwise, the list represents a function call.
The first element is evaluated in a unique way, as a function.
This means it can either be a symbol or a lambda expression.
In either case, the function named by the first element is applied to the values of the remaining elements in the list.
These values are determined by the normal evaluation rules.
If we want to refer to a function in a position other than the first element of a function call, we have to use the `#'` notation.
Otherwise, the expressions will be evaluated by the normal evaluation rule, and will not be treated as functions.
For example:

```lisp
> append => *Error: APPEND is not a bound variable*

> (lambda (x) (+ x 2)) => *Error: LAMBDA is not a function*
```

Here are some more examples of the correct use of functions:

```lisp
>(mapcar #'(lambda (x) (+ x x))
         '(1 2 3 4 5)) =>
(2 4 6 8 10)

> (mappend #'(lambda (l) (list l (reverse l)))
           ((1 2 3) (a b c))) =>
((1 2 3) (3 2 1) (A B C) (C B A))
```

Programmers who are used to other languages sometimes fail to see the point of lambda expressions.
There are two reasons why lambda expressions are very useful.

First, it can be messy to clutter up a program with superfluous names.
Just as it is clearer to write `(a+b)*(c+d)` rather than to invent variable names like `temp1` and `temp2` to hold `a+b` and `c+d`, so it can be clearer to define a function as a lambda expression rather than inventing a name for it.

Second, and more importantly, lambda expressions make it possible to create new functions at run time.
This is a powerful technique that is not possible in most programming languages.
These run-time functions, known as *closures,* will be covered in section 3.16.

## 1.8 Other Data Types

So far we have seen just four kinds of Lisp objects: numbers, symbols, lists, and functions.
Lisp actually defines about 25 different types of objects: vectors, arrays, structures, characters, streams, hash tables, and others.
At this point we will introduce one more, the string.
As you can see in the following, strings, like numbers, evaluate to themselves.
Strings are used mainly for printing out messages, while symbols are used for their relationships to other objects, and to name variables.
The printed representation of a string has a double quote mark `(")` at each end.

```lisp
> "a string" => "a string"

> (length "a string") => 8

> (length "") => 0
```

## 1.9 Summary: The Lisp Evaluation Rule

We can now summarize the evaluation rule for Lisp.

*   Every expression is either a *list* or an *atom.*

*   Every list to be evaluated is either a *special form expression* or a *function application*.

*   A *special form expression* is defined to be a list whose first element is a special form operator.
The expression is evaluated according to the operator's idiosyncratic evaluation rule.
For example, the evaluation rule for `setf` is to evaluate the second argument according to the normal evaluation rule, set the first argument to that value, and return the value as the result.
The rule for `defun` is to define a new function, and return the name of the function.
The rule for quote is to return the first argument unevaluated.
The notation `'x` is actually an abbreviation for the special form expression `(quote x)`.
Similarly, the notation `#'f` is an abbreviation for the special form expression `(function f)`.

```lisp
'John = (quote John) => JOHN

(setf p 'John) => JOHN

defun twice (x) (+ x x)) => TWICE

(if (=  2 3) (error) (+  5 6)) => 11
```

*   A *function application* is evaluated by first evaluating the arguments (the rest of the list) and then finding the function named by the first element of the list and applying it to the list of evaluated arguments.

```lisp
(+  2 3) => 5
(- (+  90 9) (+  50 5 (length '(Pat Kim)))) => 42
```

Note that if `'(Pat Kim)` did not have the quote, it would betreated as a function application of the function `pat` to the value of the variable `kim.`

*   Every atom is either a *symbol* or a *nonsymbol.*

*   A *symbol* evaluates to the most recent value that has been assigned to the variable named by that symbol.
Symbols are composed of letters, and possibly digits and, rarely, punctuation characters.
<a id="tfn01-6"></a>
To avoid confusion, we will use symbols composed mostly of the letters `a-z` and the `'-'` character, with a few exceptions.[6](#fn01-6)

```lisp
names
p
*print-pretty*
```

*   A *nonsymbol atom* evaluates to itself.
For now, numbers and strings are the only such non-symbol atoms we know of.
Numbers are composed of digits, and possibly a decimal point and sign.
There are also provisions for scientific notation, rational and complex numbers, and numbers with different bases, but we won't describe the details here.
Strings are delimited by double quote marks on both sides.

```lisp
42 => 42
-273.15 => -273.15
"a string" => "a string"
```

There are some minor details of Common Lisp that complicate the evaluation rules, but this definition will suffice for now.

One complication that causes confusion for beginning Lispers is the difference between *reading* and *evaluating* an expression.
Beginners often imagine that when they type an expression, such as

```lisp
> (+ (* 3 4) (* 5 6))
```

the Lisp system first reads the (`+`, then fetches the addition function, then reads `(* 3 4)` and computes `12`, then reads `(* 5 6)` and computes 30, and finally computes 42.
In fact, what actually happens is that the system first reads the entire expression, the list `(+ (* 3 4) (* 5 6))`.
Only after it has been read does the system begin to evaluate it.
This evaluation can be done by an interpreter that looks at the list directly, or it can be done by a compiler that translates the list into machine language instructions and then executes those instructions.

We can see now that it was a little imprecise to say, "Numbers are composed of digits, and possibly a decimal point and sign." It would be more precise to say that the printed representation of a number, as expected by the function read and as produced by the function print, is composed of digits, and possibly a decimal point and sign.
The internal representation of a number varies from one computer to another, but you can be sure that it will be a bit pattern in a particular memory location, and it will no longer contain the original characters used to represent the number in decimal notation.
Similarly, it is the printed representation of a string that is surrounded by double quote marks; the internal representation is a memory location marking the beginning of a vector of characters.

Beginners who fail to grasp the distinction between reading and evaluating may have a good model of what expressions evaluate to, but they usually have a terrible model of the efficiency of evaluating expressions.
One student used only one-letter variable names, because he felt that it would be faster for the computer to look up a one-letter name than a multiletter name.
While it may be true that shorter names can save a microsecond at read time, this makes no difference at all at evaluation time.
Every variable, regardless of its name, is just a memory location, and the time to access the location does not depend on the name of the variable.

## 1.10 What Makes Lisp Different?

What is it that sets Lisp apart from other languages?
Why is it a good language for AI applications?
There are at least eight important factors:

*   Built-in Support for Lists
*   Automatic Storage Management
*   Dynamic Typing
*   First-Class Functions
*   Uniform Syntax
*   Interactive Environment
*   Extensibility
*   History

In sum, these factors allow a programmer to delay making decisions.
In the example dealing with names, we were able to use the built-in list functions to construct and manipulate names without making a lot of explicit decisions about their representation.
If we decided to change the representation, it would be easy to go back and alter parts of the program, leaving other parts unchanged.

This ability to delay decisions-or more accurately, to make temporary, nonbinding decisions-is usually a good thing, because it means that irrelevant details can be ignored.
There are also some negative points of delaying decisions.
First, the less we tell the compiler, the greater the chance that it may have to produce inefficient code.
Second, the less we tell the compiler, the less chance it has of noticing inconsistencies and warning us.
Errors may not be detected until the program is run.
Let's consider each factor in more depth, weighing the advantages and disadvantages:

*   *Built-in Support for Lists.*
The list is a very versatile data structure, and while lists can be implemented in any language, Lisp makes it easy to use them.
Many AI applications involve lists of constantly changing size, making fixed-length data structures like vectors harder to use.
Early versions of Lisp used lists as their only aggregate data structure.
Common Lisp provides other types as well, because lists are not always the most efficient choice.

*   *Automatic Storage Management.*
The Lisp programmer needn't keep track of memory allocation; it is all done automatically.
This frees the programmer of a lot of effort, and makes it easy to use the functional style of programming.
Other languages present programmers with a choice.
Variables can be allocated on the stack, meaning that they are created when a procedure is entered, and disappear when the procedure is done.
This is an efficient use of storage, but it rules out functions that return complex values.
The other choice is for the programmer to explicitly allocate and free storage.
This makes the functional style possible but can lead to errors.

For example, consider the trivial problem of computing the expression *a* x (b + c), where *a*, *b*, and *c* are numbers.
The code is trivial in any language; here it is in Pascal and in Lisp:

| []()           |                 |
|----------------|-----------------|
| `/* Pascal */` | `;;; Lisp`      |
| `a * (b + c)`  | `(* a (+ b c))` |

The only difference is that Pascal uses infix notation and Lisp uses prefix.
Now consider computing *a* x (b + c) when *a*, *b*, and *c* are matrices.
Assume we have procedures for matrix multiplication and addition.
In Lisp the form is exactly the same; only the names of the functions are changed.
In Pascal we have the choice of approaches mentioned before.
We could declare temporary variables to hold intermediate results on the stack, and replace the functional expression with a series of procedure calls:

| []()                        |                      |
|-----------------------------|----------------------|
| `/* Pascal */`              | `;;; Lisp`           |
| `var temp, result: matrix;` |                      |
| `add(b,c,temp);`            | `(mult a (add b c))` |
| `mult(a,temp,result);`      |                      |
| `return(result);`           |                      |

The other choice is to write Pascal functions that allocate new matrices on the heap.
Then one can write nice functional expressions like `mult(a,add(b,c))` even in Pascal.
However, in practice it rarely works this nicely, because of the need to manage storage explicitly:

| []()                     |                      |
|--------------------------|----------------------|
| `/* Pascal */`           | `;;; Lisp`           |
| `var a,b,c,x,y: matrix;` |                      |
| `x := add(b.c);`         | `(mult a (add b c))` |
| `y := mult(a,x);`        |                      |
| `free(x);`               |                      |
| `return(y);`             |                      |

In general, deciding which structures to free is a difficult task for the Pascal programmer.
If the programmer misses some, then the program may run out of memory.
Worse, if the programmer frees a structure that is still being used, then strange errors can occur when that piece of memory is reallocated.
Lisp automatically allocates and frees structures, so these two types of errors can *never* occur.

*   *Dynamic Typing.*
Lisp programmers don't have to provide type declarations, because the language keeps track of the type of each object at run time, rather than figuring out all types at compile time.
This makes Lisp programs shorter and hence faster to develop, and it also means that functions can often be extended to work for objects to which they were not originally intended to apply.
In Pascal, we can write a procedure to sort an array of 100 integers, but we can't use that same procedure to sort 200 integers, or 100 strings.
In Lisp, one `sort` fits all.
One way to appreciate this kind of flexibility is to see how hard it is to achieve in other languages.
It is impossible in Pascal; in fact, the language Modula was invented primarily to fix this problem in Pascal.
The language Ada was designed to allow flexible generic functions, and a book by Musser and Stepanov (1989) describes an Ada package that gives some of the functionality of Common Lisp's sequence functions.
But the Ada solution is less than ideal: it takes a 264-page book to duplicate only part of the functionality of the 20-page chapter 14 from Steele (1990), and Musser and Stepanov went through five Ada compilers before they found one that would correctly compile their package.
Also, their package is considerably less powerful, since it does not handle vectors or optional keyword parameters.
In Common Lisp, all this functionality comes for free, and it is easy to add more.
On the other hand, dynamic typing means that some errors will go undetected until run time.
The great advantage of strongly typed languages is that they are able to give error messages at compile time.
The great frustration with strongly typed languages is that they are only able to warn about a small class of errors.
They can tell you that you are mistakenly passing a string to a function that expects an integer, but they can't tell you that you are passing an odd number to a function that expects an even number.

*   *First-Class Functions.*
A *first-class* object is one that can be used anywhere and can be manipulated in the same ways as any other kind of object.
In Pascal or C, for example, functions can be passed as arguments to other functions, but they are not first-class, because it is not possible to create new functions while the program is running, nor is it possible to create an anonymous function without giving it a name.
In Lisp we can do both those things using `lambda`.
This is explained in section 3.16, page 92.

*   *Uniform Syntax.*
The syntax of Lisp programs is simple.
This makes the language easy to learn, and very little time is wasted correcting typos.
In addition, it is easy to write programs that manipulate other programs or define whole new languages-a very powerful technique.
The simple syntax also makes it easy for text editing programs to parse Lisp.
Your editor program should be able to indent expressions automatically and to show matching parentheses.
This is harder to do for languages with complex syntax.
On the other hand, some people object to all the parentheses.
There are two answers to this objection.
First, consider the alternative: in a language with "conventional" syntax, Lisp's parentheses pairs would be replaced either by an implicit operator precedence rule (in the case of arithmetic and logical expressions) or by a `begin/end` pair (in the case of control structures).
But neither of these is necessarily an advantage.
Implicit precedence is notoriously error-prone, and `begin/end` pairs clutter up the page without adding any content.
Many languages are moving away from `begin/end: C` uses { and }, which are equivalent to parentheses, and several modern functional languages (such as Haskell) use horizontal blank space, with no explicit grouping at all.
Second, many Lisp programmers *have* considered the alternative.
There have been a number of preprocessors that translate from "conventional" syntax into Lisp.
None of these has caught on.
It is not that Lisp programmers find it *tolerable* to use all those parentheses, rather, they find it *advantageous.*
With a little experience, you may too.
It is also important that the syntax of Lisp data is the same as the syntax of programs.
Obviously, this makes it easy to convert data to program.
Less obvious is the time saved by having universal functions to handle input and output.
The Lisp functions `read` and `print` will automatically handle any list, structure, string, or number.
This makes it trivial to test individual functions while developing your program.
In a traditional language like C or Pascal, you would have to write special-purpose functions to read and print each data type you wanted to debug, as well as a special-purpose driver to call the routines.
Because this is time-consuming and error-prone, the temptation is to avoid testing altogether.
Thus, Lisp encourages better-tested programs, and makes it easier to develop them faster.

*   *Interactive Environment.*
Traditionally, a programmer would write a complete program, compile it, correct any errors detected by the compiler, and then run and debug it.
This is known as the *batch* mode of interaction.
For long programs, waiting for the compiler occupied a large portion of the debugging time.
In Lisp one normally writes a few small functions at a time, getting feedback from the Lisp system after evaluating each one.
This is known as an *interactive* environment.
When it comes time to make a change, only the changed functions need to be recompiled, so the wait is much shorter.
In addition, the Lisp programmer can debug by typing in arbitrary expressions at any time.
This is a big improvement over editing the program to introduce print statements and recompiling.
Notice that the distinction between *interactive* and a *batch* languages is separate from the distinction between *interpreted* and *compiled* languages.
It has often been stated, incorrectly, that Lisp has an advantage by virtue of being an interpreted language.
Actually, experienced Common Lisp programmers tend to use the compiler almost exclusively.
The important point is interaction, not interpretation.
The idea of an interactive environment is such a good one that even traditional languages like C and Pascal are starting to offer interactive versions, so this is not an exclusive advantage of Lisp.
However, Lisp still provides much better access to the interactive features.
A C interpreter may allow the programmer to type in an expression and have it evaluated immediately, but it will not allow the programmer to write a program that, say, goes through the symbol table and finds all the user-defined functions and prints information on them.
In C-even interpreted C-the symbol table is just a Cheshire-cat-like invention of the interpreter's imagination that disappears when the program is run.
<a id="tfn01-7"></a>
In Lisp, the symbol table is a first-class object[7](#fn01-7) that can be accessed and modified with functions like `read, intern` and `do-symbols`.
Common Lisp offers an unusually rich set of useful tools, including over 700 built-in functions (ANSI Common Lisp has over 900).
Thus, writing a new program involves more gathering of existing pieces of code and less writing of new code from scratch.
In addition to the standard functions, Common Lisp implementations usually provide extensions for interacting with the editor, debugger, and window system.

*   *Extensibility*.
When Lisp was invented in 1958, nobody could have foreseen the advances in programming theory and language design that have taken place in the last thirty years.
Other early languages have been discarded, replaced by ones based on newer ideas.
However, Lisp has been able to survive, because it has been able to adapt.
Because Lisp is extensible, it has been changed to incorporate the newest features as they become popular.
The easiest way to extend the language is with macros.
When so-called structured programming constructs such as *case* and *if-then-else* arose, they were incorporated into Lisp as macros.
But the flexibility of Lisp goes beyond adding individual constructs.
Brand new styles of programming can easily be implemented.
Many AI applications are based on the idea of *rule-based* programming.
<a id="tfn01-8"></a>
Another new style is *object-oriented* programming, which has been incorporated with the Common Lisp Object System (CLOS),[8](#fn01-8) a set of macros, functions, and data types that have been integrated into ANSI Common Lisp.

To show how far Lisp has come, here's the only sample program given in the *Lisp/MTS Programmer's Guide* (Hafner and Wilcox 1974):

```lisp
(PROG (LIST DEPTH TEMP RESTLIST)
(SETQ RESTLIST (LIST (CONS (READ) O)))
A (COND
((NOT RESTLIST) (RETURN 'DONE))
(T (SETQ LIST (UNCONS (UNCONS RESTLIST
     RESTLIST) DEPTH))
(COND ((ATOM LIST)
(MAPC 'PRIN1 (LIST '"ATOM:" LIST '"," 'DEPTH DEPTH))
(TERPRI))
(T (SETQ TEMP (UNCONS LIST LIST))
(COND (LIST
(SETQ RESTLIST (CONS(CONS LIST DEPTH) RESTLIST))))
(SETQ RESTLIST (CONS (CONS TEMP
     (ADD1 DEPTH)) RESTLIST))
))))
))))(GO A))
```

Note the use of the now-deprecated goto `(GO)` statement, and the lack of consistent indentation conventions.
The manual also gives a recursive version of the same program:

```lisp
(PROG NIL (
(LABEL ATOMPRINT (LAMBDA (RESTLIST)
(COND ((NOT RESTLIST) (RETURN 'DONE))
((ATOM (CAAR RESTLIST)) (MAPC 'PRIN1
     (LIST '"ATOM:" (CAAR RESTLIST)
          '"," 'DEPTH (CDAR RESTLIST)))
(TERPRI)
(ATOMPRINT (CDR RESTLIST)))
( T (ATOMPRINT (GRAFT
(LIST (CONS (CAAAR RESTLIST) (ADD1 (CDAR RESTLIST))))
(AND (CDAAR RESTLIST) (LIST (CONS (CDAAR RESTLIST)
     (CDAR RESTLIST))))
           (CDR RESTLIST)))))))
(LIST (CONS (READ) 0))))
```

Both versions are very difficult to read.
With our modern insight (and text editors that automatically indent), a much simpler program is possible:

```lisp
(defun atomprint (exp &optional (depth 0))
  "Print each atom in exp, along with its depth of nesting."
  (if (atom exp)
      (format t "~&ATOM: ~a, DEPTH ~d" exp depth)
      (dolist (element exp)
        (atomprint element (+ depth 1)))))
```

## 1.11 Exercises

&#9635; **Exercise  1.1 [m]** Define a version of `last-name` that handles "Rex Morgan MD," "Morton Downey, Jr.," and whatever other cases you can think of.

&#9635; **Exercise  1.2 [m]** Write a function to exponentiate, or raise a number to an integer power.
For example: `(power 3 2)` = 3<sup>2</sup> = 9.

&#9635; **Exercise  1.3 [m]** Write a function that counts the number of atoms in an expression.
For example: `(count-atoms '(a (b) c)) = 3`.
Notice that there is something of an ambiguity in this: should (`a nil c`) count as three atoms, or as two, because it is equivalent to (`a () c`)?

&#9635; **Exercise  1.4 [m]** Write a function that counts the number of times an expression occurs anywhere within another expression.
Example: `(count-anywhere 'a '(a ((a) b) a)) => 3.`

&#9635; **Exercise  1.5 [m]** Write a function to compute the dot product of two sequences of numbers, represented as lists.
The dot product is computed by multiplying corresponding elements and then adding up the resulting products.
Example:

```lisp
(dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110
```

## 1.12 Answers

### Answer 1.2
```lisp
(defun power (x n)
  "Power raises x to the nth power.  N must be an integer >= 0.
   This executes in log n time, because of the check for even n."
  (cond ((= n 0) 1)
        ((evenp n) (expt (power x (/ n 2)) 2))
        (t (* x (power x (- n 1))))))
```

### Answer 1.3

```lisp
(defun count-atoms (exp)
  "Return the total number of non-nil atoms in the expression."
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))

(defun count-all-atoms (exp &optional (if-null 1))
  "Return the total number of atoms in the expression,
  counting nil as an atom only in non-tail position."
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (count-all-atoms (first exp) 1)
              (count-all-atoms (rest exp) 0)))))
```

### Answer 1.4

```lisp
(defun count-anywhere (item tree)
  "Count the times item appears anywhere within tree."
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere item (first tree))
              (count-anywhere item (rest tree))))))
```

### Answer 1.5
Here are three versions:


```lisp
(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (let ((sum 0))
    (dotimes (i (length a))
      (incf sum (* (elt a i) (elt b i))))
    sum))

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (apply #'+ (mapcar #'* a b)))
```

----------------------

<a id="fn01-1"></a>
[1](#tfn01-1) This list of symbols is not a legal Lisp assignment statement, but it is a Lisp data object.

<a id="fn01-2"></a>
[2](#tfn01-2) The variable `*print-case*` controls how symbols will be printed.
By default, the value of this variable is :`upcase`, but it can be changed to :`downcase` or `:capitalize`.

<a id="fn01-3"></a>
[3](#tfn01-3) Later we will see what happens when the second argument is not a list.

<a id="fn01-4"></a>
[4](#tfn01-4) In ANSI Common Lisp, `last` is defined to return a list of the last *n* elements, where n defaults to 1.
Thus `(last p) = (last p 1) = (PUBLIC)`,and `(last p 2) = (Q PUBLIC)`.
This may make the definition of `last` seem less perverse.

<a id="fn01-5"></a>
[5](#tfn01-5) Just as we can change the value of a variable, we can also change the value of a function in Lisp.
It is not necessary to recompile everything when a change is made, as it would be in other languages.

<a id="fn01-6"></a>
[6](#tfn01-6) For example, symbols that denote so-called *special* variables usually begin and end in asterisks.
Also, note that I did not hesitate to use the symbol `won!` on page 11.

<a id="fn01-7"></a>
[7](#tfn01-7) Actually, there can be several symbol tables.
They are known as *packages* in Common Lisp.

<a id="fn01-8"></a>
[8](#tfn01-8) Pronounced "see-loss." An alternate pronunciation, "klaus," seems to be losing favor.
