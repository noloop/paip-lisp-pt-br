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
  "Retorna o sobrenome de um nome representado como uma lista."
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
  "Retorna o primeiro nome de um nome representado como uma lista."
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

Uma coisa boa sobre a definição de uma lista de nomes, como fizemos acima, é que isso facilita testar nossas
funções. Considere a seguinte expressão que pode ser usada para testar a função `last-name`:

```lisp
> (mapcar #'last-name names)
(PUBLIC X HOPPER SPOT ARISTOTLE MILNE TOP OLIVIER SCARLET)
```

A notação `#'` engraçada é mapeada do nome de uma função para a própria função.
Isso é análogo à notação `'x`.
Na função embutida `mapcar` está sendo passado dois argumentos, a função `last-name` e uma lista de nomes `names`.
Ela retorna uma lista construída chamando a função em todos os elementos da lista de entrada.
Em outras palavras, a chamada de `mapcar` acima é equivalente a:

```lisp
(list (last-name (first names))
      (last-name (second names))
      (last-name (third names))
```
O nome `mapcar` vem do fato que "mapeia" a função em cada um dos argumentos. 
A parte `car` do nome refere-se a função Lisp `car`, um velho nome para `first`. 
E `cdr` é um velho nome para `rest`. 
Os nomes representam "contents of the address register" (conteúdo do registro de endereço) e "contents of the decrement
register" (conteúdo do registro de decremento), as instruções que foram usadas na primeira implementação de Lisp no IBM 704.
Eu estou certo que você irá aceitar que `first` e `rest` são nomes muito melhores, e eles serão usados em vez de `car` e
`cdr` sempre que falarmos sobre listas.
Contudo, nós iremos continuar usando `car` e `cdr` na ocasião em que considerarmos um par de valores que não são
considerados como uma lista. 
Cuidado que alguns programadores ainda usam `car` e `cdr` para listas também.

Aqui temos mais um exemplo de `mapcar`:

```lisp
> (mapcar #'- '(1 2 3 4)) => (-1 -2 -3 -4)

> (mapcar #'+ '(1 2 3 4) '(10 20 30 40)) => (11 22 33 44)
```
Este último exemplo mostra que podemos passar também três argumentos para `mapcar`, nesse caso o primeiro argumento deve ser
uma função binária, qual aplicará para os elementos correspondentes das outras duas listas.
Em geral, `mapcar` espera uma função binária como primeiro argumento, seguida por *n* listas.
Primeiro aplica-se a função para a lista de argumentos obtida coletando o primeiro elemento de cada lista. 
Então é aplicado a função para o segundo elemento de cada lista, e assim por diante, até que uma das listas esteja
esgotada.
É retornado a lista de todos os valores computados pela função.

Agora que entendemos `mapcar`, vamos usá-la para testar a função `first-name`:

```lisp
> (mapcar #'first-name names)
(JOHN MALCOLM ADMIRAL SPOT ARISTOTLE A Z SIR MISS)
```
Nós podemos ficar desapontados com esses resultados.
Suponhamos que queremos uma versão de `first-name` qual ignorasse títulos como "Admiral" e "Miss", e chegasse ao "real" primeiro
nome.
Poderíamos proceder da seguinte maneira:

```lisp
(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "Uma lista de títulos que podem aparecer no início de um nome.")
```

Introduzimos outro novo formulário especial, `defparameter`, qual define um parâmetro, um parâmetro aqui é uma variável 
que não pode mudar em todo curso da computação, mas que pode mudar quando nós pensamos em adicionar novas 
coisas (como o francês "Mme" ou o militar "Lt.").
O formulário `defparameter` também atribuí um valor à variável e permite usá-la nas definições de funções subsequentes.
Nesse exemplo temos exercitado a opção de fornecer a String de documentação qual descreve a variável.
Em `*titles*` se atente aos asteriscos, colocar um nome entre asteriscos é uma convenção amplamente usada entre 
programadores Lisp para marcar variáveis especiais.
Isso e apenas uma convenção; em Lisp, o asterisco é apenas outro caractere que não tem um significado particular.

<a id="tfn01-5"></a>
Em seguida damos uma nova definição para `first-name` qual substituí a definição anterior.[5](#fn01-5)
A definição diz que se a primeira palavra do nome é um nome da lista de títulos, então nós ignoramos essa primeira
palavra e retornamos o `first-name` do resto das outras palavras do nome.
Caso contrário, nós usamos a primeira palavra, como antes.
Outra função embutida, `member`, serve para ver se o primeiro argumento passado é um elemento da lista passada 
como segundo argumento.

O formulário especial `if` tem o formulário `(if *test then-part else-part*)`.
Existem muitos formulários especiais para realizar testes condicionais em Lisp; `if` é o mais apropriado para este exemplo.
Um formulário `if` é avaliado primeiro avaliando a expressão *test*.
Se ela for verdadeira, então a parte *then-part* é avaliada e retornada como valor do formulário `if`; caso contrário a
parte *else-part* é avaliada e retornada.
Enquanto algumas linguagens insistem que o valor do teste condicional deve ser `true` ou `false`, Lisp é mais tolerante.
O teste pode avaliar legalmente qualquer valor.
Apenas o valor `nil` é considerado falso; todos outros valores são considerados verdadeiros.

Note que Lisp não tem uma palavra em sua sintaxe para `false`, então você pode até criar uma variável chamada de "false" 
atribuindo um valor não-nulo, e usá-la como seu teste condicional em um formulário `if`, e ele será 
avaliado como verdadeiro. Mas certamente você não vai querer causar confusão fazendo isso.

Na definição de `first-name` abaixo, a função `member` irá retornar um valor não-nulo (portanto verdade) se o primeiro
elemento do nome está na lista de títulos, e retornará `nil` (portanto falso) se não estiver.

Embora todos não-nulos valores são considerados verdadeiros, por convenção a constante `t` é usada para representar "verdade".

```lisp
(defun first-name (name)
  "Retorna o primeiro nome de um nome representado como uma lista."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))
```
Quando mapeamos a nova `first-name` sobre a lista de nomes, o resultado é mais encorajador.
Além disso, a função recebe o resultado "correto" para `'(Madam Major General Paula Jones)` largando os títulos um de
cada vez.

```lisp
> (mapcar #'first-name names)
(JOHN MALCOLM GRACE SPOT ARISTOTLE A Z LARRY SCARLET)

> (first-name '(Madam Major General Paula Jones))
PAULA
```

Nós podemos ver como isso trabalha rastreando (tracing) a execução da `first-name`, assim vendo os valores passados e
retornados da função.
Os formulários especais `trace` e `untrace` são usados para esta finalidade.

```lisp
> (trace first-name)
(FIRST-NAME)

> (first-name '(John Q Public))
(1 ENTER FIRST-NAME: (JOHN Q PUBLIC))
(1 EXIT FIRST-NAME: JOHN)
JOHN
```

Quando `first-name` é chamada, a definição recebe um único argumento, `name`, com o valor `(JOHN Q PUBLIC)`.
O valor retornado é `JOHN`.
`trace` imprime duas linhas indicando a entrada e saída da função, e então Lisp, como usualmente, imprime o resultado
final, `JOHN`.

O próximo exemplo é mais complicado.
A função `first-name` é usada quatro vezes.
Primeiro, é inserido `(Madam Major General Paula Jones).` como argumento `name`.
O primeiro elemento da lista é `Madam,` e desde que é um membro da lista de títulos, o resultado é computado chamando
`first-name` novamente no restante do nome, que é `(Major General Paula Jones).`.
O processo se repete mais duas vezes, e nós finalmente passamos `(Paula Jones)` para `first-name`.
Desde que `Paula` não é um título, torna-se o resultado dessa chamada para `first-name`, e portanto, o resultado de
todas as quatro chamadas, como mostra o rastreamento.

Quando estivermos felizes com o funcionamento da `first-name`, o formulário especial `untrace` desativa o rastreamento.

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

A função `first-name` é considerada *recursiva* porque sua definição inclui uma chamada para si mesma.
Programadores que são novos no conceito de recursão às vezes o consideram misterioso.
Mas funções recursivas não são realmente diferentes das não recursivas.
Qualquer função tem o requisito de retornar o valor correto para as entradas fornecidas.
Outro caminho de olhar para esse requisito é de quebrar isso em duas partes: a função deve retornar um valor, e ela
não deve retornar nenhum valor incorreto.
Essa segunda parte do requisito é equivalente ao primeiro, mas facilita projetar e pensar sobre definições de funções e
seu design.

A seguir, eu mostro uma descrição abstrata do problema `first-name`, destacando o design de função e o fato que soluções
recursivas não estão vinculadas ao Lisp de alguma forma:

```lisp
function first-name(name):
  Se *O primeiro elemento do nome é um título*
    então *faça alguma coisa complicada para obter o primeiro nome*
    senão *retorne o primeiro elemento do nome*
```

Isso divide o problema em dois casos.
O segundo cado, retornamos a resposta, e é de fato a resposta correta.
Ainda não especificamos o que fazer no primeiro caso.
Mas sabemos que algo tem de ser feito com o restante do nome após o primeiro elemento, e que o queremos é extrair o
primeiro nome desses elementos.

O pulo de gato é ir a frente e usar `first-name`, mesmo que ainda não tenha sido totalmente definida:

```lisp
function first-name(name):
  Se *O primeiro elemento do nome é um título*
    então *retorne o primeiro nome (ou seja, a função first-name) do resto do nome*
    senão *retorne o primeiro elemento do nome*
```

Agora o primeiro caso em `first-name` é recursivo, e o segundo caso permanece inalterado.
Já concordamos que o segundo caso retorna a resposta correta, e que o primeiro caso apenas retorna o que `first-name`
retorna.
Então `first-name` como um todo só pode retornar respostas corretas.
Portanto, temos metade do caminho mostrando que a função está correta; e a outra metade está mostrando que,
eventualmente, retorna alguma reposta.
Mas toda chamada recursiva corta o primeiro elemento e olha para o resto, então para uma lista de *n* elementos poderemos
ter no máximo *n* chamadas recursivas.
Isso completa a demostração que a função está correta.
Programadores que aprendem a pensar dessa maneira consideram a recursão uma ferramenta valiosa e não um mistério confuso.

## 1.7 Funções de ordem superior

As funções em Lisp não podem apenas ser "chamadas", ou aplicadas a argumentos, mas também podem ser manipuladas como
qualquer outro tipo de objeto.
Uma função que aceita outra função como um argumento é chamada de função de ordem superior (higher-order function).
`mapcar` é um exemplo.
Para demonstrar o estilo de programação de funções de ordem superior, iremos definir uma nova função chamada `mappend`,
qual aceita dois argumentos, uma função e uma lista.
`mappend` mapeia a função sobre cada elemento da lista e une todos os resultados.
A primeira definição segue imediatamente a descrição e o fato que a função `apply` pode ser usada para aplicar uma
função a uma lista de argumentos.

```lisp
(defun mappend (fn the-list)
  "Aplique fn a cada elemento da lista e adicione os resultados."
  (apply #'append (mapcar fn the-list)))
```

Agora experimentamos um pouco para ver como `apply` e `mappend` funcionam.
O primeiro exemplo aplica a função de adição para uma lista de quatro números.

```lisp
> (apply #'+ '(1 2 3 4)) => 10
```

O próximo exemplo aplica a função `append` para uma lista de dois argumentos, onde cada argumento é uma lista.
Se os argumentos não forem listas, isso daria errado.

```lisp
> (apply #'append '((1 2 3) (a b c))) => (1 2 3 A B C)
```

Agora definimos uma nova função, `self-and-double`, e a aplicamos uma variedade de argumentos.

```lisp
> (defun self-and-double (x) (list x (+ x x)))

> (self-and-double 3) => (3 6)

> (apply #'self-and-double '(3)) => (3 6)
```

Se tentarmos aplicar `self-and-double` para uma lista de mais de um argumento, ou para uma lista que não tenha nenhum
número, isso seria um erro, assim como seria um erro avaliar `(self-and-double 3 4)` ou `(self-and-double 'kim)`.
Agora vamos retornar para as funções de mapeamento:

```lisp
> (mapcar #'self-and-double '(1 10 300)) => ((1 2) (10 20) (300 600))

> (mappend #'self-and-double '(1 10 300)) => (1 2 10 20 300 600)
```

Quando `mapcar` recebe uma função e uma lista com três argumentos, isso sempre retorna uma lista com três valores.
Cada valor é o resultado da chamada da função no respectivo argumento.
Em contraste, quando `mappend` é chamada, é retornada uma grande lista, qual é igual a todos os valores que `mapcar`
geraria anexados juntos.
Seria um erro chamar `mappend` com uma função que não retorna listas, porque `append` espera ver listas como seus
argumentos.

Agora considere o seguinte problema: ao dar uma lista de elementos, retorne uma lista consistindo de todos os números na
lista original e a negação desses números.
Por exemplo, ao dar a lista `(testing 1 2 3 test)`, retorne `(1 -1 2 -2 3 -3)`.
Este problema pode ser resolvido facilmente usando `mappend` como um componente:

```lisp
(defun numbers-and-negations (input)
  "Dada uma lista, retorne apenas os números e suas negações."
  (mappend #'number-and-negation input))

(defun number-and-negation (x)
  "Se x é um número, retorne uma lista de x e -x."
  (if (numberp x)
      (list x (- x))
      nil))

> (numbers-and-negations '(testing 1 2 3 test)) => (1 -1 2 -2 3 -3)
```

A definição alternativa de `mappend` mostrada um seguir não faz uso de `mapcar`, em vez disso, cria a lista utiliza 
o conceito de recursão para aplicar-se a um elemento de cada vez:

```lisp
(defun mappend (fn the-list)
  "Aplique fn a cada elemento da lista e adicione os resultados."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))
```

`funcall` é similar a `apply`, pois também recebe uma função como seu primeiro argumento e aplica a função para uma
lista de argumentos, mas `funcall` lista os argumentos separadamente:

```lisp
> (funcall #'+ 2 3) => 5

> (apply #' + '(2 3)) => 5

> (funcall #' + '(2 3)) => *Error: (2 3) is not a number.*
```

Eles são equivalentes a `(+  2 3)`, `(+ 2 3)` e `(+ '(2 3))`, respectivamente.

Até agora, todas funções que usamos foram predefinidas no Common Lisp ou introduzida com um `defun`, que emparelha 
uma função com um nome.
Também é possível introduzir uma função sem dar um nome a ela, usando a sintaxe especial `lambda`.

O nome *lambda* vem da notação de funções do matemático Alonzo Church's(Church 1941).
Lisp geralmente prefere nomes expressivos a letras Gregas concisas, mas `lambda` é uma exceção.
Um melhor nome seria `make-function`.
Lambda deriva da notação em *Principia Mathematica* de Russell e Whitehead, que usava um sinal de intercalação sobre
variáveis ligadas: *x&#x302;*(*x + x*).

Alonzo Church queria uma String unidimensional, então ele colocou o acento circunflexo em frente: *^x*(*x + x*).
O acento circunflexo parecia engraçado sem nada embaixo, então Church mudou para coisa mais próxima, uma lambda
maiúscula, *&Lambda;x*(*x + x*).
O &Lambda; é facilmente confundido com outros símbolos, então eventualmente, o lambda minúsculo foi o 
substituto: *&lambda;x*(*x + x*). 
John McCarthy era aluno de Church em Princeton, então quando McCarthy inventou Lisp em 1958, ele adotou a notação lambda.

Não havia letras Gregas nas teclas dessa época, então McCarthy usou `(lambda (x) (+ x x))`, e isso sobreviveu até hoje.
Em geral, o formulário de uma expressão lambda é:

```lisp
(lambda (*parameters...*) *body...*)
```

A expressão lambda é apenas um nome (o *name* do formulário `defun`) não atômico para uma função, assim como `append` é
um nome atômico para uma função embutida.
Sendo assim, é apropriado para o uso na primeira posição de uma chamada de função, mas se queremos chegar a função real,
e não ao nome, ainda precisamos usar a notação `#'`.
Por exemplo:

```lisp
> ((lambda (x) (+ x 2)) 4) => 6

> (funcall #'(lambda (x) (+ x 2)) 4) => 6
```

Para entender a diferença, precisamos esclarecer como as expressões são avaliadas no Lisp.
A regra normal para avaliação afirma que os símbolos são avaliados consultando o valor da variável a que o símbolo 
se refere.
Portanto, o `x` em `(+ x 2)` é avaliado consultando o valor da variável chamada `x`.
Uma lista é avaliada de uma de duas maneiras.
Se o primeiro elemento da lista é um operador de formulário especial, então a lista é avaliada de acordo com a regra
de sintaxe para esse formulário especial.
Caso contrário, a lista representa uma chamada de função.
O primeiro elemento é avaliado de uma maneira única, como uma função.
Isso significa que podemos ter um símbolo ou uma expressão lambda.
Nos dois casos, a função nomeada pelo primeiro elemento é aplicada aos valores dos elementos restantes da lista.
Esses valores são determinados pelas regras normais de avaliação.
Se quisermos nos referir a uma função em uma posição que não seja o primeiro elemento de uma chamada de função,
precisamos usar a notação `#'`.
Caso contrário, as expressões serão valiadas pela regra normal de avaliação, e não irá ser tratada como funções.
Por exemplo:

```lisp
> append => *Error: APPEND is not a bound variable*

> (lambda (x) (+ x 2)) => *Error: LAMBDA is not a function*
```

Aqui temos mais alguns exemplos do uso correto de funções:

```lisp
>(mapcar #'(lambda (x) (+ x x))
         '(1 2 3 4 5)) =>
(2 4 6 8 10)

> (mappend #'(lambda (l) (list l (reverse l)))
           ((1 2 3) (a b c))) =>
((1 2 3) (3 2 1) (A B C) (C B A))
```

Programadores que estão acostumados a outras linguagens às vezes falham em ver o ponto das 
expressões lambda.
Existem duas razões porque expressões lambda são muito úteis.

Primeiro, pode ser confuso desorganizar um programa com nomes supérfluos.
Assim como é mais claro escrever `(a+b)*(c+d)` ao invés de inventar nomes como `temp1` e `temp2` para guardar `a+b` e
`c+d`, por isso pode ser mais claro definir uma função como uma expressão lambda do que inventar nomes para ela.

Segundo, e mais importante, as expressões lambda tornam possível criar novas funções em tempo de execução.
Isso é uma técnica poderosa que não é possível na maioria das linguagens de programação.
Essas funções de tempo de execução, conhecidas como encerramentos/fechamentos (closures), serão abordadas na seção 3.16.

## 1.8 Outros tipos de dados

Até agora temos visto apenas quatro tipos de objetos Lisp: números (numbers), símbolos (symbols), listas (lists) e
funções (functions).
Lisp atualmente define certa de 25 diferentes tipos de objetos: vetores (vectors), matrizes (arrays), estruturas
(structures), caracteres (characters), fluxos (streams), tabelas de hash (hash tables), entre outros.

Neste ponto, apresentamos mais uma, a cadeia de caracteres (string).
Como você pode ver a seguir, cadeia de caracteres, assim como os números, são avaliadas por elas mesmas.
Strings são usadas principalmente para imprimir mensagens, enquanto símbolos são usados para seus relacionamentos 
com outros objetos e para nomear variáveis.
A representação impressa de uma string tem uma marcação de aspas dupla `(")` em cada extremidade.

```lisp
> "a string" => "a string"

> (length "a string") => 8

> (length "") => 0
```

## 1.9 Sumário: A regra de avaliação do Lisp

Podemos agora resumir a regra de avaliação do Lisp:

*   Toda expressão é uma *lista* ou um *átomo*.

*   Toda lista avaliada é uma *expressão de formulário especial* ou uma *aplicação de função*.

*   Uma *expressão de formulário especial* é definida como uma lista cujo primeiro elemento da lista é um 
    operador de fomulário especial. A expressão é avaliada de acordo com a regra de avaliação 
    idiossincrática do operador. Por exemplo, 
    a regra de avaliação para `setf` é avaliar o segundo argumento de acordo com a regra de avaliação normal, e definir 
    ao primeiro argumento esse valor do segundo argumento, e retornar o valor como resultado. A regra para `defun` é
    definir a nova função, e retornar o nome da função. A regra para `quote` é retornar o primeiro argumento não
    avaliado. A notação `'x` é uma abreviação para a expressão de fomulário especial `(quote x)`. Similarmente, a
    notação `#'f` é uma abreviação para a expressão de formulário especial `(function f)`.
    
```lisp
'John = (quote John) => JOHN

(setf p 'John) => JOHN

(defun twice (x) (+ x x)) => TWICE

(if (=  2 3) (error) (+  5 6)) => 11
```

*   A *aplicação de função* é avaliada primeiro avaliando os argumentos (o restante da lista) e então localizando a 
    função nomeada pelo primeiro elemento da lista e aplicando essa função à lista de argumentos avaliados.

```lisp
(+  2 3) => 5
(- (+  90 9) (+  50 5 (length '(Pat Kim)))) => 42
```

Note que se `'(Pat Kim)` não tiver uma citação (`quote`), seria tratada como uma aplicação de função da função `pat`
para o valor da variável `kim`.

*   Todo átomo é um *símbolo* ou um *não-símbolo*.

*   Um *símbolo* avalia o valor mais recente atribuído à variável nomeada por esse símbolo. Símbolos são compostos de
    letras, e possíveis dígitos, e raramente, caracteres de pontuação. <a id="tfn01-6"></a>
    Para evitar confusão, iremos usar símbolos compostos principalmente de letras `a-z` e o caractere `-`, 
    com algumas exceções.[6](#fn01-6)

```lisp
names
p
*print-pretty*
```

*   Um átomo *não-símbolo* é avaliado por si mesmo. Por enquanto, números e cadeias de caracteres são os únicos
    não-símbolos que conhecemos. Números são compostos de dígitos, e possivelmente um ponto decimal e um sinal. Existem
    também disposições para notação científica, racionais e números complexos, e números com bases diferentes, mas não
    iremos descrever os detalhes aqui. Cadeias de caracteres são delimitadas por marcação de aspas dupla em suas extremidades.

```lisp
42 => 42
-273.15 => -273.15
"a string" => "a string"
```

Existe alguns pequenos detalhes do Common Lisp que complicam as regras de avaliação, mas essa definição será suficiente
por enquanto.

Uma complicação que causa confusão em Lispers iniciantes é a diferença entre *ler* e *avaliar* uma expressão.
Iniciantes geralmente imaginam que quando eles escrevem uma expressão, como:

```lisp
> (+ (* 3 4) (* 5 6))
```

O sistema Lisp primeiro lê o `(+`, então busca a função de adição, depois lê `(* 3 4)` e computa `12`, depois 
lê `(* 5 6)` e computa `30`, e finalmente computa `42`.
De fato, o que realmente acontece é que o sistema primeiro lê toda a expressão, a lista `(+ (* 3 4) (* 5 6))`.
Somente depois da leitura, o sistema começa a avaliar.
A avaliação pode ser feita por um interpretador que olha a lista diretamente, ou pode ser feita por um compilador que
traduz a lista em instruções de linguagem de máquina e depois executa essas instruções.

Podemos ver agora que era um pouco impreciso dizer, "Números são compostos de dígitos, e possivelmente um ponto decimal
e um sinal." Seria mais preciso dizer que a representação impressa de um número, como esperado pela função lida, e como
produzida pela função de impressão, é compostas de dígitos, e possivelmente um ponto decimal e sinal.
A representação interna do número varia de um computador para outro, mas você pode ter certeza que haverá um padrão de 
bits em um determinado local da memória e não conterá mais os caracteres originais usados para representar o número em
notação decimal. Similarmente, é a representação impressa de uma cadeia de caracteres que é cercada com marcação de
aspas dupla; a representação interna é um local da memória que marca o início de um vetor de caracteres.

Os iniciantes que não conseguem entender a distinção entre leitura e avaliação podem ter um bom modelo do que as expressões
avaliam, mas eles geralmente têm um modelo terrível da eficiência da avaliação de expressões.
Um aluno usou apenas nomes de variáveis de uma letra, porque ele intuiu que seria mais rápido para o computador procurar 
para um nome de uma letra do que um nome de muitas letras.
Embora pode ser verdade que nomes mais curtos possam economizar um microssegundo no tempo de leitura, isso não faz diferença no
tempo total de avaliação. 
Toda variável, independente do seu nome, é apenas um local da memória, e o tempo de acesso a esse local não depende do nome
da variável.

## 1.10 O que torna o Lisp diferente?

O que é que diferencia Lisp de outras linguagens?
Porque é uma boa linguagem para aplicações de IA?
Há pelo menos oito fatores importantes:

*   Suporte embutido para listas
*   Gerenciamento automático de armazenamento
*   Tipagem dinâmica
*   Funções de Primeira Classe
*   Sintaxe uniforme
*   Ambiente interativo
*   Extensibilidade
*   História

Em suma, esses fatores permitem que um programador adie a tomada de decisões.
No exemplo de lidar com nomes, fomos capazes de usar as funções embutidas para manipulação de listas para construir e
manipular nomes sem tomar um monte de decisões explícitas sobre sua representação.
Se decidimos mudar a representação, seria fácil voltar e alterar partes do programa, deixando outras partes
inalteradas.

Essa habilidade de adiar decisões, ou com mais precisão, tomar temporariamente, decisões não vinculativas, geralmente é
uma coisa boa, porque isso significa que detalhes irrelevantes podem ser ignorados.
Existem também alguns pontos negativos no adiamento de decisões.
Primeiro, quanto menos dissermos ao compilador, maior é chance dele produzir código ineficiente.
Segundo, quanto menos dissermos ao compilador, menor é a chance dele perceber inconsistências e nos avisar.
Os erros podem não ser detectados até que o programa seja executado.
Vamos considerar cada fator com mais profundidade, pesando suas vantagens e desvantagens:

*   *Suporte embutido para listas.*
A lista é uma estrutura de dados muito versátil, e embora listas podem ser implementadas em qualquer linguagem, Lisp
facilita o uso delas.
Muitas aplicações de IA envolvem listas que constantemente têm seu tamanho alterado, tornando as estruturas de tamanho fixo como
vetores de difícil uso.
As versões iniciais do Lisp usavam listas como sua única estrutura de dados agregada.
Common Lisp fornece outros tipos, porque listas não são sempre a escolha mais eficiente.

*   *Gerenciamento automático de armazenamento.*
O programador Lisp não precisa acompanhar a alocação de memória; tudo é feito automaticamente.
Isso liberta o programador de um monte de esforço, e facilita o uso de programação funcional.
Outras linguagens presenteiam aos programadores com uma escolha.
Variáveis podem ser alocadas na pilha, o que significa que elas são criadas quando o procedimento é inserido, e desaparece
quando o procedimento é concluído.
Esse é um uso eficiente do armazenamento, mas exclui funções que retornam valores complexos.
A outra escolha é o programador explicitamente alocar e liberar armazenamento.
Isso faz a programação funcional ser possível, mas pode levar a erros.

Por exemplo, considere o simples problema de calcular a expressão *a* x (*b* + *c*), onde *a*, *b* e *c* são números.
O código é trivial em qualquer linguagem, aqui temos em Pascal e em Lisp:

| []()           |                 |
|----------------|-----------------|
| `/* Pascal */` | `;;; Lisp`      |
| `a * (b + c)`  | `(* a (+ b c))` |

A única diferença é que Pascal usa notação infixa e Lisp usa prefixa.
Considere agora calcular *a* x (*b* + *c*), onde *a*, *b* e *c* são matrizes.
Suponha que temos procedimentos para multiplicação e adição de matrizes.
No Lisp a forma é exatamente a mesma; apenas os nomes das funções irão mudar.
Em Pascal nós temos a escolha das abordagens mencionadas anteriormente.
Poderíamos declarar variáveis temporárias para manter resultados intermediários na pilha (stack), e substituir a
expressão funcional com uma série de chamadas de procedimentos:

| []()                        |                      |
|-----------------------------|----------------------|
| `/* Pascal */`              | `;;; Lisp`           |
| `var temp, result: matrix;` |                      |
| `add(b,c,temp);`            | `(mult a (add b c))` |
| `mult(a,temp,result);`      |                      |
| `return(result);`           |                      |

A outra escolha é escrever funções em Pascal que alocam novas matrizes no monte (heap).
Então pode-se escrever boas expressões funcionais como `mult(a,add(b,c))` mesmo em Pascal.
Contudo, na prática raramente isso funciona, por causa da necessidade de manipular o armazenamento explicitamente: 

| []()                     |                      |
|--------------------------|----------------------|
| `/* Pascal */`           | `;;; Lisp`           |
| `var a,b,c,x,y: matrix;` |                      |
| `x := add(b.c);`         | `(mult a (add b c))` |
| `y := mult(a,x);`        |                      |
| `free(x);`               |                      |
| `return(y);`             |                      |

No geral, decidir quais estruturas liberar é uma dificuldade difícil para o programador Pascal.
Se o programador perder algo, então o programa pode ficar sem memória.
Pior ainda, se o programador liberar uma estrutura que ainda está sendo usada, então erros estranhos podem ocorrer quando
esse pedaço da memória é realocado.
Lisp automaticamente aloca e libera estruturas, para que esses dois tipos de erros *nunca* ocorram.

*   *Tipagem dinâmica.*
Os programadores Lisp não têm que fornecer declarações de tipo, porque a linguagem mantém o controle dos tipos de cada
objeto em tempo de execução, em vez de figurar todos os tipos em tempo de compilação.
Isso torna os programas Lisp curtos e, consequentemente, mais rápido de desenvolver, e isso também significa que as 
funções podem frequentemente ser estendidos para trabalhar com objetos aos quais eles não foram originalmente 
destinados a trabalhar. 
No Pascal, podemos escrever um procedimento para classificar uma matriz de 100 inteiros, mas não podemos usar o mesmo
procedimento para classificar 200 inteiros, ou 100 cadeias de caráteres.
No Lisp, um `sort` serve para todos.
O caminho apreciado desse tipo de flexibilidade é ver quão difícil é alcançar isso em outras linguagens.
Isso é impossível em Pascal; em fato, a linguagem Modula foi inventada principalmente para corrigir esse problema em
Pascal.
A linguagem Ada foi designada a permitir funções genéricas flexíveis, e um livro por Musser e Stepanov (1989) descreve um
pacote Ada que dá algumas das funcionalidades das funções de sequência do Common Lisp.
Mas a solução de Ada está abaixo do ideal: é necessário um livro de 264 páginas para duplicar apenas uma parte da
funcionalidade do capítulo 14 de 20 páginas de Steele (1990), e Musser e Stepanov passaram por cinco compiladores Ada
antes de encontrar um que compilasse corretamente seu pacote.
Também, seus pacotes eram considerados menos poderosos, pois não trata de vetores ou parâmetros de palavra-chave opcionais.
Em Common Lisp, toda essa funcionalidade vem de graça, e é fácil adicionar mais.
Por outro lado, a tipagem dinâmica significa que alguns erros não serão detectados até o tempo de execução.
A grande vantagem de linguagens fortemente tipadas é que elas permitem obter mensagens de erros em tempo de compilação.
A grande frustração com linguagens fortemente tipadas é que elas apenas permitem alertar sobre uma pequena classe de
erros.
Elas podem dizer que você está passando erroneamente uma cadeia de caracteres para uma função que espera um inteiro, mas
elas não pode dizer que você está passando um número ímpar para a função que espera um número par.

*   *Funções de Primeira Classe.*
O objeto de *Primeira Classe* é que pode ser usado em qualquer lugar e pode ser manipulado da mesma maneira que qualquer
outro tipo de objeto.
Em Pascal ou C, por exemplo, funções podem ser passadas como argumentos para outras funções, mas elas não são funções de
primeira classe, porque não é possível criar novas funções enquanto o programa é executado, nem é possível criar funções
anônimas sem dar um nome.
No Lisp podemos fazer essas mesmas coisas usando `lambda`.
Isso é explicado na seção 3.16, na página 92.

*   *Sintaxe uniforme.*
A sintaxe dos programas Lisp é simples.
Isso facilita o aprendizado da linguagem, assim desperdiçando muito pouco tempo para corrigir erros de tipos.
Além disso, é fácil escrever programas que manipulam outro programas ou que definem linguagens totalmente novas, qual é uma
técnica muito poderosa.
A sintaxe simples também facilita a análise de Lisp por programas de edição de texto.
Seu programa editor deve poder recuar expressões automaticamente e mostrar parênteses correspondentes.
Isso é mais difícil de fazer em linguagens com sintaxe complexa.
Por outro lado, algumas pessoas se opõem a todos os parênteses.
Existem duas respostas para essa objeção.
Primeiro, considere a alternativa: uma linguagem com sintaxe "convencional", os pares de parênteses do Lisp seriam
substituídos por uma regra implícita de precedência do operador (no caso de expressões aritméticas e lógicas) ou por um par de
`being/end`, no português, `início/fim` (no caso de estruturas de controle).
Mas nenhuma delas é necessariamente uma vantagem.
A precedência implícita é notoriamente propensa a erros, e os pares de `begin/end` bagunçam a página sem adicionar
nenhum conteúdo.
Muitas linguagens estão se afastando do `begin/end`, C usa `{}`, que são equivalentes a parênteses, e várias linguagens
funcionais modernas (como Haskell) usam espaços em branco horizontal, sem nenhum agrupamento explícito.
Segundo, muitos programadores Lisp *têm* considerado a alternativa.
Existem diversos pré-processadores que convertem a sintaxe "convencional" para o Lisp.
Nada disso pegou.
Não é que os programadores Lisp acham *tolerável* usar todos esses parênteses, pelo contrário, eles acham *vantajoso*.
E com um pouco de experiência, você também achará.
Também é importante que a sintaxe dos dados do Lisp seja a mesma que a dos programas.
Obviamente, isso facilita a conversão de dados em programas.
Menos óbvio é o tempo economizado por ter funções universais para lidar com entrada e saída.
As funções de Lisp `read` e `print` irão lidar automaticamente com qualquer lista, estrutura, cadeia de caracteres ou números.
Isso torna trivial testar funções individuais enquanto você desenvolve seu programa.
Em uma linguagem tradicional como C ou Pascal, você teria que escrever funções de propósito especial para ler e imprimir
cada tipo de dados que desejasse depurar, além de um driver de propósito especial para chamar as rotinas.
Como isso consome tempo e é propenso a erros, a tentação é evitar os testes completamente.
Portanto, Lisp encoraja programas melhor testados, e facilita o desenvolvimento rápido.

*   *Ambiente interativo.*
Tradicionalmente, um programador escreveria um programa completo primeiro, e depois iria compilar o programa escrito, 
depois corrigiria qualquer erro detectado pelo compilador, e então o executaria e depuraria.
Isso é conhecido como modo *lote* de interação.
Para longos programas, aguardar o compilador terminar seu trabalho ocupava grande parte do tempo da depuração.
No Lisp, normalmente se escreve pequenas funções de cada vez, obtendo o feedback do sistema Lisp após avaliar cada uma
delas.
Isso é conhecido ambiente *interativo*.
Quando chega a hora de fazer uma alteração, apenas as funções alteradas precisam ser recompiladas, portanto a espera é muito menor.
Além disso, o programador Lisp pode depurar digitando expressões arbitrárias a qualquer momento.
Isso é uma grande melhoria em relação a edição de programas para introduzir instruções de impressão e depois ter que
recompilar tudo novamente.
Aviso que a distinção entre uma linguagem *interativa* e uma linguagem de *lote* é diferente da distinção entre
linguagens *interpretadas* e *compiladas*.
Muitas vezes foi afirmado, incorretamente, que Lisp tem uma vantagem em virtude de ser uma linguagem interpretada.
Atualmente, experientes programadores Common Lisp tendem a usar o compilador quase exclusivamente. 
O importante ponto é interação, não interpretação.
A idéia de um ambiente interativo é tão boa que até mesmo idiomas tradicionais como C e Pascal estão começando a 
oferecer versões interativas, portanto, essa não é uma vantagem exclusiva do Lisp.
No entanto, o Lisp ainda oferece um acesso muito melhor aos recursos interativos.
Um interpretador C permite o programador digitar uma expressão e a avaliar imediatamente, mas isso não irá
permitir o programador escrever um programa que, digamos, atravessa a tabela de símbolos e encontre todas as funções
definidas pelo usuário e imprime informações sobre elas.
<a id="tfn01-7"></a>
No Lisp, a tabela de símbolo é um *Objeto de Primeira Classe*[7](#fn01-7) que pode ser acessado e modificado com funções
como: `read`, `intern` e `do-symbols`.
Common Lisp oferece um conjunto extraordinariamente rico de ferramentas úteis, incluindo mais de 700 funções embutidas
(ANSI Common Lisp tem mais de 900).
Assim, escrever um novo programa envolve mais coleta de partes de código existentes e menos escrever novos códigos a
partir do zero.
Além disso, para as funções padrões, as implementações Common Lisp normalmente oferecem extensões para interação com o
editor, depurador e sistema de janelas.

*   *Extensibilidade*.
Quando Lisp foi inventado em 1958, ninguém poderia prever os avanços na teoria da programação e no design de linguagem
que ocorreram nos últimos trinta anos.
Outras linguagens antigas foram descartadas, substituídas por outras baseadas em idéias mais recentes.
No entanto, o Lisp é extensivo, e foi alterado para incorporar os novos recursos que se tornaram populares.
A maneira mais fácil para estender a linguagem é com macros.
Quando as construções de programação estruturada como *case* e *if-then-else* surgiram, elas foram incorporadas ao Lisp como macros.
Mas a flexibilidade do Lisp vai além de adicionar construções individuais.
Novos estilos de programação podem ser facilmente implementados.
Muitas aplicações de IA são baseadas na ideia de programação *baseadas em regras*.
<a id="tfn01-8"></a>
Outro novo estilo é a programação *orientada a objetos*, que foi incorporada com o Common Lisp Object System
(CLOS)[8](#fn01-8), um conjunto de macros, funções, e tipos de dados que tem sido integrado no ANSI Common Lisp.

Para mostrar até que ponto o Lisp chegou, aqui está um programa de amostra fornecido no *Lisp/MTS Programmer's Guide* (Hafner and Wilcox 1974:

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

Note o uso da obsoleta instrução goto `(GO)`, e a falta de convenções de recuo consistente.
O manual também fornece uma recursiva versão do mesmo programa:

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

Ambas versões são muito difíceis de ler.
Com nossa visão moderna (e editores de texto que recuam automaticamente), é possível um programa muito mais simples:

```lisp
(defun atomprint (exp &optional (depth 0))
  "Imprima cada átomo em exp, juntamente com sua profundidade de aninhamento."
  (if (atom exp)
      (format t "~&ATOM: ~a, DEPTH ~d" exp depth)
      (dolist (element exp)
        (atomprint element (+ depth 1)))))
```

## 1.11 Exercícios

&#9635; **Exercício  1.1 [m]** Define uma versão da `last-name` que lida com "Rex Morgan MD," "Morton Downey, Jr.," e quaisquer outros casos que você
possa pensar.

&#9635; **Exercício  1.2 [m]** Escreva uma função para exponenciar ou aumentar um número para uma potência inteira. Por exemplo: `(power 3 2)` = 3<sup>2</sup> = 9.

&#9635; **Exercício  1.3 [m]** 
Escreva uma função que conte o número de átomos em uma expressão. Por exemplo: `(count-atoms '(a (b) c)) = 3`.
Observe que há algo de ambiguidade nisso: deve (`a nil c`) contar como três átomos ou como dois, porque é equivalente a (`a () c`)?

&#9635; **Exercício  1.4 [m]** 
Escreva uma função que conte o número de vezes que uma expressão ocorre em qualquer lugar dentro de outra expressão.
Exemplo: `(count-anywhere 'a '(a ((a) b) a)) => 3.`

&#9635; **Exercício  1.5 [m]** 
Escreva uma função para computar o produto escalar de duas sequências de números, representados como listas.
O produto escalar é calculado multiplicando os elementos correspondentes e adicionando os produtos resultantes. 
Exemplo:

```lisp
(dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110
```

## 1.12 Repostas

### Resposta 1.1

Essa questão não foi respondida no livro em Inglês, então eu (@noloop) resolvi 
respondê-la aqui nesta versão em português do PAIP Lisp.
Veja que existem muitos caminhos para responder essa questão, mas me baseando na 
função `last-name` que foi apresentada anteriormente, tratei cada nome como um 
símbolo em uma lista de símbolos, desconsiderando vírgulas e pontos, 
assim como Norvig fez na sua versão de `last-name`. 

```lisp
(defvar *titles*
  '(Mr Mrs Miss Ms Sir MD Jr Madam Dr Admiral Major General)
  "Uma lista de títulos que podem aparecer no início de um nome.")

(defun last-name (fullname)
  "Retorne o sobrenome para o nome completo, exceto quando forem títulos.
   Examples: (last-name '(Morton Downey Jr)) => Downey
             (last-name '(Rex Morgan MD)) => Morgan
             (last-name '(MD Jr)) => NIL"
  (let ((lastname (first (last fullname))))
    (cond ((null fullname) nil)
	  ((find lastname *titles*) (last-name (reverse (cdr (reverse fullname)))))
	  (t lastname))))

```

### Resposta 1.2

```lisp
(defun power (x n)
  "Power eleva x à enésima potência. N deve ser um número inteiro >= 0. 
   Isso é executado no tempo log(n), devido à verificação do n mesmo."
  (cond ((= n 0) 1)
        ((evenp n) (expt (power x (/ n 2)) 2))
        (t (* x (power x (- n 1))))))
```

### Resposta 1.3

```lisp
(defun count-atoms (exp)
  "Retorne o número total de átomos diferentes de nil na expressão."
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))

(defun count-all-atoms (exp &optional (if-null 1))
  "Retorna o número total de átomos na expressão, 
   contando nil como um átomo apenas na posição não cauda."
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (count-all-atoms (first exp) 1)
              (count-all-atoms (rest exp) 0)))))
```

### Resposta 1.4

```lisp
(defun count-anywhere (item tree)
  "Contar as vezes que o item aparece em qualquer lugar da árvore."
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere item (first tree))
              (count-anywhere item (rest tree))))))
```

### Resposta 1.5

Aqui estão três versões:

```lisp
(defun dot-product (a b)
  "Calcule o produto escalar matemático de dois vetores."
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun dot-product (a b)
  "Calcule o produto escalar matemático de dois vetores."
  (let ((sum 0))
    (dotimes (i (length a))
      (incf sum (* (elt a i) (elt b i))))
    sum))

(defun dot-product (a b)
  "Calcule o produto escalar matemático de dois vetores."
  (apply #'+ (mapcar #'* a b)))
```

----------------------

<a id="fn01-1"></a>
[1](#tfn01-1) Esta lista de símbolos não é uma instrução legal de Lisp, mas é um objeto de dados Lisp.

<a id="fn01-2"></a>
[2](#tfn01-2) A variável `*print-case*` controla como símbolos irão ser imprimidos.
Por padrão, o valor dessa variável é: `upcase`, mas isso pode ser alterado para: `downcase` or `:capitalize`

<a id="fn01-3"></a>
[3](#tfn01-3) Mais tarde veremos o que acontece quando o segundo argumento não é uma lista.

<a id="fn01-4"></a>
[4](#tfn01-4) Em ANSI Common Lisp, `last` é definido para retornar uma lista dos últimos *n* elementos, onde *n* por
padrão é 1.
Portanto `(last p) = (last p 1) = (PUBLIC)`, e `(last p 2) = (Q PUBLIC)`.
Isso pode fazer que a definição de `last` pareça menos perversa.

<a id="fn01-5"></a>
[5](#tfn01-5) Assim como podemos alterar o valor de uma variável, nós podemos alterar também o valor de uma função no
Lisp.
Não é necessário recompilar tudo quando uma alteração é feita, como seria em outras linguagens.

<a id="fn01-6"></a>
[6](#tfn01-6) Por exemplo, símbolos que usam `*` em suas extremidades são chamados de variáveis especiais. Por exemplo: `*special*`. 
Também, perceba que eu não hesitei em usar o símbolo `won!` na página 11.

<a id="fn01-7"></a>
[7](#tfn01-7) Atualmente, pode haver várias tabelas símbolos.
Eles são conhecidos como *packages* no Common Lisp.

<a id="fn01-8"></a>
[8](#tfn01-8) Pronunciado "see-loss." Uma pronúncia alternativa, "klaus," parece está perdendo o favor.
