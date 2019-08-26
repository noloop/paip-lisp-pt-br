# Capítulo 1

## Introdução ao Lisp

> You think you know when you learn, are more sure when you can write, even more when you can teach, but certain when you can program. 
> - Alan Perlis, Yale University computer scientist

Em português:

>Você acha que sabe quando você aprende, tem mais certeza quando você pode escrever, ainda mais quando você pode ensinar, mas certo quando você pode programar.
> - Alan Perlis, Cientista da computação da Universidade de Yale

Este capítulo é para pessoas com pouca ou nenhuma experiência em Lisp. Leitores que sentem confiantes em sua habilidade de programação em Lisp, podem rapidamente dar uma olhada no capítulo ou ignorá-lo completamente. Este capítulo necessariamente se move rapidamente, então aqueles com pouca experiência em programação, ou qualquer leitor que ache este capítulo difícil, deve procurar um texto introdutório suplementar. Minhas recomendações estão no prefácio.
Computadores permitem realizar computações. Um programa de processamento de palavras lida com palavras, enquanto uma calculadora lida com números, mas os princípios são os mesmos. Em ambos os casos, você fornece a entrada (no inglês:  input, no exemplo deste parágrafo seriam palavras ou números.) e especifica as operações (no inglês:  operations, como excluir uma palavra ou adicionar dois números) para produzir um resultado (no inglês: output ou results, um documento ou resultado de um cálculo).

Vamos nos referir a qualquer coisa que possa ser representada na memória de um computador como um objeto computacional (no inglês: computacional object), ou apenas um objeto (no inglês: object). Assim, palavras, parágrafos e números podem ser objetos. E como as operações (exclusão e adição) devem estar representadas em algum lugar da memória do computador, elas também são objetos.
Normalmente, a distinção entre um “usuário” de computador e um “programador” de computador é que o usuário fornece novas entradas, ou dados (palavras ou números), enquanto o programador define novas operações, ou programas, e também novos tipos de dados. Todo objeto novo, seja um dado ou uma operação, deve ser definido em termos de objetos previamente definidos. A má notícia é que pode ser muito tedioso obter essas definições corretamente. A boa notícia e que cada novo objeto, pode, por sua vez, ser usado na definição de objetos futuros. Assim, mesmo programas complexos podem ser construídos a partir de objetos menores e mais simples. Este livro cobre uma série de problemas típicos de IA, mostrando como cada problema pode ser dividido em partes gerenciáveis, e também como cada peça pode ser descrita na linguagem de programação Common Lisp. O ideal é que os leitores aprendam o suficiente estudando esses exemplos para atacar novos problemas de IA com estilo, graça e sucesso.

Vamos considerar um exemplo simples de uma computação: encontrar a soma de dois números , digamos, 2 e 2. Se tivéssemos uma calculadora à mão, digitaríamos “2 + 2 =” e veríamos a resposta exibida. Em uma calculadora usando a notação polonesa reversa (no ingês: reverse Polish notation), teríamos que digitar “2 2 +” para ver a mesma resposta. Em Lisp, assim como na calculadora, o usuário realiza um diálogo interativo com o computador, digitando uma expressão e vendo o computador imprimir o valor dessa expressão. Esse modo interativo é diferente de muitas outras linguagens de programação que oferecem apenas um modo de lote(no inglês: batch mode), em que um programa inteiro é compilado e executado antes que qualquer saída possa ser vista.

Começamos uma calculadora de bolso, invertendo o botão liga/desliga (no inglês: on/off). O programa Lisp também deve ser iniciado, mas os detalhes variam de um computador para outro, então não posso explicar com o Lisp funcionará. Assumindo que conseguimos iniciar o Lisp, provalmente veremos algum tipo de prompt. No meu computador, o Lisp digita “`>`” para indicar que está pronto para aceitar a próxima computação. Então nos deparamos com uma tela que se parece com isso:

```lisp
> 
```
Podemos agora digitar nossa computação e ver o resultado exibido. Acontece que a convenção de Lisp para expressões aritméticas é um pouco diferente: uma computação consiste em uma lista de parênteses com o nome da operação primeiro seguido por qualquer número de operandos ou argumentos. Isso é chamado de notação de prefixo(no inglês: prefix notation).

```lisp
> (+ 2 2)
4
> 
```
Vemos que Lisp imprimiu a resposta 4, e depois outro prompt ">", para indicar que está pronto para o próximo cálculo. Ao longo deste livro, todas as expressões Lisp serão exibidas na fonte `typewriter`. O texto na mesma linha do primeiro prompt ">" é digitado pelo usuário e o texto seguinte é impresso pelo computador. Normalmente, a entrada digitada(input) pelo programador estará em letras `lowercase`, enquanto a saida(output) que é impressa pelo computador, estará em letras `UPPERCASE`. Claro, com símbolos como + e 4, não há diferença.

Para economizar espaço na página, a saída será mostrada às vezes na mesma linha da entrada, separada por uma seta (=>), que pode ser lida como "avavaliada como" (no inglês: evaluates to), e também pode ser considerada como return ou ENTER que o usuário pressiona para completar a entrada(input):

```lisp
> (+ 2 2) => 4
```
Uma vantagem da notação de prefixo entre parênteses, é que os parênteses marcam claramente o início e o fim de uma expressão. Se quisermos, podermfornecer mais de dois argumentos para `+`, e ainda adicionar todos eles:

```lisp
> (+ 1 2 3 4 5 6 7 8 9 10) => 55 
```
Desta vez tentamos (9000 + 900 + 90 + 9) - (5000 + 500 + 50 + 5):

```lisp
> (- (+ 9000  900  90  9 ) (+ 5000  500  50  5 )) => 4444
```
Este exemplo mostra que as expressões podem ser aninhadas. Os argumentos para a função `-` são listas de parênteses(no inglês: parenthesized lists), enquanto os argumentos para cada função `+` são atómos (no inglês: atoms). A notação de Lisp pode parecer incomum em comparação à notação matemática padrão, mas há vantagens nessa notação; como as expressões de Lisp podem consistir de uma função seguida por qualquer número de argumentos, não precisamos continuar repetindo `+`, mais importante que a notação é a regra para avaliação(no inglês: evaluation). Em Lisp, as listas são avaliadas primeiro avaliando todos os argumentos, e em seguida, aplciando a função ao argumentos, calculando assim o resultado. Essa regra é muito mais simples que a regra para avaliar expressões matemáticas normais, onde há muitas convenções a serem lembradas, como fazer multiplicações e divisões antes de somas e diferenças. Veremos abaixo que a regra real de avaliação Lisp é uma pouco mais complicada, mas não muito.

Às vezes, os programadores que estão familiarizados com outras linguagens têm preconceitos que dificultam o aprendizado de Lisp. Para eles, vale a pena destacar três pontos aqui. Primeiro, muitas outras linguagens fazem uma distinção entre declarações(statements) e expressões(expressions). Uma expressão, como `2 + 2`, tem um valor, mas não tem uma declaração, como `x = 2 + 2`. As declarações têm efeitos(effects), mas não retornam valores. Em Lisp, não existe tal distinção: toda expressão retorna um valor. É verdade que algumas expressões têm efeitos, mas mesmo essas expressões também retornam valore(values).

Em segundo lugar, as regras lexicais do Lisp são muito mais simples do que as regras de outras linguagens. Em particular, há menos caracteres de pontuação: apenas parênteses, marcas de aspas(quote marks, podem ser simples, dupla ou invertida), espaços(spaces), e a vírgula(comma), quais servem para separar simbolos(symbols) um do outro. Assim, enquanto a declaração `y=a*x+3` é analizada como sete tokens separados em outroas linguagens, em Lisp ela seria tratada como um único símbolo. Para obter uma lista de tokens, teríamos que inserir espaços: `(y = a * x + 3)`[1](#fn01-1)

Terceiro, enquanto muitas linguagens usam ponto e vírgula para demilitar instruções, o Lisp não precisa de ponto e vírgula, já que as expressões são demilitadas por parênteses. O Lisp escolhe usar ponto e vírgula para outro propósito: marcar o início de um comentário, que dura até o final da linha:

```lisp
> (+ 2 2) ; este é um comentário 
4
```
## Computação Simbólica

Tudo o que fizemos até agora foi manipular números da mesma maneira que uma simples calculadora de bolso faria, Lisp é mais útil que uma calculadora por dois motivos principais. Primeiro, ele nos permite manipular objetos além de números e, segundo, permite definir novos objetos que podem ser úteis em cálculcos subsequentes. vamos examinar essas duas propriedades importantes por sua vez.

Além de números, Lisp pode representar caracteres (characters), sequências de caracteres(strings) e símbolos arbitrários, onde somos livres para interpretar esses símbolos como se referindo a coisas fora do mundo da matemática. O Lisp também pode criar objetos que não são atómos(nonatomic objects) combinando vários objetos em uma lista. Esta capacidade é fundamental e bem suportada na linguagem; na verdade, o nome Lisp é abreviação de List Processing(Processamento de lista).

Aqui está um exemplo de computação em listas:

```lisp
> (append '(Pat Kim) '(Robin Sandy)) => (PAT KIM ROBIN SANDY)
```

Esta expressão acrescenta duas listas de nomes. A regra para avaliar essa expressão é igual a regra para cálculos(computação) numéricos: avalie todos os argumentos(no caso acima, '(Pat Kim) e '(Robin Sandy)) e depois aplique a função(no caso acima, append) ao valor dos argumentos.

A parte incomum é a marca de aspas (quote mark, `'()`) antes dos argumentos (Pat Kim) e (Robin Sandy), a aspas simples serve para bloquear a avaliaçao da seguinte expressão, retornando-a literalmente. Se apenas tivéssemos a expressão `(Pat Kim)`, ela seria avaliada considerando `Pat` como uma função e aplicando-a ao valor da expressão `Kim`. Não é isso que tínhamos em mente, se não existir uma função definida chamada ´Pat`, receberemos um erro. A marca de aspas instrui Lisp a tratar a lista como um fragmento de dados e não como uma chamada de função.

```lisp
> '(Pat Kim) (PAT KIM)
```

Em outras linguagens de computador, as citações geralmente vêm em pares: uma para marcar o início e outra para marcar o fim. Em Lisp, uma aspa simples é usada para marcar o começo de uma expressão. Como sempre sabemos qual é o fim da expresão, um final de um átomo ou nos parênteses correspondentes de uma lista, não precisamos de um sinal de pontuação explícito para nos informar onde a expressão termina. As citações podem ser usadas em listas, com em `(Pat kim)`, em símbolos como em `'Robin` e de fato em qualquer outra coisa, aqui estão alguns exemplos:

```lisp
> 'John => JOHN

> '(John Q Public) => (JOHN Q PUBLIC)

> '2 => 2

> 2 => 2

> '(+  2 2) => (+  2 2)

> (+  2 2) 4

> John => *Error: JOHN is not a bound variable*

> (John Q Public) => *Error: JOHN is not a function*
```
Note que `'2`avalia para `2` porque é uma expressão entre aspas, e `2` avalia para `2` porque os números avaliam a si mesmos, temos o mesmo resultado, porém, motivos diferentes. Em contraste, `'Jonh`avalia para `Jonh`porque é uma expressão precedida de aspa simples(quote), enquanto a avaliação `John` leva a um erro, porque avaliar um símbolo significa obter o valor do símbolo e nenhum valor foir atribuído a `John`.

Computações simbólicas podem ser aninhadas e até misturadas com computações numéricas. A expressão a seguir cria uma lista de nomes de uma maneira um pouco diferente do que vimos anteriormente, usando a função embutida do Common Lisp `list`. Então, vemos como encontrar o número de elementos na lista, usando a função interna `length`:

```lisp
> (append '(Pat Kim) (list '(John Q Public) 'Sandy))
(PAT KIM (JOHN Q PUBLIC) SANDY)

> (length (append '(Pat Kim) (list '(John Q Public) 'Sandy)))
4
```
Há quatro pontos importantes a serem feitos sobre os símbolos:

* Primeiro, é importante lembrar que o Lisp nãio atribui nenhum significado externo aos objetos que manipula. Por exemplo, pensamos naturalmente em `(Robin Sandy)` como uma lista de dois primeiros nomes e `(John Q Public)` como uma lista do primeiro nome de uma pessoa, inicial do meio e o sobrenome. Lisp não tem tais preconceitos. Par Lisp, ambos `Robin`e `xyzzy`são símbolos perfeitamente bons.

* Segundo, para fazer as computações acima, é preciso que haja já definido as funções `append`, `length`e `+`, que são funções já definidas por padrão no Common Lisp. Aprender uma linguagem envolve lembrar itens de vocabulário (ou saber onde procurá-los), bem como aprender as regras básicas para formar expressões e determinar o que elas significam. Common Lisp fornece mais de 700 funções embutidas(built-in functions).Em algum momento, o leito deve folhear um texto de referência para ver o que está lá, mas a maioria das funções importantes é apresentada na parte I deste livro.

* Em terceiro lugar, observe que os símbolos no Common Lisp não diferenciam maiúsculas de minúsculas. Com isso quero dizer que as entradas `John`, `john` e `jOhN` todos se referem ao mesmo símbolo, que normalmente é impresso como `JOHN`.[2](#fn01-2)

* Quarto, observe que uma grande variedade de caracteres é permitida em símbolos: números, letras e outros sinais de pontuação `+` ou `!`, as regras exatadas para o que constitui um símbolo são um pouco complicadas, mas a convenção normal é usar símbolos consistindo principalmente de letras, com palavras separadas por um `-` e talvez com um número no final. Alguns programadores são mais liberais ao nomerar variáveis e incluem caracteres como `?!$/<=>`. Por exemplo, uma função para converter dólares em iene(no inglês: yen, moeda usada no Japão) poderia ser nomeada com símbolo `$-to-yen` ou `$->yen` em Lisp, enquanto alguém usuaria algo como `DollarsToYen`, `dollars_to_yen` ou `dol2yen`em Pascal ou C. Existem algumas exceções a essas convenções de nomeclatura, que serão tratadas à medida que surgirem.

## 1.2 Variáveis

<a id="fn01-1"></a>
[1](#tfn01-1) Essa lista de símbolos não é uma instrução de atribuição Lisp legal, mas é um objeto de dados Lisp.

<a id="fn01-2"></a>
[2](#tfn01-2) A variável `*print-case*` controla como os símbolos serão impressos. Por padrão, o valor dessa variável é `:upcase`, mas pode ser alterado para `:downcase` ou `:capitalize.`

## <span style="color:red">INCOMPLETO!!!</span>