# Prefácio

> **paradigm** *n* **1** an example or pattern; *esp* an outstandingly clear or typical example. 
> -*Longman's Dictionary of the English Language*, 1984

Em português:

> **paradigma** *n* **1** um exemplo ou padrão; *esp* um exemplo excepcionalmente claro ou típico.
> -*Longman's Dicionário do idioma inglês*, 1984

Este livro trata de três tópicos relacionados: o campo de inteligência artificial, ou IA, a habilidade de programação de computadores,
e a linguagem de programação Commmon Lisp.

Os leitores atentos deste livro podem esperar obter uma apreciação das principais questões e técnicas de inteligência artificial, uma
compreensão de alguns programas importantes da IA e uma capacidade de ler, modificar e criar programas usando o Common Lisp.

Os exemplos deste livro foram criados para serem exemplos claros de bons paradigmas de estilo de programação.
Eles também são paradigmas de programas historicamente significativos de pesquisa em IA que usam técnicas amplamente aplicáveis para resolver problemas importantes.

<a id="tfnpreface-1"></a>
Assim como a educação em artes liberais inclui um curso nos "grandes livros" de uma cultura, este livro é, em um nível, um curso nos "grandes programas" que definem a cultura da IA.[1](#fnpreface-1)

Em outro nível, este livro é um compêndio altamente técnico do conhecimento que você precisará para passar de um programador intermediário do Lisp para um especialista.
As partes I e II foram projetadas para ajudar o novato a se atualizar, mas o iniciante completo pode ter dificuldade mesmo com este material.
Felizmente, você poderá ver minhas recomendações no tópico *Como usar este livro*.

Com muita freqüência, o ensino da programação de computadores consiste em explicar a sintaxe do idioma escolhido, mostrar ao aluno um programa de 10 linhas e depois pedir ao aluno que escreva programas.
Neste livro, adotamos a abordagem de que a melhor maneira de aprender a escrever é ler (e, inversamente, uma boa maneira de melhorar as habilidades de leitura é escrever).
Após uma breve introdução ao Lisp, começamos imediatamente com programas complexos e pedimos ao leitor que entenda e faça pequenas modificações nesses programas.

A premissa deste livro é que você só pode escrever algo útil e interessante quando entende o que faz uma boa escrita ser considerada boa e quando tem algo interessante a dizer.
Isso vale tanto para escrever programas quanto para escrever em prosa.
Como Kernighan e Plauger colocaram na capa de *Software Tools in Pascal:*

> A boa programação não é aprendida com generalidades, mas vendo como programas significativos podem ser limpos, fáceis de ler, fáceis de manter e modificar, projetados por humanos, eficientes e confiáveis, pela aplicação de bom senso e boas práticas de programação.
O estudo cuidadoso e a imitação de bons programas levam a uma melhor escrita.

O artesão orgulhoso é muitas vezes tentado a exibir apenas o trabalho finalizado, sem nenhuma indicação de falsos começos e erros que são uma parte infeliz, mas inevitável, do processo criativo.
Infelizmente, essa relutância em desvendar o processo é uma barreira ao aprendizado; um estudante de matemática que vê uma bela prova de 10 linhas em um livro pode se maravilhar com sua concisão, mas não aprende como construí-la.
Este livro tenta mostrar o processo completo de programação, "verrugas e tudo". Cada capítulo começa com uma versão simples de um programa, que funciona em alguns exemplos, mas falha em outros.
Cada capítulo mostra como essas falhas podem ser analisadas para criar versões cada vez mais sofisticadas do programa básico.
Assim, o leitor pode não apenas apreciar o resultado final, mas também ver como aprender com os erros e refinar um design inicialmente incompleto.
Além disso, o leitor que acha que um capítulo em particular está se tornando muito difícil pode pular para o próximo capítulo, tendo apreciado a área problemática, pode optar por não ficar sobrecarregado com os detalhes.

Este livro apresenta um corpo de conhecimento vagamente conhecido como "técnicas de programação de IA", mas deve-se reconhecer que não há limites bem definidos para esse corpo de conhecimento.
Certamente, ninguém pode ser um bom programador de IA sem antes ser um bom programador.
Portanto, este livro apresenta tópicos (especialmente nas partes III e V) que não são IA propriamente dita, mas são antecedentes essenciais para qualquer praticante de IA.

## Porque Lisp? Porque Common Lisp?

O Lisp é uma das linguagens de programação mais antigas ainda hoje amplamente utilizadas.
Existem muitas versões do Lisp, cada uma compartilhando recursos básicos, mas diferindo em detalhes.
Neste livro, usamos a versão chamada Common Lisp, que é o padrão mais amplamente aceito.
Lisp foi escolhido por três razões.

Primeiro, o Lisp é a linguagem mais popular para a programação de IA, principalmente nos Estados Unidos.
Se você estiver aprendendo um idioma, pode ser um com uma literatura crescente, em vez de uma língua morta.

Segundo, o Lisp facilita a captura de generalizações relevantes na definição de novos objetos.
Em particular, o Lisp facilita a definição de novos idiomas, especialmente direcionados ao problema em questão.
Isso é especialmente útil em aplicativos de IA, que geralmente manipulam informações complexas que são mais facilmente representadas de alguma forma nova.
Lisp é uma das poucas linguagens que permite total flexibilidade na definição e manipulação de programas, bem como de dados.
Todas as linguagens de programação, por definição, fornecem um meio de definir programas, mas muitas outras linguagens limitam as maneiras pelas quais um programa pode ser usado, ou limitam o intervalo de programas que podem ser definidos, ou exigem que o programador declare explicitamente detalhes irrelevantes.

Terceiro, o Lisp facilita muito o desenvolvimento rápido de um programa de trabalho.
Os programas Lisp são concisos e organizados por detalhes de baixo nível.
O Common Lisp oferece um número extraordinariamente grande de objetos predefinidos úteis, incluindo mais de 700 funções.
O ambiente de programação (como ferramentas de depuração, compiladores incrementais, editores integrados e interfaces para sistemas de janelas) que cercam os sistemas Lisp geralmente são muito bons.
E a natureza dinâmica e interativa do Lisp facilita a experimentação e alteração de um programa enquanto ele está sendo desenvolvido.

Deve-se mencionar que na Europa e no Japão, o Prolog tem sido tão popular quanto o Lisp para trabalhos de IA.
O Prolog compartilha a maioria das vantagens da Lisp em termos de flexibilidade e concisão.
Recentemente, a Lisp ganhou popularidade em todo o mundo, e o Prolog está se tornando mais conhecido nos Estados Unidos.
Como resultado, o trabalhador médio de IA atualmente é provavelmente bilíngue.
Este livro apresenta as idéias principais por trás do Prolog nos [capítulos 11](chapter11) e [12](chapter12) e usa essas idéias nos capítulos seguintes, particularmente [20](chapter20) e [21](chapter21).

O dialeto do Lisp, conhecido como Scheme, também está ganhando popularidade, mas principalmente para ensinar e experimentar o design e as técnicas da linguagem de programação, e não tanto para escrever grandes programas de IA.
O esquema é apresentado nos [capítulos 22](chapter22) e [23](chapter23).

Outros dialetos do Lisp, como Franz Lisp, MacLisp, InterLisp, ZetaLisp e Standard Lisp agora são considerados obsoletos.
O único novo dialeto do Lisp proposto recentemente é o EuLisp, o Lisp europeu.
Alguns dialetos do Lisp vivem como idiomas de extensão incorporados.
Por exemplo, o editor de texto do Gnu Emacs usa elisp e o pacote de design auxiliado por computador do AutoCad usa o AutoLisp, um derivado do Xlisp.
No futuro, é provável que o Scheme se torne uma linguagem de extensão popular, pois é pequena, mas poderosa e possui uma definição padrão oficialmente sancionada.

Existe um mito de que o Lisp (e o Prolog) são linguagens de "propósito especial", enquanto idiomas como Pascal e C são "de propósito geral". Na verdade, apenas o contrário é verdadeiro.
Pascal e C são linguagens de propósito especial para manipular os registros e a memória de um computador no estilo von Neumann.
A maioria de sua sintaxe é dedicada a expressões aritméticas e booleanas e, embora ofereçam alguns recursos para a formação de estruturas de dados, eles possuem mecanismos deficientes para abstração processual ou abstração de controle.
Além disso, eles são projetados para o estilo de programação orientado ao estado: computando um resultado por alteração do valor das variáveis por meio de instruções de atribuição.

Lisp, por outro lado, não possui sintaxe especial para aritmética.
Adição e multiplicação não são mais ou menos básicas do que operações de lista, como anexar, ou operações de cadeia, como converter para maiúsculas.
Mas o Lisp fornece tudo o que você precisa para programar em geral: definindo estruturas de dados, funções e os meios para combiná-los.

O estilo de programação orientado a estado e dominado por atribuição é possível no Lisp, mas além disso, estilos orientados a objetos, baseados em regras e funcionais são todos suportados no Lisp.
Essa flexibilidade deriva de dois recursos principais do Lisp: Primeiro, o Lisp possui um poderoso recurso *macro*, que pode ser usado para estender a linguagem básica.

Quando novos estilos de programação foram inventados, outras linguagens desapareceram; Lisp simplesmente incorporou os novos estilos, definindo algumas novas macros.
O recurso de macro é possível porque os programas Lisp são compostos de uma estrutura de dados simples: a lista.
Nos primeiros dias, quando o Lisp era interpretado, a maioria das manipulações de programas era feita através dessa estrutura de dados.
Atualmente, o Lisp é mais frequentemente compilado do que interpretado, e os programadores confiam mais no segundo grande recurso flexível do Lisp: a *function* (função).
Obviamente, outras linguagens têm funções, mas o Lisp é raro ao permitir a criação de novas funções enquanto um programa está em execução.

A flexibilidade do Lisp permite que ele se adapte conforme os estilos de programação mudam, mas o mais importante é que o Lisp pode se adaptar ao seu problema de programação específico.
Em outros idiomas, você ajusta seu problema ao idioma; Com o Lisp, você estende o idioma para se adequar ao seu problema.

Devido à sua flexibilidade, o Lisp foi bem-sucedido como uma linguagem de alto nível para prototipagem rápida em áreas como IA, gráficos e interfaces de usuário.
O Lisp também tem sido a linguagem dominante na programação exploratória, onde os problemas são tão complexos que nenhuma solução clara está disponível no início do projeto.
Grande parte da IA se enquadra nesse cabeçalho.

O tamanho do Common Lisp pode ser uma vantagem ou uma desvantagem, dependendo da sua perspectiva.
No bom livro de David Touretzky (1989) para programadores iniciantes, a ênfase está na simplicidade.
Ele escolhe escrever alguns programas de forma menos concisa, em vez de introduzir um novo recurso esotérico (ele cita o `pushnew` como exemplo).

Essa abordagem é inteiramente apropriada para iniciantes, mas este livro vai muito além do nível de iniciantes.
Isso significa expor o leitor a novos recursos do idioma sempre que apropriado.
Na maioria das vezes, novos recursos são descritos à medida que são introduzidos, mas às vezes a explicação dos detalhes de uma função de baixo nível prejudicaria a explicação do funcionamento de um programa.
Ao aceitar o privilégio de ser tratado como um "adulto", o leitor também aceita a responsabilidade de procurar termos desconhecidos em uma fonte de referência apropriada.

## As partes deste livro

Este livro está organizado em cinco partes.

A **Parte I** apresenta a linguagem de programação Common Lisp.

[Capítulo 1](chapter1) fornece uma rápida introdução por meio de pequenos exemplos que demonstram os novos recursos do Lisp.
Pode ser pulado ou visto rapidamente com segurança pelo programador experiente.

[Capítulo 2](chapter2) é um exemplo mais extenso, mostrando como as primitivas Lisp podem ser reunidas para formar um programa.
Ele deve ser estudado cuidadosamente pelo iniciante, e mesmo o programador experiente vai querer examiná-lo para ter uma ideia do meu estilo de programação.

[Capítulo 3](chapter3) fornece uma visão geral das primitivas do Lisp.
Ele pode ser lido na primeira leitura e usado como referência sempre que uma função desconhecida for mencionada no texto.

A Parte I foi mantida intencionalmente breve, para que haja mais espaço para a apresentação de programas reais de IA.
Infelizmente, isso significa que outro texto ou livro de referência (ou ajuda on-line) pode ser necessário para esclarecer alguns dos recursos mais esotéricos do idioma.
Você pode encontrar minhas recomendações para textos no tópico mais abaixo *Como usar este livro*.

O leitor também pode consultar o [capítulo 25](chapter25), que oferece algumas dicas de depuração e solução de problemas.

A **Parte II** abrange quatro programas iniciais de IA que usam técnicas de correspondência de padrões baseadas em regras.
Iniciando com versões relativamente simples dos programas que ao serem melhorados vão se tornando programas mais complexos, o leitor pode gradualmente adquirir habilidades de programação cada vez mais avançadas.

[Capítulo 4](chapter4) apresenta uma reconstrução do GPS, o General Problem Solver.
A implementação segue a abordagem STRIPS.

[Capítulo 5](chapter5) descreve o ELIZA, um programa que imita o diálogo humano.
Isto é seguido por um capítulo que generaliza algumas das técnicas usadas no GPS e no ELIZA e as disponibiliza como ferramentas para uso em programas subseqüentes.

[Capítulo 7](chapter7) abrange o STUDENT, um programa que resolve problemas de palavras de álgebra no ensino médio.

[Capítulo 8](chapter8) desenvolve um pequeno subconjunto do programa MACSYMA para fazer álgebra simbólica, incluindo cálculo diferencial e integral.
Pode ser ignorado por aqueles que fogem da matemática pesada.

A **Parte III** desvia da AI por um momento para apresentar algumas ferramentas gerais para uma programação mais eficiente.
O leitor que domina o material desta parte pode ser considerado um programador avançado do Lisp.

[Capítulo 9](chapter9) é um estudo detalhado das técnicas de eficiência, concentrando-se no armazenamento em cache, indexação (indexing), compilação (compilation) e atraso (delaying) na computação.
O [Capítulo 10](chapter10) aborda questões de eficiência de nível inferior, como usar declarações, evitar a geração de lixo e escolher a estrutura de dados correta.

[Capítulo 11](chapter11) apresenta a linguagem Prolog.
O objetivo é duplo: mostrar como escrever um intérprete para outro idioma e introduzir os recursos importantes do Prolog, para que possam ser usados quando apropriado.
[Capítulo 12](chapter12) mostra como um compilador para o Prolog pode ser 20 a 200 vezes mais rápido que o intérprete.

[Capítulo 13](chapter13) apresenta a programação orientada a objetos em geral, depois explora o Common Lisp Object System (CLOS).

[Capítulo 14](chapter14) discute as vantagens e limitações da programação orientada a lógica e orientada a objetos e desenvolve um formalismo de representação do conhecimento usando todas as técnicas da parte III.

A **Parte IV** abrange alguns programas avançados de IA.

[Capítulo 15](chapter15) usa as técnicas da parte III para criar uma implementação muito mais eficiente do MACSYMA. Ele usa a idéia de uma forma canônica e substitui a abordagem de regra de reescrita muito geral por uma série de funções mais específicas.

[Capítulo 16](chapter16) abrange o shell do sistema especialista EMYCIN, um sistema baseado em regras de encadeamento reverso baseado em fatores de certeza.
O sistema médico especialista MYCIN também é coberto brevemente.

[Capítulo 17](chapter17) aborda o algoritmo de marcação de linha Waltz para poliedros (usando rótulos Huffman-Clowes).
Diferentes abordagens para propagação de restrição e retorno são discutidas.

[Capítulo 18](chapter18) apresenta um programa que reproduz um excelente jogo de Othello.
A técnica usada, a pesquisa alfa-beta, é apropriada para uma ampla variedade de jogos para duas pessoas.

[Capítulo 19](chapter19) é uma introdução ao processamento de linguagem natural.
Ele abrange gramática livre de contexto, análise de cima para baixo e de baixo para cima, análise de gráficos e algumas interpretações e preferências semânticas.

[Capítulo 20](chapter20) estende a cobertura linguística do capítulo anterior e introduz gramáticas lógicas, usando o compilador Prolog desenvolvido no [capítulo 11](chapter11).

[Capítulo 21](chapter21) é uma gramática bastante abrangente do inglês usando o formalismo da gramática lógica.
Os problemas de passar de uma idéia simples para um programa realista e abrangente são discutidos.


**Parte V** inclui material periférico à IA, mas importante para qualquer programador Lisp sério.

[Capítulo 22](chapter22) apresenta o dialeto Scheme de Lisp.
Um simples intérprete de Scheme é desenvolvido, um intérprete recursivo de cauda (tail-recursive interpreter), um intérprete que manipula explicitamente as continuações e suporta `call/cc`.
[Capítulo 23](chapter23) apresenta um compilador de Scheme.

[Capítulo 24](chapter24) apresenta os recursos exclusivos do Common Lisp do American National Standards Institute (ANSI).
Isso inclui a macro `loop`, bem como tratamento de erros, impressão bonita, séries e seqüências, e o recurso de pacotes.

[Capítulo 25](chapter25) é um guia para solução de problemas e depuração de programas Lisp.

A bibliografia lista mais de 200 fontes, e há um índice abrangente.
Além disso, o apêndice fornece um diretório de programas Lisp disponíveis ao público.

## Como usar este livro

O público-alvo deste livro é amplo: qualquer pessoa que queira se tornar um programador avançado de Lisp e qualquer pessoa que queira ser um praticante avançado de IA.
Existem vários caminhos recomendados no livro:

* *Em um curso introdutório de IA:* Concentre-se nas partes I e II e pelo menos um exemplo da parte IV.

* *Em um curso avançado de programação de IA:* Concentre-se nas partes I, II e IV, pulando os capítulos de menor interesse e adicionando a maior parte da parte III, conforme o tempo permitir.

* *Em um curso avançado de idiomas de programação:* Concentre-se nas partes I e V, com as seleções da parte III.
Cubra os capítulos [11](chapter11) e [13](chapter13) se material semelhante não for apresentado com outro texto.

* *Para o programador profissional de Lisp:* Leia o máximo possível do livro e consulte-o com frequência.
A parte III e [capítulo 25](chapter25) são particularmente importantes.

## Textos complementares e livros de referência

A fonte de referência definitiva é *Common Lisp the Language* de Steele.
De 1984 a 1990, isso definiu inequivocamente a linguagem Common Lisp.
No entanto, em 1990, a imagem ficou mais complicada com a publicação do *Common Lisp the Language*, 2ª edição.
Este livro, também de Steele, contém as recomendações do subcomitê ANSI X3J13, cujo termo de abertura é definir um padrão para o Lisp.
Essas recomendações incluem muitas pequenas alterações e esclarecimentos, além de material novo em programação orientada a objetos, tratamento de condições de erro e macro de loop.
O novo material dobra o tamanho do livro de 465 para 1029 páginas.

Até que as recomendações da ANSI sejam formalmente aceitas, os usuários do Common Lisp estão na situação infeliz de ter dois padrões distintos e incompatíveis: Common Lisp "original" e ANSI Common Lisp.
A maior parte do código deste livro é compatível com os dois padrões.
O uso mais significativo de uma função ANSI é a macro `loop`.
As funções ANSI `map-into`,` complement` e `reduce` também são usadas, embora raramente.
As definições para todas essas funções estão incluídas, portanto, mesmo aqueles que usam um sistema Common Lisp "original" ainda podem executar todo o código do livro.

Embora *Common Lisp the Language* seja o padrão definitivo, às vezes é conciso e pode ser difícil para um iniciante.
*Common Lisp: the Reference*, publicado pela Franz Inc., oferece cobertura completa do idioma com muitos exemplos úteis.
*Common LISPcraft*, de Robert Wilensky, e *Artificial Intelligence Programming*, de Charniak et al., Também incluem breves resumos das funções do Common Lisp.
Eles não são tão abrangentes, mas isso pode ser uma bênção, porque pode levar o leitor mais diretamente às funções importantes (pelo menos aos olhos do autor).

É uma boa idéia ler este livro com um computador em mãos, experimentar os exemplos e experimentar seus próprios exemplos.
Um computador também é útil porque o Lisp é auto-documentado, através das funções `apropos`,` describe` e `documentation`.
Muitas implementações também fornecem documentação mais extensa através de algum tipo de comando ou menu de 'ajuda' (help).

Os cinco livros introdutórios de Lisp que eu recomendo estão listados abaixo.
O primeiro é mais elementar que os outros.

* *Common Lisp: A Gentle Introduction to Symbolic Computation* por David Touretzky.
Mais apropriado para iniciantes, incluindo aqueles que não são cientistas da computação.

* *A Programmer's Guide to Common Lisp*  de Deborah G. Tatar.
Apropriado para aqueles com experiência em outra linguagem de programação, mas nenhuma no Lisp.

* *Common LISPcraft* de Robert Wilensky.
Mais abrangente e com ritmo mais rápido, mas ainda útil como introdução e também como referência.

* *Common Lisp* de Wade L. Hennessey.
Um pouco desorganizado em termos de tópicos, mas com uma discussão esclarecida sobre questões de implementação e eficiência que não aparecem nos outros textos.

* *LISP* (3d edition) de Patrick H. Winston e Bertold Horn.
Abrange o maior terreno em termos de conselhos de programação, mas não tão abrangente quanto uma referência.
Pode ser difícil para iniciantes.
Inclui alguns exemplos de IA.

Embora possa ser uma distração para o iniciante a busca contínua de alguma fonte de referência, a alternativa - fazer com que este livro explique cada nova função em detalhes completos à medida que é apresentada - seria ainda mais perturbadora.
Interromperia a descrição dos programas de IA, que é a essência deste livro.

Existem alguns textos que mostram como escrever programas e ferramentas de IA, mas nenhum que seja aprofundado como neste livro.
No entanto, o programador especialista em IA desejará estar familiarizado com todos os seguintes textos, listados em uma ordem aproximada de crescente sofisticação:

* *LISP* (3d edition).
(Veja acima.)

* *Programming Paradigms in Lisp* por Rajeev Sangal.
Apresenta os diferentes estilos de programação que o Lisp acomoda, ilustrando-os com algumas ferramentas úteis de IA.

* *Programming for Artificial Intelligence* por Wolfgang Kreutzer e Bruce McKenzie.
Abrange alguns dos conceitos básicos de sistemas baseados em regras e de correspondência de padrões, mas abrange Lisp, Prolog e Smalltalk e, portanto, não resta tempo para detalhes em nenhum dos idiomas.

* *Artificial Intelligence Programming* (2ª edição) de Eugene Charniak, Christopher Riesbeck, Drew McDermott e James Meehan.
Contém 150 páginas da visão geral do Lisp, seguidas de uma discussão avançada sobre ferramentas de IA, mas nenhum programa de IA real.

* *AI in Practice: Examples in Pop-11* de Allan Ramsey e Rosalind Barrett.
Implementações avançadas e de alta qualidade de cinco programas de IA, infelizmente usando um idioma que não ganhou popularidade.

O texto atual combina as virtudes das duas últimas entradas: apresenta os programas reais de IA e as ferramentas necessárias para construí-los.
Além disso, a apresentação é incremental, com versões simples apresentadas primeiro por questões de clareza, seguidas por versões mais sofisticadas para completude.

## Uma nota sobre exercícios

Exemplos de exercícios são fornecidos por toda parte.
Os leitores podem testar seu nível de entendimento fazendo fielmente os exercícios.
Os exercícios são classificados nas escalas [s], [m], [h], [d], que podem ser interpretadas como um nível de dificuldade ou como um tempo esperado para o exercício:

| Cod  | Dificuldade | Tempo para fazer |
|------|-------------|------------------|
| [s]  | Simple      | Seconds          |
| [m]  | Medium      | Minutes          |
| [h]  | Hard        | Hours            |
| [d]  | Difficult   | Days             |

O tempo para fazer o exercício é medido a partir do ponto em que os conceitos foram bem compreendidos.
Se o leitor não estiver claro sobre os conceitos subjacentes, poderá levar horas de revisão para entender um [m] problema.
As respostas para os exercícios podem ser encontradas em uma seção separada no final de cada capítulo.

## Agradecimentos

Muitas pessoas contribuíram para este livro.
Antes de mais, gostaria de agradecer aos meus alunos da USC e Berkeley, bem como aos alunos de James Martin no Colorado e aos alunos de Michael Pazzani em Irvine, que testaram as versões anteriores deste livro.
Sugestões, correções e adições úteis foram feitas por:

Nina Amenta (Berkeley), Ray S.
Babcock and John Paxton (Montana State), Bryan A.
Bentz (BBN), Mary P.
Boelk (Johnson Controls), Michael Braverman (Berkeley), R.
Chandrasekar and M.
Sasikumar (National Centre for Software Technology, Bombay), Mike Clancy (Berkeley), Michael Covington (Georgia), Bruce D'Ambrosio (Oregon State), Piew Datta (Irvine), Shawn Dettrey (USC), J.
A.
Durieux (AI Engineering BV, Amsterdam), Joseph Faletti (ETS), Paul Fuqua (Texas Instruments), Robert Goldman (Tulane), Marty Hall (Johns Hopkins), Marti Hearst (Berkeley), Jim Hendler (Maryland), Phil Laird (NASA), Raymond Lang (Tulane), David D.
Loeffler (MCC), George Luger (New Mexico), Rob MacLachlan (CMU), Barry Margolin (Thinking Machines), James Mayfield (UMBC), Sanjay Manchandi (Arizona), Robert McCartney (Connecticut), James Meehan (DEC), Andrew L.
Ressler, Robert S.
Rist (University of Technology, Sydney), Paul Snively (Apple), Peter Van Roy (Berkeley), David Gumby Wallace (Cygnus), and Jeff Wu (Colorado).

Sam Dooley e Eric Wefald escreveram programas para tocar Otelo sem os quais eu não teria escrito [capítulo 18](chapter18).
Eric também me mostrou as citações de Aristóteles na análise de meios-fins.
Tragicamente, Eric morreu em agosto de 1989.
Ele sente muita falta de seus amigos e colegas.
Richard Fateman fez sugestões para o [capítulo 8](chapter8), me convenceu a escrever [capítulo 15](chapter15) e, com a ajuda de Peter Klier, escreveu um programa substancial a partir do qual adaptei algum código para esse capítulo.
Charley Cox (Franz Inc.), Jamie Zawinski (Lucid Inc.) e Paul Fuqua (Texas Instruments) explicaram o funcionamento interno dos compiladores de suas respectivas empresas.
Mike Harrison, Paul Hilfinger, Marc Luria, Ethan Munson e Stephan Slade ajudaram com o LATEX.
Narciso Jarimillo testou todo o código e o separou nos arquivos que estão disponíveis para o leitor.

Durante a redação deste livro, fui apoiado por uma bolsa da Agência de Projetos de Pesquisa Avançada de Defesa (DoD), Arpa Order No.
4871, monitorado pelo Comando dos Sistemas Espaciais e Naval Warfare sob o Contrato N00039-84-C-0089.
Um agradecimento especial à DARPA e a Robert Wilensky e ao resto de meus colegas e alunos em Berkeley por fornecer um ambiente estimulante para pesquisa, programação e redação.

Finalmente, obrigado a Mike Morgan e Yonie Overton por supervisionarem a produção do livro e me incentivarem a terminar a tempo.

----------------------

 <a id="fnpreface-1"></a>
[1](#tfnpreface-1) Isso não implica que os programas escolhidos sejam os melhores de todos os programas de IA - apenas que eles estão representativos.
