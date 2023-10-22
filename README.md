# Agenda de Compromissos em Haskell

## Introdução

Este projeto, implementado em Haskell, fornece uma aplicação de console para gerenciar uma agenda de compromissos. O usuário pode interagir com um menu de opções para realizar diversas ações como adicionar, remover, editar, visualizar, e filtrar compromissos por dia ou mês. Cada compromisso mantém dados essenciais como título, descrição e uma tupla representando dia, mês, ano e hora. O software é intuitivo e serve como uma ótima base para aqueles que desejam explorar mais sobre a programação funcional e gestão de dados em Haskell.


## Estrutura de Dados

### Compromisso

A estrutura principal utilizada é a `Compromisso`, que armazena informações sobre cada evento.

```haskell
data Compromisso = Compromisso {
    titulo :: String,
    descricao :: String, 
    dataHora :: (Int, Int, Int, Int) -- (Dia, mes, Ano, Hora)
} deriving (Show, Read, Eq)
```

## Funções Principais

### Adicionar Compromisso

```haskell
adicionarCompromisso :: Compromisso -> [Compromisso] -> [Compromisso]
adicionarCompromisso compromisso agenda = compromisso : agenda 
```

### Remover Compromisso

```haskell
removerCompromisso :: String -> [Compromisso] -> [Compromisso]
removerCompromisso tit agenda = filter (\c -> tit /= titulo c) agenda
```

### Editar Compromisso

```haskell
editarCompromisso :: String -> Compromisso -> [Compromisso] -> [Compromisso]
editarCompromisso tit novoCompromisso agenda =
    case find (\c -> tit == titulo c) agenda of
        Just _ -> map (\c -> if titulo c == tit then novoCompromisso else c) agenda
        Nothing -> agenda
```

### Visualizar Compromissos

```haskell
visualizarCompromissos :: [Compromisso] -> IO ()
visualizarCompromissos agenda = mapM_ print agenda
```

### Filtrar Compromissos por Mês ou Dia

```haskell
filtrarPorMes :: Int -> [Compromisso] -> [Compromisso]
filtrarPorMes mes = filter (\(Compromisso _ _ (_, m, _, _)) -> m == mes)

filtrarPorDia :: Int -> [Compromisso] -> [Compromisso]
filtrarPorDia dia = filter (\(Compromisso _ _ (d, _, _, _)) -> d == dia)
```

## Tecnologias Utilizadas

- **Haskell**: Uma linguagem de programação puramente funcional que oferece uma expressividade muito elevada, permitindo que os programadores definam bem a lógica da aplicação de uma maneira clara e concisa.

## Uso

Ao executar o programa, o usuário é saudado e apresentado com um menu de opções, incluindo adicionar um novo compromisso, remover um compromisso existente, editar um compromisso, visualizar todos os compromissos, filtrar compromissos por dia ou mês, e sair do programa.

Cada opção guiada por prompts de console solicitará ao usuário entradas adicionais conforme necessário, como título, descrição, dia, mês, ano e hora do compromisso.

