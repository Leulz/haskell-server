Primeiro, certifique-se de que tem o haskell-stack e o bower (o bower depende de npm e node estarem disponíveis) instalados. No GNU/Linux, execute:
```
curl -sSL https://get.haskellstack.org/ | sh
npm install -g bower
```

Após isso, execute o seguinte comando para instalar dependências (somente necessrio na primeira vez ou quando for instalar novos pacotes Haskell e novos módulos bower):
```
stack install yesod-bin cabal-install --install-ghc
bower install
```
Para construir o projeto:
```
stack build
```

Por fim, para executar:
```
stack exec -- yesod devel
```

O serviço estará disponível na porta 3000.
