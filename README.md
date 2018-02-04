Primeiro, certifique-se de que tem o haskell-stack e o bower (o bower depende de npm e node estarem disponíveis) instalados. No GNU/Linux, execute:
```
curl -sSL https://get.haskellstack.org/ | sh
npm install -g bower
```

Após isso, execute os seguintes comandos para instalar dependências e construir o projeto:
```
stack install yesod-bin cabal-install --install-ghc
stack build
bower install
```

Por fim, para executar:
```
stack exec -- yesod devel
```

O serviço estará disponível na porta 3000.
