Para definir o sandbox e instalar as dependências:
```
cabal sandbox init
cabal install --only-dependencies
```
Para executar:
```
cabal exec runhaskell server.hs
```
