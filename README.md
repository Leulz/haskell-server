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

Templates HTML devem ser colocados no diretório static/templates.

Scripts JS devem ser colocados no diretório static/scripts e injetados no HTML através do arquivo hamlet do widget em que ele for usado. O widget é o arquivo servido pela requisição.

Por exemplo:

```
postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
```

No código acima, o widget servido é o intitulado "homepage", e portanto quaisquer componentes Angular devem ser postos no arquivo ```homepage.hamlet```, localizado na pasta ```templates```. Leitura recomendada sobre o assunto: https://www.yesodweb.com/book/scaffolding-and-the-site-template