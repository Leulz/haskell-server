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

Para acessar leitor do google sheet:
- criar uma conta no site google developers - console.cloud.google.com
- ativar api do google sheets
- em IAM & Admin, criar uma conta de serviço e compartilhar a sheet com email gerado
- criar credencial, chave conta de serviço e baixar a chave 'algumnome.json'
- alterar nome da chave para 'application_default_credentials' e em seguida salvar no endereço ~/.config/gcloud/
- por fim alterar sheetId e range no arquivo ReadSheet.hs

para mais informações sobre como acessar google sheets, [link para exeplo da api](https://github.com/brendanhay/gogol/blob/develop/examples/src/Example/Sheets.hs)
