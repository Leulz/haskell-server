module TesteReadSheet where

import ReadSheet (PessoaAdministrador(..), getNome)

-- | arquivo de teste e exemplo para utilizar o data PessoaAdministrador o readsheet.hs

test::PessoaAdministrador -> String
test pessoa = (getNome pessoa)

p = test (PessoaAdministrador {_matricula = "111", _nome = "nome", _email = "teste@teste.com", _funcao = "Tester"})