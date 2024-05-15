# Como executar
`runghc -isrc src/Main.hs`
# Compilação com Optimizações
`ghc -O3 -o Main -isrc src/Main.hs`
# Estatísticas de Execução
`env time -v ./Main < input`