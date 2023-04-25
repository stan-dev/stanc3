Debug data generation
Provide partial data file
  $ stanc --debug-generate-data debug.stan --debug-data-file partial_data.json | jq -r ".y | length"
  24
  $ stanc --debug-generate-inits debug.stan --debug-data-file partial_data.json | jq -r ".theta | length"
  24

Don't provide any data
  $ stanc --debug-generate-inits debug.stan
  Error in 'debug.stan', line 8, column 10 to column 13:
  Cannot evaluate expression: (K + N)
  Supplying a --debug-data-file may help
