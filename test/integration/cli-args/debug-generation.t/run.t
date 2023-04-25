Debug data generation
Provide partial data file
  $ stanc --debug-generate-data debug.stan --debug-data-file partial_data.json | python -c "import sys, json; data=json.loads(sys.stdin.read()); print(len(data['y']))"
  24
  $ stanc --debug-generate-inits debug.stan --debug-data-file partial_data.json | python -c "import sys, json; data=json.loads(sys.stdin.read()); print(len(data['theta']))"
  25
Don't provide any data
  $ stanc --debug-generate-inits debug.stan
  Error in 'debug.stan', line 8, column 10 to column 17:
  Cannot evaluate expression: ((K + N) + 1)
  Supplying a --debug-data-file may help
