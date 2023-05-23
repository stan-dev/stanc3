Debug data generation
Provide partial data file
  $ stanc --debug-generate-data debug.stan --debug-data-file partial_data.json | python3 -c "import sys, json; data=json.loads(sys.stdin.read()); print(len(data['y']))"
  24
  $ stanc --debug-generate-inits debug.stan --debug-data-file partial_data.json | python3 -c "import sys, json; data=json.loads(sys.stdin.read()); print(len(data['theta']))"
  29

Don't provide any data
  $ stanc --debug-generate-inits debug.stan
  Error in 'debug.stan', line 8, column 10 to column 25:
  Cannot evaluate expression: (((K + N) + 1) + x.1)
  Supplying a --debug-data-file may help
  [1]

Don't provide _enough_ data
  $ stanc --debug-generate-inits debug.stan --debug-data-file incomplete_data.json
  Error in 'debug.stan', line 8, column 10 to column 25:
  Cannot evaluate expression: (((14 + N) + 1) + x.1)
  [1]
