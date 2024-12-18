Debug data generation
Provide partial data file
  $ stanc --debug-generate-data debug.stan --debug-data-file partial_data.json | python3 -c "import sys, json; data=json.loads(sys.stdin.read()); print(len(data['y']))"
  24
  $ stanc --debug-generate-inits debug.stan --debug-data-file partial_data.json | python3 -c "import sys, json; data=json.loads(sys.stdin.read()); print(len(data['theta']))"
  29

Output file works

  $ stanc --debug-generate-data debug.stan --o output.json && ls *.json && rm output.json
  bad.json
  incomplete_data.json
  output.json
  partial-div0.json
  partial_data.json

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

Provide a non-existant file
  $ stanc --debug-generate-inits debug.stan --debug-data-file non_existant.json
  %%NAME%%: option '--debug-data-file': no 'non_existant.json' file
  Usage: %%NAME%% [OPTION]… [MODEL_FILE]
  Try '%%NAME%% --help' for more information.
  [124]

Provide an invalid file
  $ stanc --debug-generate-inits debug.stan --debug-data-file bad.json
  Error: Failed to parse data JSON for debug generation: Line 3, bytes 10-13:
  Expected ',' or '}' but found 'a,
  '
  [1]


Bad data block, cannot be partially evaluated
  $ stanc --debug-generate-data div0.stan --debug-data-file partial-div0.json
  Error in 'div0.stan', line 4, column 9 to column 16:
  Integer division by zero
  [1]
