Cmdliner error output can be different if color is enabled
  $ export NO_COLOR=1
Debug data generation
Provide partial data file
  $ stanc --debug-generate-data debug.stan --debug-data-file partial_data.json | python3 -c "import sys, json; data=json.loads(sys.stdin.read()); print(len(data['y']))"
  24
  $ stanc --debug-generate-inits debug.stan --debug-data-file partial_data.json | python3 -c "import sys, json; data=json.loads(sys.stdin.read()); print(len(data['theta']))"
  29

Output file works

  $ stanc --debug-generate-data debug.stan --o output.json
  $ ls *.json
  bad.json
  incomplete_data.json
  output.json
  partial-div0.json
  partial_data.json
  $ rm output.json

Don't provide any data
  $ stanc --debug-generate-inits debug.stan
  error: Cannot evaluate expression: (((K + N) + 1) + x.1)
      ┌─ debug.stan:8:11
    7 │  parameters {
    8 │    simplex[K + N + 1 + x.1] theta;
      │            ^^^^^^^^^^^^^^^ here
    9 │  }
      = Supplying a --debug-data-file may help.
  [1]

Don't provide _enough_ data
  $ stanc --debug-generate-inits debug.stan --debug-data-file incomplete_data.json
  error: Cannot evaluate expression: (((14 + N) + 1) + x.1)
      ┌─ debug.stan:8:11
    7 │  parameters {
    8 │    simplex[K + N + 1 + x.1] theta;
      │            ^^^^^^^^^^^^^^^ here
    9 │  }
  [1]

Provide a non-existant file
  $ stanc --debug-generate-inits debug.stan --debug-data-file non_existant.json
  Usage: %%NAME%% [--help] [OPTION]… [MODEL_FILE]
  %%NAME%%: option '--debug-data-file': no 'non_existant.json' file
  [124]

Provide an invalid JSON file
  $ stanc --debug-generate-inits debug.stan --debug-data-file bad.json
  error: Failed to parse 'bad.json' for debug generation:
    Line 3, bytes 10-13:
    Expected ',' or '}' but found 'a,
    '
  [1]

Provide an unreadable JSON file
  $ touch unreadable.json
  $ chmod -r unreadable.json
  $ stanc --debug-generate-inits debug.stan --debug-data-file unreadable.json
  Usage: %%NAME%% [--help] [OPTION]… [MODEL_FILE]
  %%NAME%%: File 'unreadable.json' not found or cannot be opened.
  [124]
  $ rm unreadable.json

Bad data block, cannot be partially evaluated
  $ stanc --debug-generate-data div0.stan --debug-data-file partial-div0.json
  error: Integer division by zero
      ┌─ div0.stan:4:10
    3 │    int M;
    4 │    vector[N %/% M] x;
      │           ^^^^^^^ here
    5 │  }
  [1]
