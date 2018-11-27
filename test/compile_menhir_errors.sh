dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
menhir --compile-errors $dir/../lib/parser.messages $dir/../lib/parser.mly > $dir/../lib/parsing_errors.ml