dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
menhir --list-errors $dir/../lib/parser.mly > $dir/../lib/parser_new.messages