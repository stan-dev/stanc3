dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
menhir $dir/../lib/parser.mly --update-errors $dir/../lib/parser.messages > $dir/../lib/parser_updated.messages