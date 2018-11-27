dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
menhir $dir/../lib/parser.mly --compare-errors $dir/../lib/parser_new.messages --compare-errors $dir/../lib/parser.messages 