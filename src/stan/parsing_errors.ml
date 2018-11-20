(* This file was auto-generated based on "src/stan/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message s =
  match s with
  | 594 -> "Expected  \";\" or \"=\" expression \";\".\n"
  | 582 -> "Expected \";\".\n"
  | 409 -> "Expected  \";\" or \"=\" expression \";\".\n"
  | 0 ->
      "Expected \"functions {\" or \"data {\" or \"transformed data {\" or \
       \"parameters {\" or \"transformed parameters {\" or \"model {\" or \
       \"generated quantities {\".\n"
  | 610 -> "We expect \"{\" after \"transfored parameters\".\n"
  | 611 -> "Expect a statement or top-level variable declaration.\n"
  | 615 ->
      "\"model {\" or \"generated quantities {\" expected after end of \
       transformed parameters block.\n"
  | 589 -> "We expect \"{\" after \"transfored data\".\n"
  | 30 -> "After \"while\", we expect \"(\" expression \")\" statement.\n"
  | 31 ->
      "Ill-formed expression. We expect an expression after \"(\" for the \
       test of the while-loop.\n"
  | 422 ->
      "Ill-formed expression. We expect an expression after \"(\" for the \
       test of the while-loop.\n"
  | 423 ->
      "Ill-formed statement. We expect a statement after \")\", for the body \
       of the while-loop.\n"
  | 221 ->
      "Ill-formed expression. We expect an expression after \"(\" for the \
       test of the while-loop.\n"
  | 222 ->
      "Ill-formed statement. We expect a statement after \")\", for the body \
       of the while-loop.\n"
  | 590 -> "Expect a statement or top-level variable declaration.\n"
  | 592 ->
      "Ill-formed top-level variable declaration. Expect an identifier next.\n"
  | 370 ->
      "Ill-formed statement or expression. A statement or expression could be \
       expected here.\n"
  | 323 ->
      "Ill-formed expression. Found L-value \"*=\". Expect an expression \
       followed by \";\" next.\n"
  | 326 ->
      "Ill-formed phrase. Found L-value \"*=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 324 ->
      "Ill-formed phrase. Found L-value \"*=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 102 ->
      "Ill-formed expression. Found expression \"*\". Expect an expression \
       next.\n"
  | 104 ->
      "Ill-formed phrase. Found expression \"*\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 103 ->
      "Ill-formed phrase. Found expression \"*\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 328 ->
      "Ill-formed \"~\"-statement. Expected identifier for distribution name \
       after \"~\".\n"
  | 329 ->
      "Ill-formed \"~\"-statement. Expected \"(\" followed by a \
       comma-separated list of expressions followed by \")\" after \
       distribution name.\n"
  | 330 ->
      "Ill-formed \"~\"-statement. Expected comma-separated list of \
       expressions followed by \")\".\n"
  | 331 -> "Ill-formed \"~\"-statement. Expected \",\" or \")\".\n"
  | 332 ->
      "Ill-formed \"~\"-statement. Expected \";\" or \"T[\" optional \
       expression \",\" optional expression \"];\".\n"
  | 333 -> "Ill-formed \"~\"-statement. Expected \";\".\n"
  | 322 ->
      "Ill-formed phrase. Found L-value. This can be completed in many ways.\n"
  | 127 ->
      "Ill-formed expression. Found expression \">\". Expect an expression \
       next.\n"
  | 129 ->
      "Ill-formed phrase. Found expression \">\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 128 ->
      "Ill-formed phrase. Found expression \">\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 158 ->
      "Ill-formed expression. Found expression \"?\". Expect an expression \
       next.\n"
  | 166 ->
      "Ill-formed expression. Found expression \"?\". Expect an expression \
       next.\n"
  | 167 ->
      "Ill-formed expression. Found expression \"?\" expression \":\". Expect \
       an expression next.\n"
  | 169 ->
      "Ill-formed phrase. Found expression \"?\" expression \":\" expression. \
       There are many ways in which this can be completed to a valid phrase.\n"
  | 168 ->
      "Ill-formed phrase. Found expression \"?\" expression \":\" expression. \
       There are many ways in which this can be completed to a valid phrase.\n"
  | 159 ->
      "Ill-formed expression. Found expression \"?\". Expect an expression \
       next.\n"
  | 160 ->
      "Ill-formed expression. Found expression \"?\" expression \":\". Expect \
       an expression next.\n"
  | 162 ->
      "Ill-formed phrase. Found expression \"?\" expression \":\" expression. \
       There are many ways in which this can be completed to a valid phrase.\n"
  | 161 ->
      "Ill-formed phrase. Found expression \"?\" expression \":\" expression. \
       There are many ways in which this can be completed to a valid phrase.\n"
  | 335 ->
      "Ill-formed expression. Found L-value \"+=\". Expect an expression \
       followed by \";\" next.\n"
  | 338 ->
      "Ill-formed phrase. Found L-value \"+=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 336 ->
      "Ill-formed phrase. Found L-value \"+=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 114 ->
      "Ill-formed expression. Found expression \"+\". Expect an expression \
       next.\n"
  | 116 ->
      "Ill-formed phrase. Found expression \"+\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 115 ->
      "Ill-formed phrase. Found expression \"+\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 163 ->
      "Ill-formed expression. Found expression \"||\". Expect an expression \
       next.\n"
  | 165 ->
      "Ill-formed phrase. Found expression \"||\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 164 ->
      "Ill-formed phrase. Found expression \"||\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 145 ->
      "Ill-formed expression. Found expression \"!=\". Expect an expression \
       next.\n"
  | 147 ->
      "Ill-formed phrase. Found expression \"!=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 146 ->
      "Ill-formed phrase. Found expression \"!=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 105 ->
      "Ill-formed expression. Found expression \"%\". Expect an expression \
       next.\n"
  | 107 ->
      "Ill-formed phrase. Found expression \"%\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 106 ->
      "Ill-formed phrase. Found expression \"%\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 340 ->
      "Ill-formed expression. Found L-value \"-=\". Expect an expression \
       followed by \";\" next.\n"
  | 343 ->
      "Ill-formed phrase. Found L-value \"-=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 341 ->
      "Ill-formed phrase. Found L-value \"-=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 117 ->
      "Ill-formed expression. Found expression \"-\". Expect an expression \
       next.\n"
  | 119 ->
      "Ill-formed phrase. Found expression \"-\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 118 ->
      "Ill-formed phrase. Found expression \"-\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 371 ->
      "Ill-formed function application. Expect comma-separated list of \
       expressions followed by \")\" after \"(\".\n"
  | 185 ->
      "Ill-formed function application. Expect comma-separated list of \
       expressions followed by \")\" after \"(\".\n"
  | 372 ->
      "Ill-formed function application. Expect comma-separated list of \
       expressions followed by \")\" after \"(\".\n"
  | 186 ->
      "Ill-formed conditional distribution evaluation. Expect comma-separated \
       list of expressions followed by \")\" after \"|\".\n"
  | 187 ->
      "Ill-formed conditional distribution evaluation. Expect comma-separated \
       list of expressions followed by \")\" after \"|\".\n"
  | 373 ->
      "Ill-formed phrase. Found a well-formed function application. After \
       this, there are many legal completions of the phrase.\n"
  | 76 ->
      "Ill-formed function application. Expect comma-separated list of \
       expressions followed by \")\" after \"(\".\n"
  | 180 ->
      "Ill-formed conditional distribution evaluation. Expect comma-separated \
       list of expressions followed by \")\" after \"|\".\n"
  | 181 ->
      "Ill-formed conditional distribution evaluation. Expect comma-separated \
       list of expressions followed by \")\" after \"|\".\n"
  | 130 ->
      "Ill-formed expression. Found expression \"<=\". Expect an expression \
       next.\n"
  | 132 ->
      "Ill-formed phrase. Found expression \"<=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 131 ->
      "Ill-formed phrase. Found expression \"<=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 92 ->
      "Ill-formed expression. Found expression \"\\\". Expect an expression \
       next.\n"
  | 94 ->
      "Ill-formed phrase. Found expression \"\\\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 93 ->
      "Ill-formed phrase. Found expression \"\\\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 59 ->
      "Ill-formed indices. Expected comma-separated set of indices followed \
       by \"]\" after \"[\".\n"
  | 133 ->
      "Ill-formed expression. Found expression \"<\". Expect an expression \
       next.\n"
  | 135 ->
      "Ill-formed phrase. Found expression \"<\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 134 ->
      "Ill-formed phrase. Found expression \"<\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 70 ->
      "Ill-formed expression. Found expression \"^\". Expect an expression \
       next.\n"
  | 72 ->
      "Ill-formed phrase. Found expression \"^\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 71 ->
      "Ill-formed phrase. Found expression \"^\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 136 ->
      "Ill-formed expression. Found expression \">=\". Expect an expression \
       next.\n"
  | 138 ->
      "Ill-formed phrase. Found expression \">=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 137 ->
      "Ill-formed phrase. Found expression \">=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 148 ->
      "Ill-formed expression. Found expression \"==\". Expect an expression \
       next.\n"
  | 150 ->
      "Ill-formed phrase. Found expression \"==\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 149 ->
      "Ill-formed phrase. Found expression \"==\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 345 ->
      "Ill-formed expression. Found L-value \".*=\". Expect an expression \
       followed by \";\" next.\n"
  | 348 ->
      "Ill-formed phrase. Found L-value \".*=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 346 ->
      "Ill-formed phrase. Found L-value \".*=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 95 ->
      "Ill-formed expression. Found expression \".*\". Expect an expression \
       next.\n"
  | 97 ->
      "Ill-formed phrase. Found expression \".*\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 96 ->
      "Ill-formed phrase. Found expression \".*\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 350 ->
      "Ill-formed expression. Found L-value \"./=\". Expect an expression \
       followed by \";\" next.\n"
  | 353 ->
      "Ill-formed phrase. Found L-value \"./=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 351 ->
      "Ill-formed phrase. Found L-value \"./=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 98 ->
      "Ill-formed expression. Found expression \"./\". Expect an expression \
       next.\n"
  | 100 ->
      "Ill-formed phrase. Found expression \"./\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 99 ->
      "Ill-formed phrase. Found expression \"./\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 355 ->
      "Ill-formed expression. Found L-value \"/=\". Expect an expression \
       followed by \";\" next.\n"
  | 358 ->
      "Ill-formed phrase. Found L-value \"/=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 356 ->
      "Ill-formed phrase. Found L-value \"/=\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 108 ->
      "Ill-formed expression. Found expression \"/\". Expect an expression \
       next.\n"
  | 110 ->
      "Ill-formed phrase. Found expression \"/\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 109 ->
      "Ill-formed phrase. Found expression \"/\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 360 ->
      "Ill-formed expression. Found L-value \"=\". Expect an expression \
       followed by \";\" next.\n"
  | 363 ->
      "Ill-formed phrase. Found L-value \"=\" expression. There are many ways \
       in which this can be completed to a valid phrase.\n"
  | 361 ->
      "Ill-formed phrase. Found L-value \"=\" expression. There are many ways \
       in which this can be completed to a valid phrase.\n"
  | 365 ->
      "Ill-formed expression. Found L-value \"<-\". Expect an expression \
       followed by \";\" next.\n"
  | 368 ->
      "Ill-formed phrase. Found L-value \"<-\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 366 ->
      "Ill-formed phrase. Found L-value \"<-\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 152 ->
      "Ill-formed expression. Found expression \"&&\". Expect an expression \
       next.\n"
  | 154 ->
      "Ill-formed phrase. Found expression \"&&\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 153 ->
      "Ill-formed phrase. Found expression \"&&\" expression. There are many \
       ways in which this can be completed to a valid phrase.\n"
  | 223 ->
      "Ill-formed phrase. Expect either \"+=\" or \"()\" after \"target\".\n"
  | 224 -> "Expected an expression followed by a \";\", after \"target +=\".\n"
  | 227 ->
      "Ill-formed expression. Expression followed by \";\" expected after \
       \"target +=\".\n"
  | 225 ->
      "Ill-formed expression. Expression followed by \";\" expected after \
       \"target +=\".\n"
  | 33 -> "Ill-formed expression. Expected \")\" after \"target(\".\n"
  | 591 -> "Expected top-level variable declaration, statement or \"}\".\n"
  | 230 -> "Expression or \";\" expected after \"return\".\n"
  | 73 ->
      "Ill-formed expression. Found identifier. There are many ways to \
       complete this to a well-formed expression.\n"
  | 234 ->
      "Ill-formed expression. Expression or \";\" expected after \"return\".\n"
  | 74 ->
      "Ill-formed expression. In function application, expect comma-separated \
       list of expressions followed by \")\", after \"(\".\n"
  | 183 ->
      "Ill-formed expression. In function application, expect comma-separated \
       list of expressions followed by \")\", after \"(\".\n"
  | 32 -> "Ill formed expression. After \"target\", we expect \"()\".\n"
  | 232 ->
      "Ill formed expression followed by \";\". Expect expression after \
       \"return\".\n"
  | 40 -> "Expression expected after \"{\" in array expression.\n"
  | 209 -> "Expected either \",\" followed by expression or \"}\" next.\n"
  | 236 ->
      "Ill-formed reject statement. After \"reject(\", we expect a comma \
       separated list of either expressions or strings, followed by \");\".\n"
  | 237 ->
      "Ill-formed reject statement. After \"reject(\", we expect a comma \
       separated list of either expressions or strings, followed by \");\".\n"
  | 246 ->
      "Ill-formed phrase. Found expression. There are many options to \
       continue the phrase after this.\n"
  | 241 ->
      "Ill-formed reject statement. After \"reject(\", we expect a comma \
       separated list of either expressions or strings, followed by \");\".\n"
  | 243 ->
      "Ill-formed printable. After \"print(\" and \"reject(\", we expect a \
       comma separated list of either expressions or strings, followed by \
       \");\".\n"
  | 244 ->
      "Ill-formed printable. After \"print(\" and \"reject(\", we expect a \
       comma separated list of either expressions or strings, followed by \
       \");\".\n"
  | 240 ->
      "Ill-formed printable. After \"reject(\", we expect a comma separated \
       list of either expressions or strings, followed by \");\".\n"
  | 245 ->
      "Ill-formed phrase. Found expression. There are many options to \
       continue the phrase after this.\n"
  | 304 ->
      "Ill-formed phrase. Found expression. There are many options to \
       continue the phrase after this: \"~\", \"[\", \"]\" or an infix or \
       postfix operator.\n"
  | 51 ->
      "Ill-formed expression. Found expression \"*\" expression. Expect an \
       expression next.\n"
  | 204 ->
      "Ill-formed phrase. Found expression \"*\" expression. There are many \
       options to continue the phrase after this.\n"
  | 52 ->
      "Ill-formed phrase. Found expression \"*\" expression. There are many \
       options to continue the phrase after this.\n"
  | 305 ->
      "Ill-formed \"~\"-statement. Expect an distribution name after \"~\".\n"
  | 306 ->
      "Ill-formed \"~\"-statement. Expect \"(\" after distribution name, \
       followed by a comma separated list of expressions for arguments to the \
       distribution, followed by \")\".\n"
  | 307 ->
      "Ill-formed \"~\"-statement. Expect a comma separated list of \
       expressions for arguments to the distribution, followed by \")\".\n"
  | 308 ->
      "Ill-formed \"~\"-statement. Expect a comma separated list of \
       expressions for arguments to the distribution, followed by \")\".\n"
  | 309 ->
      "Ill-formed \"~\"-statement. Expect either \";\" or a truncation with \
       the format \"T[\" optional expression \",\" optional expression \"];\".\n"
  | 310 ->
      "Ill-formed truncation. Expect the format \"T[\" optional expression \
       \",\" optional expression \"];\".\n"
  | 311 ->
      "Ill-formed truncation. Expect the format \"T[\" optional expression \
       \",\" optional expression \"];\".\n"
  | 317 ->
      "Ill-formed expression. Found an expression. Expect a infix or postfix \
       operator or \"[\" or \"]\" or \",\" next.\n"
  | 312 ->
      "Ill-formed truncation. Expect the format \"T[\" optional expression \
       \",\" optional expression \"];\".\n"
  | 316 ->
      "Ill-formed expression. Found an expression. Expect a infix or postfix \
       operator or \"[\" or \"]\" or \",\" next.\n"
  | 313 ->
      "Ill-formed truncation. Expect the format \"T[\" optional expression \
       \",\" optional expression \"];\".\n"
  | 314 ->
      "Ill-formed truncation. Expect the format \"T[\" optional expression \
       \",\" optional expression \"];\".\n"
  | 319 -> "Expected \";\" after \"~\"-statement (with optional truncation).\n"
  | 61 ->
      "Ill-formed expression. Found expression \">\". Expect an expression \
       next.\n"
  | 191 ->
      "Ill-formed phrase. Found expression \">\" expression. There are many \
       options to continue the phrase after this.\n"
  | 62 ->
      "Ill-formed phrase. Found expression \">\" expression. There are many \
       options to continue the phrase after this.\n"
  | 77 ->
      "Ill-formed expression. Found expression \"?\". Expect an expression \
       next followed by \":\" expression.\n"
  | 170 ->
      "Ill-formed expression. Found expression \"?\" expression. Expect an \
       infix or postfix operator or \"[\" or \":\" next.\n"
  | 171 ->
      "Ill-formed expression. Found expression \"?\" expression \":\". Expect \
       an expression next.\n"
  | 173 ->
      "Ill-formed phrase. Found expression \"?\" expression \":\" expression. \
       There are many options to continue the phrase after this.\n"
  | 172 ->
      "Ill-formed phrase. Found expression \"?\" expression \":\" expression. \
       There are many options to continue the phrase after this.\n"
  | 78 ->
      "Ill-formed expression. Found expression \"?\" expression. Expect an \
       infix or postfix operator or \"[\" or \":\" next.\n"
  | 155 ->
      "Ill-formed expression. Found expression \"?\" expression \":\". Expect \
       an expression next.\n"
  | 157 ->
      "Ill-formed phrase. Found expression \"?\" expression \":\" expression. \
       There are many options to continue the phrase after this.\n"
  | 156 ->
      "Ill-formed phrase. Found expression \"?\" expression \":\" expression. \
       There are many options to continue the phrase after this.\n"
  | 63 ->
      "Ill-formed expression. Found expression \"+\". Expect an expression \
       next.\n"
  | 190 ->
      "Ill-formed phrase. Found expression \"+\" expression. There are many \
       options to continue the phrase after this.\n"
  | 64 ->
      "Ill-formed phrase. Found expression \"+\" expression. There are many \
       options to continue the phrase after this.\n"
  | 79 ->
      "Ill-formed expression. Found expression \"||\". Expect an expression \
       next.\n"
  | 151 ->
      "Ill-formed phrase. Found expression \"||\" expression. There are many \
       options to continue the phrase after this.\n"
  | 80 ->
      "Ill-formed phrase. Found expression \"||\" expression. There are many \
       options to continue the phrase after this.\n"
  | 81 ->
      "Ill-formed expression. Found expression \"!=\". Expect an expression \
       next.\n"
  | 126 ->
      "Ill-formed phrase. Found expression \"!=\" expression. There are many \
       options to continue the phrase after this.\n"
  | 82 ->
      "Ill-formed phrase. Found expression \"!=\" expression. There are many \
       options to continue the phrase after this.\n"
  | 65 ->
      "Ill-formed expression. Found expression \"%\". Expect an expression \
       next.\n"
  | 189 ->
      "Ill-formed phrase. Found expression \"%\" expression. There are many \
       options to continue the phrase after this.\n"
  | 66 ->
      "Ill-formed phrase. Found expression \"%\" expression. There are many \
       options to continue the phrase after this.\n"
  | 83 ->
      "Ill-formed expression. Found expression \"-\". Expect an expression \
       next.\n"
  | 101 ->
      "Ill-formed phrase. Found expression \"-\" expression. There are many \
       options to continue the phrase after this.\n"
  | 84 ->
      "Ill-formed phrase. Found expression \"-\" expression. There are many \
       options to continue the phrase after this.\n"
  | 111 ->
      "Ill-formed expression. Found expression \"<=\". Expect an expression \
       next.\n"
  | 113 ->
      "Ill-formed phrase. Found expression \"<=\" expression. There are many \
       options to continue the phrase after this.\n"
  | 112 ->
      "Ill-formed phrase. Found expression \"<=\" expression. There are many \
       options to continue the phrase after this.\n"
  | 53 ->
      "Ill-formed expression. Found expression \"\\\". Expect an expression \
       next.\n"
  | 203 ->
      "Ill-formed phrase. Found expression \"/\" expression. There are many \
       options to continue the phrase after this.\n"
  | 54 ->
      "Ill-formed phrase. Found expression \"/\" expression. There are many \
       options to continue the phrase after this.\n"
  | 48 ->
      "Found expression \"[\". Expect comma separated list of indices next.\n"
  | 195 ->
      "Ill-formed phrase. Found expression. There are many options to \
       continue the phrase after this.\n"
  | 196 ->
      "Ill-formed phrase. Found expression \":\". Expect \"]\", \",\" or \
       expression next.\n"
  | 198 ->
      "Ill-formed phrase. Found expression \":\" expression. There are many \
       options to continue the phrase after this: \"[\", \"]\", \",\", \":\", \
       an infix operator, a postfix operator.\n"
  | 197 ->
      "Ill-formed phrase. Found expression \":\" expression. There are many \
       options to continue the phrase after this: \"[\", \"]\", \",\", \":\", \
       an infix operator, a postfix operator.\n"
  | 60 ->
      "Ill-formed phrase. Found expression. There are many options to \
       continue the phrase after this: \"[\", \"]\", \",\", \":\", an infix \
       operator, a postfix operator.\n"
  | 192 ->
      "Ill-formed expression. Expected expression or \"]\" or \",\" after \
       \":\".\n"
  | 194 ->
      "Ill-formed phrase. Found expression \"<\" expression. There are many \
       options to continue the phrase after this.\n"
  | 193 ->
      "Ill-formed phrase. Found expression \":\" expression. There are many \
       options to continue the phrase after this.\n"
  | 201 -> "Expected index after indices followed by \",\".\n"
  | 49 ->
      "Ill-formed expression. Expected expression or \"]\" or \",\" after \
       \":\".\n"
  | 205 ->
      "Ill-formed phrase. Found \":\" expression. We expect either an infix \
       or postfix operator, or \",\" or or \"[\" or \"]\" next.\n"
  | 50 ->
      "Ill-formed phrase. Found \":\" expression. We expect either an infix \
       or postfix operator, or \",\" or or \"[\" or \"]\" next.\n"
  | 120 ->
      "Ill-formed expression. Expected expression after expression followed \
       by \"<\".\n"
  | 122 ->
      "Ill-formed phrase. Found expression \"<\" expression. There are many \
       options to continue the phrase after this.\n"
  | 121 ->
      "Ill-formed phrase. Found expression \"<\" expression. There are many \
       options to continue the phrase after this.\n"
  | 55 ->
      "Ill-formed expression. Expected expression after expression followed \
       by \"^\".\n"
  | 57 ->
      "Ill-formed phrase. Found expression \"^\" expression. There are many \
       options to continue the phrase after this.\n"
  | 56 ->
      "Ill-formed phrase. Found expression \"^\" expression. There are many \
       options to continue the phrase after this.\n"
  | 123 ->
      "Ill-formed expression. Expected expression after expression followed \
       by \">=\".\n"
  | 125 ->
      "Ill-formed phrase. Found expression \">=\" expression. There are many \
       options to continue the phrase after this.\n"
  | 124 ->
      "Ill-formed phrase. Found expression \">=\" expression. There are many \
       options to continue the phrase after this.\n"
  | 139 ->
      "Ill-formed expression. Expected expression after expression followed \
       by \"==\".\n"
  | 141 ->
      "Ill-formed phrase. Found expression \"==\" expression. There are many \
       options to continue the phrase after this.\n"
  | 140 ->
      "Ill-formed phrase. Found expression \"==\" expression. There are many \
       options to continue the phrase after this.\n"
  | 67 ->
      "Ill-formed expression. Expected expression after expression followed \
       by \".*\".\n"
  | 69 ->
      "Ill-formed phrase. Found expression \".*\" expression. There are many \
       options to continue the phrase after this.\n"
  | 68 ->
      "Ill-formed phrase. Found expression \".*\" expression. There are many \
       options to continue the phrase after this.\n"
  | 85 ->
      "Ill-formed expression. Expected expression after expression followed \
       by \"./\".\n"
  | 87 ->
      "Ill-formed phrase. Found expression \"./\" expression. There are many \
       options to continue the phrase after this.\n"
  | 86 ->
      "Ill-formed phrase. Found expression \"./\" expression. There are many \
       options to continue the phrase after this.\n"
  | 89 ->
      "Ill-formed expression. Expected expression after expression followed \
       by \"/\".\n"
  | 91 ->
      "Ill-formed phrase. Found expression \"/\" expression. There are many \
       options to continue the phrase after this.\n"
  | 90 ->
      "Ill-formed phrase. Found expression \"/\" expression. There are many \
       options to continue the phrase after this.\n"
  | 142 ->
      "Ill-formed expression. Expected expression after expression followed \
       by \"&&\".\n"
  | 144 ->
      "Ill-formed phrase. Found expression \"&&\" expression. There are many \
       options to continue the phrase after this.\n"
  | 143 ->
      "Ill-formed phrase. Found expression \"&&\" expression. There are many \
       options to continue the phrase after this.\n"
  | 593 ->
      "\";\" or plain assignment is expected after a top-level variable \
       declaration.\n"
  | 603 ->
      "Expected \"parameters {\", \"transformed parameters {\", \"model {\", \
       \"generated quantities {\" or end-of-file after end of transformed \
       data block.\n"
  | 247 ->
      "Expected \"(\" followed by a comma-separated list of expressions or \
       strings followed by \");\" after \"print\".\n"
  | 248 ->
      "Expected a comma-separated list of expressions or strings followed by \
       \");\" after \"print(\".\n"
  | 250 -> "Expected a \";\" after \"print(...)\".\n"
  | 249 ->
      "Expected a comma-separated list of expressions or strings followed by \
       \");\" after \"print(\".\n"
  | 36 -> "Ill-formed expression. Expect an expression after \"+\".\n"
  | 220 ->
      "Ill-formed expression. Found an expression. Expect an infix or postfix \
       operator or \"[\" or \"~\" or \"]\" or \";\".\n"
  | 219 ->
      "Ill-formed expression. Found an expression. Expect an infix or postfix \
       operator or \"[\" or \"~\" or \"]\" or \";\".\n"
  | 37 -> "Ill-formed expression. Expect an expression after \"-\".\n"
  | 218 ->
      "Ill-formed expression. Found an expression. Expect an infix or postfix \
       operator or \"[\" or \"~\" or \"]\" or \";\".\n"
  | 217 ->
      "Ill-formed expression. Found an expression. Expect an infix or postfix \
       operator or \"[\"\n"
  | 38 -> "Expression expected after \"(\".\n"
  | 215 ->
      "Ill-formed phrase. Found \"(\" followed by expression. Expect a \"[\", \
       \",\" or \")\" or an infix or postfix operator. \n"
  | 213 ->
      "Expression or range of expressions followed by \")\" expected after \
       \"(\".\n"
  | 39 ->
      "Comma separated list of expressions followed by \"]\" expected after \
       \"[\".\n"
  | 177 ->
      "Ill-formed phrase. We found an L-value. Parse failed on token after \
       the L-value.\n"
  | 211 ->
      "Ill-formed expression. We expect a comma separated list of \
       expressions, followed by \"]\".\n"
  | 178 ->
      "Ill-formed expression. We expect a comma separated list of expressions.\n"
  | 176 ->
      "Ill-formed phrase. Found an expression. This can be followed by a \
       \",\", a \"}\", a \")\", a \"]\", a \"[\" or an infix or postfix \
       operator.\n"
  | 252 ->
      "Ill-formed phrase. \"{\" should be followed by a statement, variable \
       declaration or expression.\n"
  | 420 ->
      "Ill-formed phrase. Found an L-value. This can be followed by a \"~\", \
       a \",\", a \"}\", a \"[\" or an infix or postfix operator or an \
       assignment operator.\n"
  | 417 ->
      "Ill-formed phrase. Found an expression. This can be followed by a \
       \"~\", a \",\", a \"}\", a \"[\" or an infix or postfix operator.\n"
  | 174 ->
      "Ill-formed expression. Expected a comma-separated list of expressions.\n"
  | 281 ->
      "Ill-formed statement. Expected \"(\" followed by an expression and \
       \");\", after \"increment_log_prob\".\n"
  | 282 ->
      "Ill-formed statement. Expected expression followed by \");\" after \
       \"(\".\n"
  | 286 ->
      "Ill-formed statement. Expected expression followed by \");\" after \
       \"(\".\n"
  | 287 -> "Ill-formed statement. Expected \";\" after \")\".\n"
  | 283 ->
      "Ill-formed statement. Expected expression followed by \");\" after \
       \"(\".\n"
  | 284 -> "Ill-formed statement. Expected \";\" after \")\".\n"
  | 289 -> "\"(\" expression \")\" expected after \"if\".\n"
  | 290 ->
      "Expected expression for test of conditional control flow construct.\n"
  | 396 ->
      "Ill-formed expression. Expression expected after \"(\", for test of \
       conditional control flow construct.\n"
  | 397 ->
      "Ill-formed statement. Expected statement after \")\" for true branch \
       of conditional.\n"
  | 398 ->
      "Ill-formed statement. Expected \"else\" or statement or variable \
       declaration or \"}\" after conditional true branch.\n"
  | 399 -> "Ill-formed statement. Expected statement after else.\n"
  | 291 ->
      "Expected \")\" after test expression of conditional control flow \
       construct.\n"
  | 292 ->
      "Ill-formed statement. Statement expected for true branch of \
       conditional. \n"
  | 393 ->
      "Ill-formed statement. Expected \"else\"  or statement or variable \
       declaration or \"}\".\n"
  | 394 -> "Ill-formed statement. Expected statement after \"else\".\n"
  | 42 -> "Expected \"()\" after \"get_lp\".\n"
  | 43 -> "Expected \")\" after \"get_lp(\".\n"
  | 293 -> "Expected \"(\" after \"for\".\n"
  | 294 -> "Expected (loop) identifier after \"(\".\n"
  | 295 -> "Expected \"in\" after loop identifier.\n"
  | 296 ->
      "Ill-formed expression. Expected expression followed by \")\" or \":\" \
       after \"for (\" identifier \"in\".\n"
  | 383 ->
      "Ill-formed expression. Expected expression followed by \")\" or \":\" \
       after \"for (\" identifier \"in\".\n"
  | 384 ->
      "Ill-formed statement. Expected statement after \")\" for the loop body \
       of the foreach loop.\n"
  | 386 ->
      "Ill-formed expression. Expected expression followed by \")\" after \
       \"for (\" identifier \"in\" expression \":\".\n"
  | 390 ->
      "Ill-formed expression. Expected expression followed by \")\" after \
       \"for (\" identifier \"in\" expression \":\".\n"
  | 391 ->
      "Ill-formed statement. Expected statement after \")\" for the loop body \
       of the for loop.\n"
  | 387 ->
      "Ill-formed expression. Expected expression followed by \")\" after \
       \"for (\" identifier \"in\" expression \":\".\n"
  | 388 ->
      "Ill-formed statement. Expected statement after \")\" for the loop body \
       of the for loop.\n"
  | 297 ->
      "Ill-formed expression. Expected expression after \"for (\" identifier \
       \"in\".\n"
  | 298 ->
      "Ill-formed statement. Expected statement after \")\" for the loop body \
       of the foreach loop.\n"
  | 376 ->
      "Ill-formed expression. Expected expression followed by \")\" after \
       \"for (\" identifier \"in\" expression \":\".\n"
  | 380 ->
      "Ill-formed expression. Expected expression followed by \")\" after \
       \"for (\" identifier \"in\" expression \":\".\n"
  | 381 ->
      "Ill-formed statement. Expected statement after \")\"  for the loop \
       body of the for loop.\n"
  | 377 ->
      "Ill-formed expression. Expected expression followed by \")\" after \
       \"for (\" identifier \"in\" expression \":\".\n"
  | 378 ->
      "Ill-formed statement. Expected statement after \")\"  for the loop \
       body of the for loop..\n"
  | 299 -> "Expected \";\" after \"continue\". \n"
  | 301 -> "Expected \";\" after \"break\". \n"
  | 45 -> "Ill-formed expression. Expression expected after \"!\".\n"
  | 208 -> "Ill-formed expression. Expression expected after \"!\".\n"
  | 46 -> "Ill-formed expression. Expression expected after \"!\". \n"
  | 604 -> "Expected \"{\" after \"parameters\".\n"
  | 605 -> "Expected top-level variable declaration or \"}\".\n"
  | 609 ->
      "\"transformed parameters {\", \"model {\" or \"generated quantities \
       {\" expected after end of parameters block.\n"
  | 616 -> "Expected \"{\" after \"model\".\n"
  | 617 -> "Variable declaration, statement or \"}\" expected.\n"
  | 253 ->
      "\"[\" expression  \"]\" expected after \"vector\" in local (or model \
       block) variable declaration. (No transformations/constraints allowed.)\n"
  | 254 -> "\"[\" expression \"]\" expected for vector sizes.\n"
  | 257 -> "\"[\" expression \"]\" expected for vector sizes.\n"
  | 255 -> "\"[\" expression \"]\" expected for vector sizes.\n"
  | 401 -> "Variable declaration, statement or \"}\" expected.\n"
  | 259 ->
      "\"[\" expression\"]\" expected after \"row_vector\" in local (or model \
       block) variable declaration. (No transformations/constraints allowed.)\n"
  | 260 -> "\"[\" expression \"]\" expected for row_vector sizes.\n"
  | 263 -> "\"[\" expression \"]\" expected for row_vector sizes.\n"
  | 261 -> "\"[\" expression \"]\" expected for row_vector sizes.\n"
  | 404 ->
      "Identifier expected after sized type in local (or model block) \
       variable declaration. (No transformations/constraints allowed.)\n"
  | 405 -> "\";\" or plain assignment expected after variable declaration.\n"
  | 406 ->
      "Ill-formed array sizes. \"[\" (non-empty comma separated list of \
       expressions) \"]\" expected to specify array sizes.\n"
  | 407 ->
      "Ill-formed array sizes. \"[\" (non-empty comma separated list of \
       expressions) \"]\" expected to specify array sizes.\n"
  | 410 ->
      "Ill-formed expression. Expression followed by \";\" expected after \
       \"=\".\n"
  | 412 ->
      "Ill-formed expression. Expression followed by \";\" expected after \
       \"=\".\n"
  | 411 ->
      "Ill-formed expression. Expression followed by \";\" expected after \
       \"=\".\n"
  | 620 ->
      "Expected \"generated quantities {\" or end of file after end of model \
       block.\n"
  | 266 ->
      "\"[\" expression \",\" expression \"]\" expected after \"matrix\" in \
       local (or model block) variable declaration. (No \
       transformations/constraints allowed.)\n"
  | 267 ->
      "\"[\" expression \",\" expression \"]\" expected for matrix sizes.\n"
  | 274 ->
      "\"[\" expression \",\" expression \"]\" expected for matrix sizes.\n"
  | 275 ->
      "\"[\" expression \",\" expression \"]\" expected for matrix sizes.\n"
  | 278 ->
      "\"[\" expression \",\" expression \"]\" expected for matrix sizes.\n"
  | 276 ->
      "\"[\" expression \",\" expression \"]\" expected for matrix sizes.\n"
  | 268 ->
      "\"[\" expression \",\" expression \"]\" expected for matrix sizes.\n"
  | 269 ->
      "\"[\" expression \",\" expression \"]\" expected for matrix sizes.\n"
  | 272 ->
      "\"[\" expression \",\" expression \"]\" expected for matrix sizes.\n"
  | 270 ->
      "\"[\" expression \",\" expression \"]\" expected for matrix sizes.\n"
  | 621 -> "Expected \"{\" after block keyword.\n"
  | 622 ->
      "Variable declaration or statement or \"}\" expected in generated \
       quantities block.\n"
  | 625 -> "Expected end of file after end of generated quantities block.\n"
  | 1 -> "\"{\" expected after \"functions\".\n"
  | 2 ->
      "Function forward declaration, definition or \"}\" expected after \
       \"functions {\".\n"
  | 10 -> "An identifier is expected as a function name.\n"
  | 13 -> "\"(\" expected after function name.\n"
  | 14 -> "(Non-void) type expected function argument declaration.\n"
  | 18 -> "An identifier is expected as a function argument name.\n"
  | 426 -> "\",\" or \")\" expected after function argument declaration.\n"
  | 427 ->
      "An argument declaration (unsized type followed by identifier) is \
       expected.\n"
  | 29 ->
      "Either \"{\" statement \"}\" is expected for a function definition or \
       \";\" for a function forward declaration.\n"
  | 17 -> "An identifier is expected as a function argument name.\n"
  | 431 -> "A \"}\" or a function definition/declaration is expected.\n"
  | 20 ->
      "Either a number of unsized dimensions is expected as part of a \
       function return type or an identifier is expected as a function name.\n"
  | 21 ->
      "\"[\" (list of commas) \"]\" expected in unsized return type of \
       function definition.\n"
  | 22 -> "List of commas expected.\n"
  | 434 ->
      "Expected \"data {\" or \"transformed data {\" or \"parameters {\" or \
       \"transformed parameters {\" or \"model {\" or \"generated quantities \
       {\".\n"
  | 435 ->
      "\"{\" followed by a list of top-level variable declarations is \
       expected after seeing \"data\".\n"
  | 436 -> "Expected top-level variable declaration or \"}\".\n"
  | 437 -> "\"[\" expression \"]\" expected for vector size.\n"
  | 494 -> "\"[\" expression \"]\" expected for vector size.\n"
  | 497 -> "\"[\" expression \"]\" expected for vector size.\n"
  | 495 -> "\"[\" expression \"]\" expected for vector size.\n"
  | 580 -> "We expect to see an identifier after a sized type.\n"
  | 438 ->
      "We expect to see \"lower =\", \"upper =\", \"location =\" or \"scale \
       =\" followed by an expression after \"<\". \n"
  | 439 -> "Expect \"=\" expression \">\" after seeing \"upper\".\n"
  | 440 ->
      "Expression (not containing binary logical operators) expected after \
       \"upper =\". Ill-formed expression.\n"
  | 444 ->
      "Expression expected. Ill-formed expression. Suggested alternatives: a \
       standalone identifier, a function application, an identifier followed \
       by an operator or an identifier followed by an index.\n"
  | 456 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 457 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 464 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 465 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 466 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 467 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 470 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 471 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 458 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 459 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 447 -> "Ill-formed expression. Expected expression followed by \"]\".\n"
  | 450 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 451 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 460 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 461 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 462 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 463 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 468 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 469 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 455 ->
      "Expected expression (not containing binary logical operators) \">\" \
       after \"upper =\".\n"
  | 441 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 454 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 442 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 453 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 443 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 445 ->
      "Ill-formed expression. Expected expression (not containing binary \
       logical operators).\n"
  | 472 ->
      "Expected \"=\" expression (not containing binary logical operators) \
       \">\" after \"scale\".\n"
  | 473 ->
      "Expected expression (not containing binary logical operators) after \
       \"scale =\".\n"
  | 474 -> "Expected \">\" after \"scale =\" expression.\n"
  | 493 -> "Expected \"[\" expression \"]\" for vector size.\n"
  | 475 ->
      "Expected \"=\" expression (not containing binary logical operators), \
       after \"lower\".\n"
  | 476 ->
      "An expression (not containing binary logical operators) is expected \
       for type lower bound.\n"
  | 478 ->
      "\"upper =\" expression (not containing binary logical operators) \">\" \
       expected after \"<lower =\" expression \",\" in top-level variable \
       declaration.\n"
  | 479 ->
      "Expected \"=\" expression (not containing binary logical operators) \
       after \"upper\".\n"
  | 480 ->
      "Expected expression (not containing binary logical operators) after \
       \"upper = \".\n"
  | 481 -> "Expected \">\" after \"upper = \" expression.\n"
  | 477 ->
      "\">\" or \", upper = expression (not containing binary logical \
       operators) \">\" expected after specifying lower bound for type.\n"
  | 482 ->
      "Expected \"location = \" expression (not containing binary logical \
       operators).\n"
  | 483 ->
      "Expected \"location = \" expression (not containing binary logical \
       operators).\n"
  | 485 ->
      "Expected \">\" or \", scale = \" expression (not containing binary \
       logical operators) \">\" after \"location = \" expression.\n"
  | 486 ->
      "Expected \"scale = \" expression (not containing binary logical \
       operators).\n"
  | 487 ->
      "Expected \"scale = \" expression (not containing binary logical \
       operators).\n"
  | 488 -> "Expected \">\" after \"scale = \" expression.\n"
  | 484 ->
      "Expected \"location = \" expression (not containing binary logical \
       operators).\n"
  | 500 ->
      "Expected \"[\" expression \"]\" for size declaration of unit_vector.\n"
  | 501 ->
      "Expected \"[\" expression \"]\" for size declaration of unit_vector.\n"
  | 504 ->
      "Expected \"[\" expression \"]\" for size declaration of unit_vector.\n"
  | 502 ->
      "Expected \"[\" expression \"]\" for size declaration of unit_vector.\n"
  | 506 -> "Expected \"[\" expression \"]\" for size declaration of simplex.\n"
  | 507 -> "Expected \"[\" expression \"]\" for size declaration of simplex.\n"
  | 510 -> "Expected \"[\" expression \"]\" for size declaration of simplex.\n"
  | 508 -> "Expected \"[\" expression \"]\" for size declaration of simplex.\n"
  | 512 ->
      "Expected \"[\" expression \"]\" for size declaration of row_vector.\n"
  | 514 ->
      "Expected \"[\" expression \"]\" for size declaration of row_vector.\n"
  | 517 ->
      "Expected \"[\" expression \"]\" for size declaration of row_vector.\n"
  | 515 ->
      "Expected \"[\" expression \"]\" for size declaration of row_vector.\n"
  | 513 -> "Expected identifier as part of top-level variable declaration.\n"
  | 519 ->
      "Identifier expected after type in top-level variable declaration.\n"
  | 581 ->
      "Only top-level variable declarations are allowed in data and parameter \
       blocks.\n"
  | 584 ->
      "Only top-level variable declarations allowed in data and parameters \
       blocks.\n"
  | 588 ->
      "Expected \"transformed data {\" or \"parameters {\" or \"transformed \
       parameters {\" or \"model {\" or \"generated quantities {\".\n"
  | 521 -> "Expected \"[\" expression \"]\" for size of positive_ordered.\n"
  | 522 -> "Expected \"[\" expression \"]\" for size of positive_ordered.\n"
  | 525 -> "Expected \"[\" expression \"]\" for size of positive_ordered.\n"
  | 523 -> "Expected \"[\" expression \"]\" for size of positive_ordered.\n"
  | 527 -> "Expected \"[\" expression \"]\" for size of ordered.\n"
  | 528 -> "Expected \"[\" expression \"]\" for size of ordered.\n"
  | 531 -> "Expected \"[\" expression \"]\" for size of ordered.\n"
  | 529 -> "Expected \"[\" expression \"]\" for size of ordered.\n"
  | 533 ->
      "Expected \"[\" expression \",\" expression \"]\" for sizes of matrix.\n"
  | 535 ->
      "Expected \"[\" expression \",\" expression \"]\" for sizes of matrix.\n"
  | 542 ->
      "Expected \"[\" expression \",\" expression \"]\" for sizes of matrix.\n"
  | 543 ->
      "Expected \"[\" expression \",\" expression \"]\" for sizes of matrix.\n"
  | 546 ->
      "Expected \"[\" expression \",\" expression \"]\" for sizes of matrix.\n"
  | 544 ->
      "Expected \"[\" expression \",\" expression \"]\" for sizes of matrix.\n"
  | 536 ->
      "Expected \"[\" expression \",\" expression \"]\" for sizes of matrix.\n"
  | 537 ->
      "Expected \"[\" expression \",\" expression \"]\" for sizes of matrix.\n"
  | 540 ->
      "Expected \"[\" expression \",\" expression \"]\" for sizes of matrix.\n"
  | 538 ->
      "Expected \"[\" expression \",\" expression \"]\" for sizes of matrix.\n"
  | 534 ->
      "Expected \"[\" expression \",\" expression \"]\" for matrix sizes as \
       part of top-level variable declaration.\n"
  | 548 ->
      "Expected range constraint or identifier as part of top-level variable \
       declaration.\n"
  | 549 ->
      "Expected \"lower = \" expression or \"upper = \" expression for \
       integer bounds.\n"
  | 551 -> "Expected \"[\" expression \"]\" for size of cov_matrix.\n"
  | 552 -> "Expected \"[\" expression \"]\" for size of cov_matrix.\n"
  | 555 -> "Expected \"[\" expression \"]\" for size of cov_matrix.\n"
  | 553 -> "Expected \"[\" expression \"]\" for size of cov_matrix.\n"
  | 557 -> "Expected \"[\" expression \"]\" for size of corr_matrix.\n"
  | 558 -> "Expected \"[\" expression \"]\" for size of corr_matrix.\n"
  | 561 -> "Expected \"[\" expression \"]\" for size of corr_matrix.\n"
  | 559 -> "Expected \"[\" expression \"]\" for size of corr_matrix.\n"
  | 563 ->
      "Expected \"[\" expression \"]\" or \"[\" expression \",\" expression \
       \"]\" for size of cholesky_factor_cov.\n"
  | 564 ->
      "Expected \"[\" expression \"]\" or \"[\" expression \",\" expression \
       \"]\" for size of cholesky_factor_cov.\n"
  | 571 ->
      "Expected \"[\" expression \"]\" or \"[\" expression \",\" expression \
       \"]\" for size of cholesky_factor_cov.\n"
  | 566 ->
      "Expected comma separated list of expressions. Ill-formed expression.\n"
  | 568 ->
      "Expected comma separated list of expressions. Ill-formed expression.\n"
  | 567 ->
      "Expected comma separated list of expressions. Ill-formed expression.\n"
  | 565 ->
      "Expected \"[\" expression \"]\" or \"[\" expression \",\" expression \
       \"]\" for size of cholesky_factor_cov.\n"
  | 574 ->
      "Expected \"[\" expression \"]\" for size of cholesky_factor_corr.\n"
  | 575 ->
      "Expected \"[\" expression \"]\" for size of cholesky_factor_corr.\n"
  | 578 ->
      "Expected \"[\" expression \"]\" for size of cholesky_factor_corr.\n"
  | 576 ->
      "Expected \"[\" expression \"]\" for size of cholesky_factor_corr.\n"
  | 632 ->
      "Expected \"functions {\" or \"data {\" or \"transformed data {\" or \
       \"parameters {\" or \"transformed parameters {\" or \"model {\" or \
       \"generated quantities {\".\n"
  | _ -> raise Not_found
