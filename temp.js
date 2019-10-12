let str = `
@int                { mkL LInteger }
@bool               { mkL LBool }
@real               { mkL LReal }
@string             { mkL LString }

-- Types
"int"               { mkL LIntT }
"bool"              { mkL LBoolT }
"real"              { mkL LRealT }
"string"            { mkL LStringT }

-- Keywords
"return"            { mkL LReturn }
"if"                { mkL LIf }
"else"              { mkL LElse }
"while"             { mkL LWhile }
"repeat"            { mkL LRepeat }
"case"              { mkL LCase }
"for"               { mkL LFor }
"in"                { mkL LIn }
":"                 { mkL LColon }
"."                 { mkL LDot }

-- Identifiers
@ident              { mkL LIdent }

-- Syntax things
".."                { mkL LRange }
"<=="               { mkL LNBTMove }
"><"                { mkL LSwap }
"->"                { mkL LGoesTo }
"="                 { mkL LGets }
","                 { mkL LComma }
"/"                 { mkL LCommand }
"("                 { mkL LLParenth }
")"                 { mkL LRParenth }
"["                 { mkL LLBracket }
"]"                 { mkL LRBracket }
"{"                 { mkL LLBrace }
"}"                 { mkL LRBrace }
"<"                 { mkL LLAngle }
">"                 { mkL LRAngle }
"|>"                { mkL LSeqStart }
"|-"                { mkL LSeqSeries  }
"|"                 { mkL LSeqCont }
"$"                 { mkL LConstant }
"~"                 { mkL LPure }
@partSeparator      { mkL LPartSeparator }
@blockSeparator     { mkL LBlockSeparator }
@blockStarter       { mkL LColon }

-- Expression Operators
"+"                 { mkL LPlus }
"-"                 { mkL LMinus }
"/"                 { mkL LDivide }
"%"                 { mkL LModulo }
"*"                 { mkL LTimes }
"/\"                { mkL LMax }
"\/"                { mkL LMin }
"&"                 { mkL LAnd }
"|"                 { mkL LOr }
"!"                 { mkL LNot }
"<"                 { mkL LLessThan }
"<="                { mkL LLessThanOrEqual }
">"                 { mkL LGreaterThan }
">="                { mkL LGreaterThanOrEqual }
"=="                { mkL LEqual }
"!="                { mkL LNotEqual }
// `;

console.log()

const lines = str.split('\n').map(x => x.split(' '));
const types = [];
lines.forEach(line => {
  let i = 0;

	const B = { lexeme: line[0] };

  for(let x of line) {
    if(x === 'mkL') {
      const t = line[i+1];
	  B.name = t;

		types.push(B);
		return;
    }
    i++;
  }
});


types.forEach(t => {

});

// // console.log(types.join('\n                 | '));
// // console.log(types.map(x => `${x}${' '.repeat(20 - x.length)}-> return (T${x.substring(1)}${' '.repeat(20 - x.length)}p)`).join('\n                                '))
// // console.log(types.map(x => ` T${x.substring(1)}${' '.repeat(20 - x.length)}{                          position :: AlexPosn }`).join('\n           |'))
// console.log(types.map(x => ` T${x.substring(1)}${' '.repeat(20 - x.length)}{                          position :: AlexPosn }`).join('\n           |'))
