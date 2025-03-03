// c4_annotated.c - A minimal self-hosting C compiler implemented in four functions

/*Overview:
This code implements a very small C compiler with four main functions for lexical analysis (tokenizing the input), parsing expressions and statements, 
generating intermediate code (a sequence of opcodes and operands), and executing that code using a simple virtual machine.
It is designed to support a limited subset of C (basic types, control flow, function calls, and a few operators) in order to allow self-compilation.
The design is intentionally compact, with all functionality integrated into one file.
*/
// Written by Robert Swierczek (original) and annotated here to explain every
// function, code block, and non-trivial line.

#include <stdio.h>    // Standard I/O functions (printf, etc.)
#include <stdlib.h>   // Standard library functions (malloc, free, exit, etc.)
#include <memory.h>   // Memory manipulation functions (memset, memcmp, etc.)
#include <unistd.h>   // POSIX API (read, close, etc.)
#include <fcntl.h>    // File control options (open, etc.)

// Force all int types to be 64-bit. This simplifies the compiler by having a uniform integer size.
#define int long long

// Global pointers and variables used throughout the compiler for managing source code,
// emitted code, the symbol table, and runtime flags.
char *p, *lp;     // p: current position in the source code; lp: pointer to the start of the current line.
char *data;       // Data area pointer (for global variables, string literals, etc.)

// Pointers for code generation and symbol management.
int *e, *le;      // e: pointer to the current position in the emitted code array; le: pointer to the end of emitted code.
int *id, *sym;    // id: pointer to the current identifier in the symbol table; sym: the symbol table (an array storing identifiers).
int tk;           // Current token (numeric code representing the token kind).
int ival;         // Integer value of a numeric token or character literal.
int ty;           // Current expression type (e.g., INT, CHAR, or pointer type represented by additional levels).
int loc;          // Offset for local variables (used to index local storage in function calls).
int line;         // Current line number of the source file (for error reporting).
int src;          // Flag to indicate whether to print the source code and assembly (debug output).
int debug;        // Flag to enable detailed debug output during virtual machine execution.

// Define token kinds and operator classes in an enum. Starting from 128 leaves room for literal character tokens.
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,  // Tokens: numeric constant, function, system call, global, local, identifier
  Char, Else, Enum, If, Int, Return, Sizeof, While, // Keywords for data types and control flow constructs
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak // Operators
};

// Define opcodes for the virtual machine. Each opcode corresponds to a low-level operation.
enum {
  LEA, IMM, JMP, JSR, BZ, BNZ, ENT, ADJ, LEV,
  LI, LC, SI, SC, PSH,
  OR, XOR, AND, EQ, NE, LT, GT, LE, GE, SHL, SHR,
  ADD, SUB, MUL, DIV, MOD,
  OPEN, READ, CLOS, PRTF, MALC, FREE, MSET, MCMP, EXIT
};

// Define basic data types (CHAR, INT) and a pointer type marker (PTR).
enum { CHAR, INT, PTR };

// Symbol table offsets for storing information about each identifier. Since no struct is used,
// the symbol table is a simple array where each identifier occupies a fixed number of entries.
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

/*
Function: next()

Purpose:
   Performs lexical analysis by scanning the source code (pointed to by 'p')
   and extracting the next token. It sets the global variable 'tk' to indicate
   what kind of token was found (number, identifier, operator, etc.), and fills in
   related globals like 'ival' for numbers.

 Why it is written this way:
   The function is designed to be as compact as possible, reflecting the overall minimalism
   of the compiler. It uses simple loops and conditions to handle different token types.

 How it fits into the compiler:
   This is the lexer; it provides the stream of tokens that the parser (expr() and stmt())
   will use to build the intermediate representation.
*/
void next() {
  char *pp; // Temporary pointer for processing identifiers and strings.
  // Loop until a token is found or the end-of-file is reached.
  while (tk = *p) {
    ++p;  // Advance the source pointer.
    // If newline: update the line counter and optionally print source/debug info.
    if (tk == '\n') {
      if (src) {
        // Print the current line number and source segment.
        printf("%d: %.*s", line, (int)(p - lp), lp);
        lp = p;  // Reset line pointer to current position.
        // Optionally, print emitted opcodes (debugging view).
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ)
            printf(" %d\n", *++le);
          else
            printf("\n");
        }
      }
      ++line;  // Increment the line number.
    }
    // Skip preprocessor directives or comments starting with '#'
    else if (tk == '#') {
      while (*p != 0 && *p != '\n') ++p;
    }
    // Identifier: must start with a letter or underscore.
    else if ((tk >= 'a' && tk <= 'z') ||
             (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1;  // Remember start of the identifier.
      // Continue while characters are alphanumeric or underscore.
      while ((*p >= 'a' && *p <= 'z') ||
             (*p >= 'A' && *p <= 'Z') ||
             (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;  // Compute a hash for the identifier.
      tk = (tk << 6) + (p - pp);  // Combine the hash with the identifier length.
      // Search the symbol table for an existing identifier with this hash and name.
      id = sym;
      while (id[Tk]) {
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) {
          tk = id[Tk];  // Found an existing identifier.
          return;
        }
        id = id + Idsz;  // Move to next symbol.
      }
      // If not found, add the new identifier to the symbol table.
      id[Name] = (int)pp;  // Store pointer to the identifier's name.
      id[Hash] = tk;       // Store the computed hash.
      tk = id[Tk] = Id;    // Mark token type as identifier.
      return;
    }
    // Numeric literal: handles decimal, hexadecimal, or octal numbers.
    else if (tk >= '0' && tk <= '9') {
      if (ival = tk - '0') {
        // Process decimal number.
        while (*p >= '0' && *p <= '9')
          ival = ival * 10 + *p++ - '0';
      }
      else if (*p == 'x' || *p == 'X') {
        // Process hexadecimal literal.
        while ((tk = *++p) &&
               ((tk >= '0' && tk <= '9') ||
                (tk >= 'a' && tk <= 'f') ||
                (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      else {
        // Process octal literal.
        while (*p >= '0' && *p <= '7')
          ival = ival * 8 + *p++ - '0';
      }
      tk = Num; // Set token type to numeric.
      return;
    }
    // Handle division operator or single-line comment.
    else if (tk == '/') {
      if (*p == '/') {
        // Skip rest of line (comment).
        ++p;
        while (*p != 0 && *p != '\n')
          ++p;
      }
      else {
        // It's the division operator.
        tk = Div;
        return;
      }
    }
    // Handle string or character literal.
    else if (tk == '\'' || tk == '"') {
      pp = data;  // Begin storing literal in the data area.
      // Read until closing quote is found.
      while (*p != 0 && *p != tk) {
        // Handle escape sequences.
        if ((ival = *p++) == '\\') {
          if ((ival = *p++) == 'n')
            ival = '\n';
        }
        // For string literals (double quotes), store each character.
        if (tk == '"')
          *data++ = ival;
      }
      ++p;  // Skip the closing quote.
      // For string literals, set ival to point to the stored string.
      if (tk == '"')
        ival = (int)pp;
      else
        tk = Num; // Character literal is treated as a number.
      return;
    }
    // Handle operators and punctuation:
    else if (tk == '=') {
      if (*p == '=') { ++p; tk = Eq; }
      else { tk = Assign; }
      return;
    }
    else if (tk == '+') {
      if (*p == '+') { ++p; tk = Inc; }
      else { tk = Add; }
      return;
    }
    else if (tk == '-') {
      if (*p == '-') { ++p; tk = Dec; }
      else { tk = Sub; }
      return;
    }
    else if (tk == '!') {
      if (*p == '=') { ++p; tk = Ne; }
      return;
    }
    else if (tk == '<') {
      if (*p == '=') { ++p; tk = Le; }
      else if (*p == '<') { ++p; tk = Shl; }
      else { tk = Lt; }
      return;
    }
    else if (tk == '>') {
      if (*p == '=') { ++p; tk = Ge; }
      else if (*p == '>') { ++p; tk = Shr; }
      else { tk = Gt; }
      return;
    }
    else if (tk == '|') {
      if (*p == '|') { ++p; tk = Lor; }
      else { tk = Or; }
      return;
    }
    else if (tk == '&') {
      if (*p == '&') { ++p; tk = Lan; }
      else { tk = And; }
      return;
    }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    // For punctuation characters that do not require further processing,
    // simply return the character as the token.
    else if (tk == '~' || tk == ';' || tk == '{' ||
             tk == '}' || tk == '(' || tk == ')' ||
             tk == ']' || tk == ',' || tk == ':') {
      return;
    }
  }
}

/*
Function: expr(int lev)

 Purpose:
   Parses an expression using a top-down operator precedence (precedence climbing) technique,
   and emits intermediate code (into the array pointed to by 'e') for the virtual machine.
   The parameter 'lev' indicates the current minimum operator precedence level.

 Why it is written this way:
   The function supports a wide variety of expression constructs (literals, identifiers,
   function calls, type casts, unary and binary operators) with minimal code.
   Using recursive descent with precedence climbing enables clear handling of operator precedence.

 How it fits into the compiler:
   This is the parser and code generator for expressions. It converts high-level expressions
   into a sequence of opcodes that the virtual machine can execute.
*/
void expr(int lev) {
  int t, *d; // t: temporary variable for type; d: pointer used for patching jump addresses in conditional expressions.

  if (!tk) {
    // If there is no token, we've reached an unexpected end-of-file.
    printf("%d: unexpected eof in expression\n", line);
    exit(-1);
  }
  else if (tk == Num) {
    // Numeric literal: load the immediate value onto the code stream.
    *++e = IMM;    // Emit opcode to load an immediate value.
    *++e = ival;   // The literal value stored in 'ival'.
    next();        // Advance to the next token.
    ty = INT;      // Numeric literals are of type INT.
  }
  else if (tk == '"') {
    // String literal: load the address of the stored string.
    *++e = IMM;    // Emit opcode for immediate value.
    *++e = ival;   // 'ival' holds the address of the string literal in the data area.
    next();        // Advance past the string literal.
    // If there are adjacent string literals, process them.
    while (tk == '"')
      next();
    // Align the data pointer to a word boundary.
    data = (char *)((int)data + sizeof(int) & -sizeof(int));
    ty = PTR;      // The type of a string literal is a pointer.
  }
  else if (tk == Sizeof) {
    // Handle the sizeof operator: compute the size of a type.
    next(); 
    if (tk == '(') 
      next(); 
    else {
      printf("%d: open paren expected in sizeof\n", line);
      exit(-1);
    }
    // Default type is INT.
    ty = INT; 
    if (tk == Int)
      next(); 
    else if (tk == Char) { 
      next(); 
      ty = CHAR; 
    }
    // Process potential pointer type (e.g., int **).
    while (tk == Mul) { 
      next(); 
      ty = ty + PTR; 
    }
    if (tk == ')') 
      next(); 
    else {
      printf("%d: close paren expected in sizeof\n", line);
      exit(-1);
    }
    // Emit code to load the size (either sizeof(char) or sizeof(int)).
    *++e = IMM; 
    *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
    ty = INT; // The result of sizeof is an integer.
  }
  else if (tk == Id) {
    // Identifier: could be a variable or a function call.
    d = id;  // Save the current identifier details.
    next();  // Advance past the identifier.
    if (tk == '(') {
      // Function call: an identifier followed by '(' indicates a function call.
      next();  // Skip the '('.
      t = 0;   // Counter for the number of arguments.
      // Process each argument separated by commas.
      while (tk != ')') { 
        expr(Assign);  // Parse an argument expression.
        *++e = PSH;    // Push the argument value onto the stack.
        ++t;
        if (tk == ',')
          next();
      }
      next();  // Skip the closing ')'.
      if (d[Class] == Sys) {
        // System functions: directly emit the function's opcode.
        *++e = d[Val];
      }
      else if (d[Class] == Fun) {
        // User-defined functions: emit a Jump to Subroutine (JSR) instruction.
        *++e = JSR;
        *++e = d[Val];  // The address of the function entry point.
      }
      else {
        printf("%d: bad function call\n", line);
        exit(-1);
      }
      // Adjust the stack if arguments were pushed.
      if (t) {
        *++e = ADJ;
        *++e = t;
      }
      // The return type of the function becomes the type of the expression.
      ty = d[Type];
    }
    else if (d[Class] == Num) {
      // Identifier represents a numeric constant.
      *++e = IMM;
      *++e = d[Val];
      ty = INT;
    }
    else {
      // Variable: load its address and then its value.
      if (d[Class] == Loc) {
        // Local variable: compute the effective address relative to the base pointer.
        *++e = LEA;
        *++e = loc - d[Val];
      }
      else if (d[Class] == Glo) {
        // Global variable: load its absolute address.
        *++e = IMM;
        *++e = d[Val];
      }
      else {
        printf("%d: undefined variable\n", line);
        exit(-1);
      }
      // Depending on the type (CHAR or INT), emit the correct load instruction.
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  else if (tk == '(') {
    // Parenthesized expression or type cast.
    next();  // Skip '('.
    if (tk == Int || tk == Char) {
      // Type cast: e.g., (int) or (char *) cast.
      t = (tk == Int) ? INT : CHAR;
      next();  // Skip the type keyword.
      while (tk == Mul) { 
        next(); 
        t = t + PTR;  // Process pointer casts (e.g., (int *) or (char **)).
      }
      if (tk == ')')
        next();  // Skip ')'.
      else {
        printf("%d: bad cast\n", line);
        exit(-1);
      }
      expr(Inc);  // Parse the expression being cast.
      ty = t;  // Update the type to the cast type.
    }
    else {
      // Just a grouped expression: evaluate it normally.
      expr(Assign);
      if (tk == ')')
        next();  // Skip ')'.
      else {
        printf("%d: close paren expected\n", line);
        exit(-1);
      }
    }
  }
  else if (tk == Mul) {
    // Unary dereference operator: *expr
    next(); 
    expr(Inc);  // Parse the expression after '*'.
    if (ty > INT)
      ty = ty - PTR;  // Decrease pointer level.
    else {
      printf("%d: bad dereference\n", line);
      exit(-1);
    }
    // Emit appropriate load instruction based on type.
    *++e = (ty == CHAR) ? LC : LI;
  }
  else if (tk == And) {
    // Address-of operator: &expr
    next(); 
    expr(Inc);  // Parse the operand.
    // Only allow lvalues (addressable memory) for address-of.
    if (*e == LC || *e == LI)
      --e;  // Remove the load instruction, so we instead take its address.
    else {
      printf("%d: bad address-of\n", line);
      exit(-1);
    }
    ty = ty + PTR;  // Increase pointer level.
  }
  else if (tk == '!') {
    // Logical NOT operator: !expr
    next();
    expr(Inc);
    // Emit code to compare the result with zero.
    *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ;
    ty = INT;  // Result is an integer.
  }
  else if (tk == '~') {
    // Bitwise NOT operator: ~expr
    next();
    expr(Inc);
    // Emit code to invert bits by XOR-ing with -1.
    *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR;
    ty = INT;
  }
  else if (tk == Add) {
    // Unary plus operator: +expr (has no effect but still parsed).
    next();
    expr(Inc);
    ty = INT;
  }
  else if (tk == Sub) {
    // Unary minus operator: -expr
    next();
    *++e = IMM;  // Emit immediate opcode for negation.
    if (tk == Num) {
      // If directly followed by a number, negate it.
      *++e = -ival;
      next();
    } else {
      // Otherwise, push -1 and multiply to negate.
      *++e = -1; *++e = PSH;
      expr(Inc);
      *++e = MUL;
    }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) {
    // Pre-increment or pre-decrement operator.
    t = tk;  // Save which operator it is.
    next();
    expr(Inc);  // Parse the operand.
    // Ensure the operand is an lvalue (addressable).
    if (*e == LC) { 
      *e = PSH; *++e = LC;
    }
    else if (*e == LI) { 
      *e = PSH; *++e = LI;
    }
    else {
      printf("%d: bad lvalue in pre-increment\n", line);
      exit(-1);
    }
    // Emit code to adjust the value.
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  else {
    printf("%d: bad expression\n", line);
    exit(-1);
  }

  // Process binary operators and further chaining using precedence climbing.
  while (tk >= lev) {
    t = ty;  // Save the current type before processing the operator.
    if (tk == Assign) {
      next();
      // The left-hand side must be an lvalue.
      if (*e == LC || *e == LI)
        *e = PSH;
      else {
        printf("%d: bad lvalue in assignment\n", line);
        exit(-1);
      }
      expr(Assign);
      // Emit store instruction based on type.
      *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    else if (tk == Cond) { // Ternary conditional operator (?:)
      next();
      *++e = BZ;   // Emit branch-if-zero opcode.
      d = ++e;     // Reserve a slot to patch the jump address.
      expr(Assign);
      if (tk == ':')
        next();
      else {
        printf("%d: conditional missing colon\n", line);
        exit(-1);
      }
      *d = (int)(e + 3);  // Patch jump address for false branch.
      *++e = JMP;         // Emit unconditional jump after true branch.
      d = ++e;
      expr(Cond);
      *d = (int)(e + 1);  // Patch jump address for false branch.
    }
    else if (tk == Lor) { 
      // Logical OR: short-circuit evaluation.
      next();
      *++e = BNZ; d = ++e;
      expr(Lan);
      *d = (int)(e + 1);
      ty = INT;
    }
    else if (tk == Lan) { 
      // Logical AND: short-circuit evaluation.
      next();
      *++e = BZ; d = ++e;
      expr(Or);
      *d = (int)(e + 1);
      ty = INT;
    }
    else if (tk == Or)  { 
      next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; 
    }
    else if (tk == Xor) { 
      next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; 
    }
    else if (tk == And) { 
      next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; 
    }
    else if (tk == Eq)  { 
      next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; 
    }
    else if (tk == Ne)  { 
      next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; 
    }
    else if (tk == Lt)  { 
      next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; 
    }
    else if (tk == Gt)  { 
      next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; 
    }
    else if (tk == Le)  { 
      next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; 
    }
    else if (tk == Ge)  { 
      next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; 
    }
    else if (tk == Shl) { 
      next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; 
    }
    else if (tk == Shr) { 
      next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; 
    }
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      // For pointer arithmetic, adjust by the size of the pointed-to type.
      if ((ty = t) > PTR) {
        *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;
      }
      *++e = ADD;
    }
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) {
        // Pointer subtraction: compute the difference and divide by the size.
        *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV;
        ty = INT;
      }
      else if ((ty = t) > PTR) {
        // Adjust for pointer arithmetic.
        *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB;
      }
      else {
        *++e = SUB;
      }
    }
    else if (tk == Mul) { 
      next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; 
    }
    else if (tk == Div) { 
      next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; 
    }
    else if (tk == Mod) { 
      next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; 
    }
    else if (tk == Inc || tk == Dec) {
      // Handle post-increment/decrement.
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else {
        printf("%d: bad lvalue in post-increment\n", line);
        exit(-1);
      }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    else {
      printf("%d: compiler error tk=%d\n", line, tk);
      exit(-1);
    }
  }
}

/*
 Function: stmt()

 Purpose:
   Parses a complete statement from the source code and emits intermediate code
   for control structures and expression evaluation. This includes handling if-else,
   while loops, block statements, return statements, and simple expression statements.

 Why it is written this way:
   The function uses recursive descent to break down statements into smaller pieces.
   It emits jump instructions for control flow and ensures proper nesting of statements.

 How it fits into the compiler:
   This is the statement parser. It works in tandem with the expression parser (expr())
   to build the complete intermediate representation of the program.
*/
void stmt() {
  int *a, *b; // Pointers used to hold jump addresses for control flow (if-else, loops).

  if (tk == If) {  // Handle an if statement.
    next(); // Move past the 'if' keyword.
    if (tk == '(')
      next();  // Expect and skip '('.
    else {
      printf("%d: open paren expected\n", line);
      exit(-1);
    }
    expr(Assign); // Parse the condition expression.
    if (tk == ')')
      next();  // Expect and skip ')'.
    else {
      printf("%d: close paren expected\n", line);
      exit(-1);
    }
    *++e = BZ;   // Emit branch-if-zero opcode; if condition is false, branch.
    b = ++e;     // Reserve space for the jump address to be patched later.
    stmt();      // Parse the statement for the 'then' block.
    if (tk == Else) {  // Handle optional 'else' clause.
      *b = (int)(e + 3);  // Patch jump address for false branch.
      *++e = JMP;         // Emit unconditional jump after 'then' block.
      b = ++e;            // Reserve space for jump address after 'else' block.
      next();             // Move past 'else' keyword.
      stmt();             // Parse the 'else' statement.
    }
    *b = (int)(e + 1);  // Final patching of jump address.
  }
  else if (tk == While) {  // Handle a while loop.
    next();  // Move past 'while' keyword.
    a = e + 1; // Save the address of the beginning of the loop.
    if (tk == '(')
      next();  // Expect and skip '('.
    else {
      printf("%d: open paren expected\n", line);
      exit(-1);
    }
    expr(Assign); // Parse the loop condition.
    if (tk == ')')
      next();  // Expect and skip ')'.
    else {
      printf("%d: close paren expected\n", line);
      exit(-1);
    }
    *++e = BZ; // Emit branch-if-zero opcode to exit loop when condition is false.
    b = ++e;   // Reserve a slot for the exit jump address.
    stmt();    // Parse the loop body.
    *++e = JMP;    // Emit jump instruction to go back to loop start.
    *++e = (int)a; // Jump back to the saved loop condition.
    *b = (int)(e + 1); // Patch the exit branch address.
  }
  else if (tk == Return) {  // Handle a return statement.
    next(); // Move past 'return'.
    if (tk != ';')
      expr(Assign); // Parse the return expression if present.
    *++e = LEV; // Emit the LEV opcode to exit the subroutine.
    if (tk == ';')
      next(); // Expect and skip the semicolon.
    else {
      printf("%d: semicolon expected\n", line);
      exit(-1);
    }
  }
  else if (tk == '{') {  // Handle a compound statement (block).
    next();  // Skip '{'
    while (tk != '}')
      stmt(); // Recursively parse statements inside the block.
    next();  // Skip '}'
  }
  else if (tk == ';') {  // Handle an empty statement.
    next();  // Simply skip the semicolon.
  }
  else {
    // Otherwise, it is an expression statement.
    expr(Assign); // Parse the expression.
    if (tk == ';')
      next(); // Expect and skip the semicolon.
    else {
      printf("%d: semicolon expected\n", line);
      exit(-1);
    }
  }
}

/*
 Function: main(int argc, char **argv)

 Purpose:
   Entry point for the compiler. It performs the following tasks:
     1. Processes command-line arguments for optional flags (-s for source output, -d for debugging).
     2. Opens and reads the source file into memory.
     3. Allocates memory pools for the symbol table, code, data, and stack.
     4. Initializes the symbol table with keywords and system function names.
     5. Parses global declarations and function definitions.
     6. Locates the main() function and sets up the virtual machine to executethe emitted code.

 Why it is written this way:
   The main function coordinates the entire compilation process, from reading the input
   to executing the generated code. Its structure reflects the sequential phases of compilation.

 How it fits into the compiler:
   This function ties together the lexer, parser, code generator, and virtual machine
   into a single workflow.
*/
int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // Variables for virtual machine registers.
  int i, *t; // Temporary variables.

  // Adjust command-line arguments (skip the program name).
  --argc; ++argv;
  // Check for optional flags.
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') {
    src = 1;  // Enable source/assembly output.
    --argc; ++argv;
  }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') {
    debug = 1;  // Enable debug output during VM execution.
    --argc; ++argv;
  }
  if (argc < 1) {
    // If no file is provided, print usage and exit.
    printf("usage: c4 [-s] [-d] file ...\n");
    return -1;
  }

  // Open the source file.
  if ((fd = open(*argv, 0)) < 0) {
    printf("could not open(%s)\n", *argv);
    return -1;
  }

  // Allocate a fixed-size memory pool for symbol, code, data, and stack areas.
  poolsz = 256 * 1024; // Arbitrary size.
  if (!(sym = malloc(poolsz))) {
    printf("could not malloc(%d) symbol area\n", poolsz);
    return -1;
  }
  if (!(le = e = malloc(poolsz))) {
    printf("could not malloc(%d) text area\n", poolsz);
    return -1;
  }
  if (!(data = malloc(poolsz))) {
    printf("could not malloc(%d) data area\n", poolsz);
    return -1;
  }
  if (!(sp = malloc(poolsz))) {
    printf("could not malloc(%d) stack area\n", poolsz);
    return -1;
  }

  // Initialize the allocated memory areas with zeros.
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  // Initialize the symbol table with keywords and system functions.
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  // Add keywords "char" to "while" to the symbol table.
  i = Char; 
  while (i <= While) {
    next();
    id[Tk] = i++;  // Assign each keyword a token value.
  }
  // Add system function names (open, read, etc.) to the symbol table.
  i = OPEN; 
  while (i <= EXIT) {
    next();
    id[Class] = Sys;   // Mark as system call.
    id[Type] = INT;    // System functions return int.
    id[Val] = i++;     // Assign a unique opcode.
  }
  next();
  id[Tk] = Char; // Handle the 'void' type as 'char' in this minimal implementation.
  next();
  idmain = id;   // Save a pointer to the main() function identifier.

  // Allocate memory for the source code and load the file into the allocated buffer.
  if (!(lp = p = malloc(poolsz))) {
    printf("could not malloc(%d) source area\n", poolsz);
    return -1;
  }
  if ((i = read(fd, p, poolsz - 1)) <= 0) {
    printf("read() returned %d\n", i);
    return -1;
  }
  p[i] = 0; // Null-terminate the source code.
  close(fd); // Close the file descriptor.

  // Begin parsing global declarations and function definitions.
  line = 1;  // Initialize line number.
  next();    // Start tokenizing.
  while (tk) {
    bt = INT; // Default base type is INT.
    if (tk == Int)
      next();  // Process 'int' keyword.
    else if (tk == Char) { 
      next(); 
      bt = CHAR;  // Process 'char' keyword.
    }
    else if (tk == Enum) {
      // Process enum declarations.
      next();
      if (tk != '{')
        next();
      if (tk == '{') {
        next();
        i = 0; // Enum value starts at 0.
        while (tk != '}') {
          if (tk != Id) {
            printf("%d: bad enum identifier %d\n", line, tk);
            return -1;
          }
          next();
          if (tk == Assign) { // Optional initializer.
            next();
            if (tk != Num) {
              printf("%d: bad enum initializer\n", line);
              return -1;
            }
            i = ival; // Set the starting value.
            next();
          }
          // Record the enum member in the symbol table.
          id[Class] = Num;
          id[Type] = INT;
          id[Val] = i++;
          if (tk == ',')
            next();
        }
        next();  // Skip closing '}'.
      }
    }
    // Parse a list of global declarations separated by commas.
    while (tk != ';' && tk != '}') {
      ty = bt;  // Set the current type.
      while (tk == Mul) { 
        next(); 
        ty = ty + PTR;  // Handle pointer types.
      }
      if (tk != Id) {
        printf("%d: bad global declaration\n", line);
        return -1;
      }
      if (id[Class]) {
        printf("%d: duplicate global definition\n", line);
        return -1;
      }
      next();  // Move to the next token after the identifier.
      id[Type] = ty;  // Set the type in the symbol table.
      if (tk == '(') {  // Function definition.
        id[Class] = Fun;  // Mark as a function.
        id[Val] = (int)(e + 1);  // Save the function entry point.
        next();
        i = 0;  // Parameter count.
        while (tk != ')') {  // Process parameters.
          ty = INT;
          if (tk == Int)
            next();
          else if (tk == Char) {
            next();
            ty = CHAR;
          }
          while (tk == Mul) {
            next();
            ty = ty + PTR;  // Process pointer parameters.
          }
          if (tk != Id) {
            printf("%d: bad parameter declaration\n", line);
            return -1;
          }
          if (id[Class] == Loc) {
            printf("%d: duplicate parameter definition\n", line);
            return -1;
          }
          // Save the current identifier state and mark it as a local parameter.
          id[HClass] = id[Class];
          id[Class] = Loc;
          id[HType]  = id[Type];
          id[Type] = ty;
          id[HVal]   = id[Val];
          id[Val] = i++;  // Parameter index.
          next();
          if (tk == ',')
            next();
        }
        next();  // Skip closing ')'.
        if (tk != '{') {
          printf("%d: bad function definition\n", line);
          return -1;
        }
        loc = ++i;  // Set the starting local variable index.
        next();
        // Parse local declarations within the function.
        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) {
              next();
              ty = ty + PTR;
            }
            if (tk != Id) {
              printf("%d: bad local declaration\n", line);
              return -1;
            }
            if (id[Class] == Loc) {
              printf("%d: duplicate local definition\n", line);
              return -1;
            }
            // Record the local variable in the symbol table.
            id[HClass] = id[Class];
            id[Class] = Loc;
            id[HType]  = id[Type];
            id[Type] = ty;
            id[HVal]   = id[Val];
            id[Val] = ++i;
            next();
            if (tk == ',')
              next();
          }
          next();  // Skip semicolon after local declarations.
        }
        // Emit function prologue: allocate space for local variables.
        *++e = ENT;
        *++e = i - loc;
        // Parse the function body.
        while (tk != '}')
          stmt();
        *++e = LEV;  // Emit function epilogue.
        // Unwind the symbol table to remove local definitions.
        id = sym;
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else {
        // Global variable declaration.
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + sizeof(int);
      }
      if (tk == ',')
        next();  // Handle multiple declarations.
    }
    next();
  }
  // Ensure that main() is defined.
  if (!(pc = (int *)idmain[Val])) {
    printf("main() not defined\n");
    return -1;
  }
  if (src)
    return 0;  // If source output is enabled, stop before execution.

  // Set up the virtual machine's stack.
  bp = sp = (int *)((int)sp + poolsz);
  *--sp = EXIT;  // Push an EXIT opcode so that the program terminates if main returns.
  *--sp = PSH; t = sp;
  *--sp = argc;    // Push command-line argument count.
  *--sp = (int)argv;  // Push pointer to command-line arguments.
  *--sp = (int)t;  // Push a pointer used for later stack adjustment.

  // Virtual machine execution loop.
  cycle = 0; // Initialize cycle counter.
  while (1) {
    i = *pc++;  // Fetch the next opcode.
    ++cycle;
    if (debug) {
      // If debugging is enabled, print the current instruction.
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ)
        printf(" %d\n", *pc);
      else
        printf("\n");
    }
    // Dispatch to the appropriate operation based on the opcode.
    if      (i == LEA) a = (int)(bp + *pc++);      // LEA: Load effective address (for local variables).
    else if (i == IMM) a = *pc++;                  // IMM: Load immediate constant.
    else if (i == JMP) pc = (int *)*pc;            // JMP: Unconditional jump.
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }  // JSR: Jump to subroutine.
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc; // BZ: Branch if zero.
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1; // BNZ: Branch if not zero.
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; } // ENT: Enter subroutine (set up stack frame).
    else if (i == ADJ) sp = sp + *pc++;            // ADJ: Adjust stack pointer.
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // LEV: Leave subroutine (restore frame).
    else if (i == LI)  a = *(int *)a;              // LI: Load integer from memory.
    else if (i == LC)  a = *(char *)a;             // LC: Load char from memory.
    else if (i == SI)  *(int *)*sp++ = a;          // SI: Store integer into memory.
    else if (i == SC)  a = *(char *)*sp++ = a;       // SC: Store char into memory.
    else if (i == PSH) *--sp = a;                  // PSH: Push value onto stack.
    else if (i == OR)  a = *sp++ |  a;             // Bitwise OR.
    else if (i == XOR) a = *sp++ ^  a;             // Bitwise XOR.
    else if (i == AND) a = *sp++ &  a;             // Bitwise AND.
    else if (i == EQ)  a = *sp++ == a;             // Equality check.
    else if (i == NE)  a = *sp++ != a;             // Inequality check.
    else if (i == LT)  a = *sp++ <  a;             // Less-than comparison.
    else if (i == GT)  a = *sp++ >  a;             // Greater-than comparison.
    else if (i == LE)  a = *sp++ <= a;             // Less-than or equal.
    else if (i == GE)  a = *sp++ >= a;             // Greater-than or equal.
    else if (i == SHL) a = *sp++ << a;             // Left shift.
    else if (i == SHR) a = *sp++ >> a;             // Right shift.
    else if (i == ADD) a = *sp++ +  a;             // Addition.
    else if (i == SUB) a = *sp++ -  a;             // Subtraction.
    else if (i == MUL) a = *sp++ *  a;             // Multiplication.
    else if (i == DIV) a = *sp++ /  a;             // Division.
    else if (i == MOD) a = *sp++ %  a;             // Modulus.
    // System calls and library functions.
    else if (i == OPEN) a = open((char *)sp[1], *sp);
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1],
                       t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (int)malloc(*sp);
    else if (i == FREE) free((void *)*sp);
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    else if (i == EXIT) { 
      printf("exit(%d) cycle = %d\n", *sp, cycle);
      return *sp;
    }
    else {
      printf("unknown instruction = %d! cycle = %d\n", i, cycle);
      return -1;
    }
  }
}
