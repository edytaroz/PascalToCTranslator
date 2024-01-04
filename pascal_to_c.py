import ply.lex as lex
import ply.yacc as yacc

reserved = {
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'for': 'FOR',
    'repeat': 'REPEAT',
    'until': 'UNTIL',
    'begin': 'BEGIN',
    'end': 'END',
    'var': 'VAR',
    'to': 'TO',
    'do': 'DO',
    'downto': 'DOWNTO',
    'string': 'STRING',
    'integer': 'INTEGER',
    'char': 'CHARACTER',
    'boolean': 'BOOLEAN',
    'real': 'REAL',
    'or': 'OR',
    'and': 'AND',
    'not': 'NOT',
    'program': 'PROGRAM',
    'function': 'FUNCTION',
    'procedure': 'PROCEDURE',
    'write': 'WRITE',
    'mod': 'MOD'
}

tokens = [
             'ID',
             'PLUS',
             'MINUS',
             'TIMES',
             'DIVIDE',
             'EQUAL',
             'GREATER',
             'LESSER',
             'GREATER_EQUAL',
             'LESSER_EQUAL',
             'ASSIGN',
             'SEMICOLON',
             'COLON',
             'COMMA',
             'LPAREN',
             'RPAREN',
             'STR',
             'INT',
             'CHAR',
             'BOOL',
             'RE'
         ] + list(reserved.values())

t_PLUS = r'\+'
t_MINUS = r'\-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_EQUAL = r'\='
t_GREATER = r'\>'
t_LESSER = r'\<'
t_GREATER_EQUAL = r'\>\='
t_LESSER_EQUAL = r'\<\='
t_ASSIGN = ':='
t_SEMICOLON = ';'
t_COLON = r':'
t_COMMA = r','

t_INT = r'(\-)*[0-9]+'
t_RE = r'(\-)*[0-9]+\.[0-9]+'
t_STR = r'\'([^\\\']|(\\.))*\''
t_CHAR = r'(\'([^\\\'])\')'
t_BOOL = r'true|false'


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # Check for reserved words
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


t_ignore = ' \t'


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


precedence = (
    ('nonassoc', 'LESSER', 'GREATER', 'LESSER_EQUAL', 'GREATER_EQUAL', 'EQUAL'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('left', 'OR', 'AND'),
)


def p_start(p):
    "start : PROGRAM ID SEMICOLON variable_block functions main_body"
    output = ("#include <stdio.h>\n"
              "#include <stdlib.h>\n"
              "#include <string.h>\n"
              "#include <stdbool.h>\n")
    if p[4] is not None:
        output += p[4]
    if p[5] is not None:
        output += p[5]
    if p[6] is not None:
        output += "int main(){\n"
        output += p[6]
        output += "return 0;\n"
        output += "}\n"
    out.write(output + "\n")


def p_functions(p):
    '''functions : FUNCTION ID LPAREN function_variables RPAREN COLON INTEGER SEMICOLON variable_block main_body functions
            | FUNCTION ID LPAREN function_variables RPAREN COLON REAL SEMICOLON variable_block main_body functions
            | FUNCTION ID LPAREN function_variables RPAREN COLON BOOLEAN SEMICOLON variable_block main_body functions
            | FUNCTION ID LPAREN function_variables RPAREN COLON STRING SEMICOLON variable_block main_body functions
            | FUNCTION ID LPAREN function_variables RPAREN COLON CHARACTER SEMICOLON variable_block main_body functions
            | PROCEDURE ID SEMICOLON variable_block main_body functions
            | empty'''
    if len(p) > 2:
        if p[1] == 'function':
            x = p[10].replace(p[2] + " =", p[2] + "1" + " =")
            p[0] = type_to_c(p[7]) + ' ' + p[2] + '(' + p[4] + '){\n' + type_to_c(p[7]) + ' ' + p[2] + "1" + ';\n' + p[9] + x + 'return ' + p[2] + "1" + ';\n' + '}\n' + p[11]
        else:
            p[0] = 'void' + ' ' + p[2] + '(){\n' + p[4] + p[5] + '}\n' + p[6]
    else:
        p[0] = p[1]


def p_function_variables(p):
    '''function_variables : ID COLON INTEGER
            | ID COLON REAL
            | ID COLON BOOLEAN
            | ID COLON STRING
            | ID COLON CHARACTER
            | ID COLON INTEGER COMMA function_variables
            | ID COLON REAL COMMA function_variables
            | ID COLON BOOLEAN COMMA function_variables
            | ID COLON STRING COMMA function_variables
            | ID COLON CHARACTER COMMA function_variables
            | ID COLON INTEGER SEMICOLON function_variables
            | ID COLON REAL SEMICOLON function_variables
            | ID COLON BOOLEAN SEMICOLON function_variables
            | ID COLON STRING SEMICOLON function_variables
            | ID COLON CHARACTER SEMICOLON function_variables
            | empty'''
    if len(p) > 4:
        if p[3] == "string":
            p[0] = type_to_c(p[3]) + " " + p[1] + "[]" + ', ' + p[5]
        else:
            p[0] = type_to_c(p[3]) + " " + p[1] + ', ' + p[5]
    elif len(p) > 3:
        p[0] = type_to_c(p[3]) + " " + p[1]
        if p[3] == "string":
            p[0] += "[]"
    else:
        p[0] = p[1]


def p_main_body(p):
    "main_body : BEGIN body END"
    p[0] = p[2]


def p_write(p):
    '''write : WRITE LPAREN STR RPAREN SEMICOLON
        | WRITE LPAREN expression RPAREN SEMICOLON'''
    p[0] = "printf(" + p[3] + ");\n"


def p_body(p):
    '''body : while_loop body
        | for_loop body
        | repeat_loop body
        | if_statement body
        | expression body
        | var_assign body
        | write body
        | empty'''
    if len(p) > 2:
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]


def p_while_loop(p):
    '''while_loop : WHILE boolean_expression DO main_body'''
    p[0] = p[1] + '(' + p[2] + '){\n' + p[4] + '}\n'


def p_for_loop(p):
    '''for_loop : FOR ID ASSIGN INT DOWNTO INT DO main_body
            | FOR ID ASSIGN INT TO INT DO main_body'''
    sign = ""
    do = ""
    if p[5] == "downto":
        sign = ">"
        do = "--"
    else:
        sign = "<"
        do = "++"
    p[0] = p[1] + '(' + p[2] + "=" + p[4] + ";" + p[2] + sign + p[6] + ";" + p[2] + do + '){\n' + p[8] + '}\n'


def p_repeat_loop(p):
    '''repeat_loop : REPEAT body UNTIL boolean_expression SEMICOLON'''
    p[0] = "do{\n" + p[2] + "}\n" + "while(" + p[4] + ")\n"


def p_if_statement(p):
    '''if_statement : IF boolean_expression THEN main_body
        | IF boolean_expression THEN main_body ELSE main_body'''
    if len(p) > 6:
        p[0] = p[1] + "(" + p[2] + "){\n" + p[4] + "}\n" + p[5] + "{\n" + p[6] + "}\n"
    elif len(p) > 4:
        p[0] = p[1] + "(" + p[2] + "){\n" + p[4] + "}\n"


def p_numeric(p):
    '''numeric : INT
            | RE
            | ID'''
    p[0] = p[1]


def p_boolean_expression(p):
    '''boolean_expression : expression LESSER expression
            | expression LESSER_EQUAL expression
            | expression EQUAL expression
            | expression GREATER_EQUAL expression
            | expression GREATER expression
            | NOT boolean_expression
            | boolean_expression and_or boolean_expression
            | LPAREN boolean_expression RPAREN
            | BOOL'''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 3:
        p[0] = "!(" + p[2] + ")"
    else:
        if p[2] == "=":
            p[2] = "=="
        if p[1] == "(" or p[1] == ")":
            p[0] = p[2]
        else:
            p[0] = p[1] + " " + p[2] + " " + p[3]


def p_and_or(p):
    '''and_or : AND
            | OR'''
    if p[1] == "or":
        p[0] = " || "
    else:
        p[0] = " && "


def p_var_assign(p):
    '''var_assign : ID ASSIGN STR SEMICOLON
        | ID ASSIGN CHAR SEMICOLON
        | ID ASSIGN BOOL SEMICOLON
        | ID ASSIGN expression SEMICOLON
        | ID SEMICOLON'''
    if len(p) > 3:
        p[0] = p[1] + " = " + p[3] + ";\n"
    else:
        p[0] = p[1] + ";\n"


def p_function_call(p):
    '''function_call : ID LPAREN vars RPAREN'''
    p[0] = p[1] + p[2] + p[3] + p[4]


def p_vars(p):
    '''vars : STR COMMA vars
            | BOOL COMMA vars
            | CHAR COMMA vars
            | expression COMMA vars
            | expression
            | STR
            | BOOL
            | CHAR'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[2] + p[3]


def p_binary_operators(p):
    '''expression : expression PLUS expression
           | expression MINUS expression
           | expression TIMES expression
           | expression DIVIDE expression
           | expression MOD expression
           | LPAREN expression RPAREN
           | numeric
           | function_call'''
    if len(p) > 3:
        if p[2] == '+':
            p[0] = p[1] + p[2] + p[3]
        elif p[2] == '-':
            p[0] = p[1] + p[2] + p[3]
        elif p[2] == '*':
            p[0] = p[1] + p[2] + p[3]
        elif p[2] == '/':
            p[0] = p[1] + p[2] + p[3]
        elif p[2] == 'mod':
            p[0] = p[1] % p[3]
        elif p[1] == '(' or p[1] == ')':
            p[0] = p[2]
    else:
        p[0] = p[1]


def p_variable_block(p):
    '''variable_block : empty
        | VAR variables'''
    if len(p) > 2:
        p[0] = p[2]
    else:
        p[0] = p[1]


def p_variables(p):
    '''variables : variable_describe
        | variable_describe variables'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[2]


def p_variable_describe(p):
    '''variable_describe : ID COLON STRING SEMICOLON
        | ID COLON INTEGER SEMICOLON
        | ID COLON CHARACTER SEMICOLON
        | ID COLON BOOLEAN SEMICOLON
        | ID COLON REAL SEMICOLON'''
    if p[3] == "string":
        p[0] = type_to_c(p[3]) + " " + p[1] + "[]" + ";\n"
    else:
        p[0] = type_to_c(p[3]) + " " + p[1] + ";\n"


def p_empty(p):
    'empty :'
    p[0] = ""


def p_error(p):
    print(parser.token())
    if p:
        print("Syntax error at token", p.type)
        # Just discard the token and tell the parser it's okay.
        parser.errok()
    else:
        print("Syntax error at EOF")


def type_to_c(t):
    if t == "integer":
        return "int"
    if t == "real":
        return "float"
    if t == "Boolean":
        return "bool"
    if t == "string":
        return "char"
    return t


lexer = lex.lex()
parser = yacc.yacc()

# f = open("HelloWorld.pas", "rt")
# out = open("HelloWorld.c", "w")
f = open("Fibonnacci.pas", "rt")
out = open("Fibonnacci.c", "w")
# f = open("test.pas", "rt")
# out = open("test.c", "w")
lines = f.read()
parser.parse(lines)
f.close()
