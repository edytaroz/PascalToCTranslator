
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'nonassocLESSERGREATERLESSER_EQUALGREATER_EQUALEQUALleftPLUSMINUSleftTIMESDIVIDEleftORANDAND ASSIGN BEGIN BOOL BOOLEAN CHAR CHARACTER COLON COMMA DIVIDE DO DOWNTO ELSE END EQUAL FOR FUNCTION GREATER GREATER_EQUAL ID IF INT INTEGER LESSER LESSER_EQUAL LPAREN MINUS MOD NOT OR PLUS PROCEDURE PROGRAM RE REAL REPEAT RPAREN SEMICOLON STR STRING THEN TIMES TO UNTIL VAR WHILE WRITEstart : PROGRAM ID SEMICOLON variable_block functions main_bodyfunctions : FUNCTION ID LPAREN function_variables RPAREN COLON INTEGER SEMICOLON variable_block main_body functions\n            | FUNCTION ID LPAREN function_variables RPAREN COLON REAL SEMICOLON variable_block main_body functions\n            | FUNCTION ID LPAREN function_variables RPAREN COLON BOOLEAN SEMICOLON variable_block main_body functions\n            | FUNCTION ID LPAREN function_variables RPAREN COLON STRING SEMICOLON variable_block main_body functions\n            | FUNCTION ID LPAREN function_variables RPAREN COLON CHARACTER SEMICOLON variable_block main_body functions\n            | PROCEDURE ID SEMICOLON variable_block main_body functions\n            | emptyfunction_variables : ID COLON INTEGER\n            | ID COLON REAL\n            | ID COLON BOOLEAN\n            | ID COLON STRING\n            | ID COLON CHARACTER\n            | ID COLON INTEGER COMMA function_variables\n            | ID COLON REAL COMMA function_variables\n            | ID COLON BOOLEAN COMMA function_variables\n            | ID COLON STRING COMMA function_variables\n            | ID COLON CHARACTER COMMA function_variables\n            | ID COLON INTEGER SEMICOLON function_variables\n            | ID COLON REAL SEMICOLON function_variables\n            | ID COLON BOOLEAN SEMICOLON function_variables\n            | ID COLON STRING SEMICOLON function_variables\n            | ID COLON CHARACTER SEMICOLON function_variables\n            | emptymain_body : BEGIN body ENDwrite : WRITE LPAREN STR RPAREN SEMICOLON\n        | WRITE LPAREN expression RPAREN SEMICOLONbody : while_loop body\n        | for_loop body\n        | repeat_loop body\n        | if_statement body\n        | expression body\n        | var_assign body\n        | write body\n        | emptywhile_loop : WHILE boolean_expression DO main_bodyfor_loop : FOR ID ASSIGN TO INT DO main_body\n            | FOR ID ASSIGN DOWNTO INT DO main_body\n            | FOR ID ASSIGN INT DOWNTO INT DO main_body\n            | FOR ID ASSIGN INT TO INT DO main_bodyrepeat_loop : REPEAT body UNTIL boolean_expression SEMICOLONif_statement : IF boolean_expression THEN main_body\n        | IF boolean_expression THEN main_body ELSE main_bodynumeric : INT\n            | RE\n            | IDboolean_expression : expression LESSER expression\n            | expression LESSER_EQUAL expression\n            | expression EQUAL expression\n            | expression GREATER_EQUAL expression\n            | expression GREATER expression\n            | NOT boolean_expression\n            | boolean_expression and_or boolean_expression\n            | LPAREN boolean_expression RPAREN\n            | BOOLand_or : AND\n            | ORvar_assign : ID ASSIGN STR SEMICOLON\n        | ID ASSIGN CHAR SEMICOLON\n        | ID ASSIGN BOOL SEMICOLON\n        | ID ASSIGN expression SEMICOLON\n        | ID SEMICOLONfunction_call : ID LPAREN vars RPARENvars : STR COMMA vars\n            | BOOL COMMA vars\n            | CHAR COMMA vars\n            | expression COMMA vars\n            | expression\n            | STR\n            | BOOL\n            | CHARexpression : expression PLUS expression\n           | expression MINUS expression\n           | expression TIMES expression\n           | expression DIVIDE expression\n           | expression MOD expression\n           | LPAREN expression RPAREN\n           | numeric\n           | function_callvariable_block : empty\n        | VAR variablesvariables : variable_describe\n        | variable_describe variablesvariable_describe : ID COLON STRING SEMICOLON\n        | ID COLON INTEGER SEMICOLON\n        | ID COLON CHARACTER SEMICOLON\n        | ID COLON BOOLEAN SEMICOLON\n        | ID COLON REAL SEMICOLONempty :'
    
_lr_action_items = {'PROGRAM':([0,],[2,]),'$end':([1,15,48,],[0,-1,-25,]),'ID':([2,7,9,10,13,16,22,23,24,25,26,27,28,30,31,32,33,34,35,36,37,38,40,41,48,54,55,56,57,58,63,64,66,68,69,70,74,79,80,81,82,83,84,85,86,87,88,90,91,92,93,94,95,96,97,111,113,119,130,131,132,133,134,135,136,137,138,140,158,160,161,162,163,164,165,166,167,168,169,170,171,181,197,200,206,207,],[3,14,17,18,14,32,32,32,32,32,32,32,32,66,67,-46,-44,32,66,66,-78,-79,-45,75,-25,66,66,66,66,66,66,66,-46,66,-62,66,66,-84,-85,-86,-87,-88,-72,-73,-74,-75,-76,66,-56,-57,66,66,66,66,66,66,-77,-36,-58,-59,-60,-61,-63,66,66,66,66,-42,-41,-26,-27,75,75,75,75,75,75,75,75,75,75,-43,-37,-38,-39,-40,]),'SEMICOLON':([3,18,32,33,37,38,40,43,44,45,46,47,65,66,84,85,86,87,88,98,102,103,104,105,113,120,121,122,123,124,125,126,134,139,141,142,143,144,145,146,147,172,173,174,175,176,],[4,42,69,-44,-78,-79,-45,79,80,81,82,83,-55,-46,-72,-73,-74,-75,-76,-52,130,131,132,133,-77,-53,-47,-48,-49,-50,-51,-54,-63,158,160,161,163,165,167,169,171,192,193,194,195,196,]),'VAR':([4,42,192,193,194,195,196,],[7,7,7,7,7,7,7,]),'FUNCTION':([4,5,6,12,13,19,48,79,80,81,82,83,118,208,209,210,211,212,],[-89,9,-80,-81,-82,-83,-25,-84,-85,-86,-87,-88,9,9,9,9,9,9,]),'PROCEDURE':([4,5,6,12,13,19,48,79,80,81,82,83,118,208,209,210,211,212,],[-89,10,-80,-81,-82,-83,-25,-84,-85,-86,-87,-88,10,10,10,10,10,10,]),'BEGIN':([4,5,6,8,11,12,13,19,42,48,78,79,80,81,82,83,89,112,118,149,159,177,180,192,193,194,195,196,198,199,201,202,203,204,205,208,209,210,211,212,213,214,215,216,217,],[-89,-89,-80,16,-8,-81,-82,-83,-89,-25,16,-84,-85,-86,-87,-88,16,16,-89,-7,16,16,16,-89,-89,-89,-89,-89,16,16,16,16,16,16,16,-89,-89,-89,-89,-89,-2,-3,-4,-5,-6,]),'COLON':([14,75,117,],[20,116,148,]),'WHILE':([16,22,23,24,25,26,27,28,32,33,34,37,38,40,48,66,69,84,85,86,87,88,113,119,130,131,132,133,134,140,158,160,161,181,197,200,206,207,],[30,30,30,30,30,30,30,30,-46,-44,30,-78,-79,-45,-25,-46,-62,-72,-73,-74,-75,-76,-77,-36,-58,-59,-60,-61,-63,-42,-41,-26,-27,-43,-37,-38,-39,-40,]),'FOR':([16,22,23,24,25,26,27,28,32,33,34,37,38,40,48,66,69,84,85,86,87,88,113,119,130,131,132,133,134,140,158,160,161,181,197,200,206,207,],[31,31,31,31,31,31,31,31,-46,-44,31,-78,-79,-45,-25,-46,-62,-72,-73,-74,-75,-76,-77,-36,-58,-59,-60,-61,-63,-42,-41,-26,-27,-43,-37,-38,-39,-40,]),'REPEAT':([16,22,23,24,25,26,27,28,32,33,34,37,38,40,48,66,69,84,85,86,87,88,113,119,130,131,132,133,134,140,158,160,161,181,197,200,206,207,],[34,34,34,34,34,34,34,34,-46,-44,34,-78,-79,-45,-25,-46,-62,-72,-73,-74,-75,-76,-77,-36,-58,-59,-60,-61,-63,-42,-41,-26,-27,-43,-37,-38,-39,-40,]),'IF':([16,22,23,24,25,26,27,28,32,33,34,37,38,40,48,66,69,84,85,86,87,88,113,119,130,131,132,133,134,140,158,160,161,181,197,200,206,207,],[35,35,35,35,35,35,35,35,-46,-44,35,-78,-79,-45,-25,-46,-62,-72,-73,-74,-75,-76,-77,-36,-58,-59,-60,-61,-63,-42,-41,-26,-27,-43,-37,-38,-39,-40,]),'LPAREN':([16,17,22,23,24,25,26,27,28,30,32,33,34,35,36,37,38,39,40,48,54,55,56,57,58,63,64,66,68,69,70,74,84,85,86,87,88,90,91,92,93,94,95,96,97,111,113,119,130,131,132,133,134,135,136,137,138,140,158,160,161,181,197,200,206,207,],[36,41,36,36,36,36,36,36,36,64,70,-44,36,64,36,-78,-79,74,-45,-25,36,36,36,36,36,64,64,70,36,-62,36,36,-72,-73,-74,-75,-76,64,-56,-57,36,36,36,36,36,64,-77,-36,-58,-59,-60,-61,-63,36,36,36,36,-42,-41,-26,-27,-43,-37,-38,-39,-40,]),'WRITE':([16,22,23,24,25,26,27,28,32,33,34,37,38,40,48,66,69,84,85,86,87,88,113,119,130,131,132,133,134,140,158,160,161,181,197,200,206,207,],[39,39,39,39,39,39,39,39,-46,-44,39,-78,-79,-45,-25,-46,-62,-72,-73,-74,-75,-76,-77,-36,-58,-59,-60,-61,-63,-42,-41,-26,-27,-43,-37,-38,-39,-40,]),'END':([16,21,22,23,24,25,26,27,28,29,32,33,37,38,40,48,49,50,51,52,53,59,60,66,69,84,85,86,87,88,113,119,130,131,132,133,134,140,158,160,161,181,197,200,206,207,],[-89,48,-89,-89,-89,-89,-89,-89,-89,-35,-46,-44,-78,-79,-45,-25,-28,-29,-30,-31,-32,-33,-34,-46,-62,-72,-73,-74,-75,-76,-77,-36,-58,-59,-60,-61,-63,-42,-41,-26,-27,-43,-37,-38,-39,-40,]),'INT':([16,22,23,24,25,26,27,28,30,32,33,34,35,36,37,38,40,48,54,55,56,57,58,63,64,66,68,69,70,74,84,85,86,87,88,90,91,92,93,94,95,96,97,101,111,113,119,127,129,130,131,132,133,134,135,136,137,138,140,151,152,158,160,161,181,197,200,206,207,],[33,33,33,33,33,33,33,33,33,-46,-44,33,33,33,-78,-79,-45,-25,33,33,33,33,33,33,33,-46,33,-62,33,33,-72,-73,-74,-75,-76,33,-56,-57,33,33,33,33,33,128,33,-77,-36,150,153,-58,-59,-60,-61,-63,33,33,33,33,-42,178,179,-41,-26,-27,-43,-37,-38,-39,-40,]),'RE':([16,22,23,24,25,26,27,28,30,32,33,34,35,36,37,38,40,48,54,55,56,57,58,63,64,66,68,69,70,74,84,85,86,87,88,90,91,92,93,94,95,96,97,111,113,119,130,131,132,133,134,135,136,137,138,140,158,160,161,181,197,200,206,207,],[40,40,40,40,40,40,40,40,40,-46,-44,40,40,40,-78,-79,-45,-25,40,40,40,40,40,40,40,-46,40,-62,40,40,-72,-73,-74,-75,-76,40,-56,-57,40,40,40,40,40,40,-77,-36,-58,-59,-60,-61,-63,40,40,40,40,-42,-41,-26,-27,-43,-37,-38,-39,-40,]),'STRING':([20,116,148,],[43,146,175,]),'INTEGER':([20,116,148,],[44,143,172,]),'CHARACTER':([20,116,148,],[45,147,176,]),'BOOLEAN':([20,116,148,],[46,145,174,]),'REAL':([20,116,148,],[47,144,173,]),'UNTIL':([22,23,24,25,26,27,28,29,32,33,34,37,38,40,48,49,50,51,52,53,59,60,66,69,71,84,85,86,87,88,113,119,130,131,132,133,134,140,158,160,161,181,197,200,206,207,],[-89,-89,-89,-89,-89,-89,-89,-35,-46,-44,-89,-78,-79,-45,-25,-28,-29,-30,-31,-32,-33,-34,-46,-62,111,-72,-73,-74,-75,-76,-77,-36,-58,-59,-60,-61,-63,-42,-41,-26,-27,-43,-37,-38,-39,-40,]),'PLUS':([26,32,33,37,38,40,62,66,73,84,85,86,87,88,100,105,110,113,115,121,122,123,124,125,134,],[54,-46,-44,-78,-79,-45,54,-46,54,-72,-73,-74,-75,54,54,54,54,-77,54,54,54,54,54,54,-63,]),'MINUS':([26,32,33,37,38,40,62,66,73,84,85,86,87,88,100,105,110,113,115,121,122,123,124,125,134,],[55,-46,-44,-78,-79,-45,55,-46,55,-72,-73,-74,-75,55,55,55,55,-77,55,55,55,55,55,55,-63,]),'TIMES':([26,32,33,37,38,40,62,66,73,84,85,86,87,88,100,105,110,113,115,121,122,123,124,125,134,],[56,-46,-44,-78,-79,-45,56,-46,56,56,56,-74,-75,56,56,56,56,-77,56,56,56,56,56,56,-63,]),'DIVIDE':([26,32,33,37,38,40,62,66,73,84,85,86,87,88,100,105,110,113,115,121,122,123,124,125,134,],[57,-46,-44,-78,-79,-45,57,-46,57,57,57,-74,-75,57,57,57,57,-77,57,57,57,57,57,57,-63,]),'MOD':([26,32,33,37,38,40,62,66,73,84,85,86,87,88,100,105,110,113,115,121,122,123,124,125,134,],[58,-46,-44,-78,-79,-45,58,-46,58,-72,-73,-74,-75,58,58,58,58,-77,58,58,58,58,58,58,-63,]),'NOT':([30,35,63,64,90,91,92,111,],[63,63,63,63,63,-56,-57,63,]),'BOOL':([30,35,63,64,68,70,90,91,92,111,135,136,137,138,],[65,65,65,65,104,108,65,-56,-57,65,108,108,108,108,]),'ASSIGN':([32,67,],[68,101,]),'LESSER':([33,37,38,40,62,66,84,85,86,87,88,100,113,134,],[-44,-78,-79,-45,93,-46,-72,-73,-74,-75,-76,93,-77,-63,]),'LESSER_EQUAL':([33,37,38,40,62,66,84,85,86,87,88,100,113,134,],[-44,-78,-79,-45,94,-46,-72,-73,-74,-75,-76,94,-77,-63,]),'EQUAL':([33,37,38,40,62,66,84,85,86,87,88,100,113,134,],[-44,-78,-79,-45,95,-46,-72,-73,-74,-75,-76,95,-77,-63,]),'GREATER_EQUAL':([33,37,38,40,62,66,84,85,86,87,88,100,113,134,],[-44,-78,-79,-45,96,-46,-72,-73,-74,-75,-76,96,-77,-63,]),'GREATER':([33,37,38,40,62,66,84,85,86,87,88,100,113,134,],[-44,-78,-79,-45,97,-46,-72,-73,-74,-75,-76,97,-77,-63,]),'RPAREN':([33,37,38,40,41,65,66,73,76,77,84,85,86,87,88,98,99,100,106,107,108,109,110,113,114,115,120,121,122,123,124,125,126,134,143,144,145,146,147,154,155,156,157,162,163,164,165,166,167,168,169,170,171,182,183,184,185,186,187,188,189,190,191,],[-44,-78,-79,-45,-89,-55,-46,113,117,-24,-72,-73,-74,-75,-76,-52,126,113,134,-69,-70,-71,-68,-77,141,142,-53,-47,-48,-49,-50,-51,-54,-63,-9,-10,-11,-12,-13,-64,-65,-66,-67,-89,-89,-89,-89,-89,-89,-89,-89,-89,-89,-14,-19,-15,-20,-16,-21,-17,-22,-18,-23,]),'COMMA':([33,37,38,40,66,84,85,86,87,88,107,108,109,110,113,134,143,144,145,146,147,],[-44,-78,-79,-45,-46,-72,-73,-74,-75,-76,135,136,137,138,-77,-63,162,164,166,168,170,]),'DO':([33,37,38,40,61,65,66,84,85,86,87,88,98,113,120,121,122,123,124,125,126,134,150,153,178,179,],[-44,-78,-79,-45,89,-55,-46,-72,-73,-74,-75,-76,-52,-77,-53,-47,-48,-49,-50,-51,-54,-63,177,180,198,199,]),'AND':([33,37,38,40,61,65,66,72,84,85,86,87,88,98,99,113,120,121,122,123,124,125,126,134,139,],[-44,-78,-79,-45,91,-55,-46,91,-72,-73,-74,-75,-76,91,91,-77,91,-47,-48,-49,-50,-51,-54,-63,91,]),'OR':([33,37,38,40,61,65,66,72,84,85,86,87,88,98,99,113,120,121,122,123,124,125,126,134,139,],[-44,-78,-79,-45,92,-55,-46,92,-72,-73,-74,-75,-76,92,92,-77,92,-47,-48,-49,-50,-51,-54,-63,92,]),'THEN':([33,37,38,40,65,66,72,84,85,86,87,88,98,113,120,121,122,123,124,125,126,134,],[-44,-78,-79,-45,-55,-46,112,-72,-73,-74,-75,-76,-52,-77,-53,-47,-48,-49,-50,-51,-54,-63,]),'ELSE':([48,140,],[-25,159,]),'STR':([68,70,74,135,136,137,138,],[102,107,114,107,107,107,107,]),'CHAR':([68,70,135,136,137,138,],[103,109,109,109,109,109,]),'TO':([101,128,],[127,152,]),'DOWNTO':([101,128,],[129,151,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'start':([0,],[1,]),'variable_block':([4,42,192,193,194,195,196,],[5,78,201,202,203,204,205,]),'empty':([4,5,16,22,23,24,25,26,27,28,34,41,42,118,162,163,164,165,166,167,168,169,170,171,192,193,194,195,196,208,209,210,211,212,],[6,11,29,29,29,29,29,29,29,29,29,77,6,11,77,77,77,77,77,77,77,77,77,77,6,6,6,6,6,11,11,11,11,11,]),'functions':([5,118,208,209,210,211,212,],[8,149,213,214,215,216,217,]),'variables':([7,13,],[12,19,]),'variable_describe':([7,13,],[13,13,]),'main_body':([8,78,89,112,159,177,180,198,199,201,202,203,204,205,],[15,118,119,140,181,197,200,206,207,208,209,210,211,212,]),'body':([16,22,23,24,25,26,27,28,34,],[21,49,50,51,52,53,59,60,71,]),'while_loop':([16,22,23,24,25,26,27,28,34,],[22,22,22,22,22,22,22,22,22,]),'for_loop':([16,22,23,24,25,26,27,28,34,],[23,23,23,23,23,23,23,23,23,]),'repeat_loop':([16,22,23,24,25,26,27,28,34,],[24,24,24,24,24,24,24,24,24,]),'if_statement':([16,22,23,24,25,26,27,28,34,],[25,25,25,25,25,25,25,25,25,]),'expression':([16,22,23,24,25,26,27,28,30,34,35,36,54,55,56,57,58,63,64,68,70,74,90,93,94,95,96,97,111,135,136,137,138,],[26,26,26,26,26,26,26,26,62,26,62,73,84,85,86,87,88,62,100,105,110,115,62,121,122,123,124,125,62,110,110,110,110,]),'var_assign':([16,22,23,24,25,26,27,28,34,],[27,27,27,27,27,27,27,27,27,]),'write':([16,22,23,24,25,26,27,28,34,],[28,28,28,28,28,28,28,28,28,]),'numeric':([16,22,23,24,25,26,27,28,30,34,35,36,54,55,56,57,58,63,64,68,70,74,90,93,94,95,96,97,111,135,136,137,138,],[37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,]),'function_call':([16,22,23,24,25,26,27,28,30,34,35,36,54,55,56,57,58,63,64,68,70,74,90,93,94,95,96,97,111,135,136,137,138,],[38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,]),'boolean_expression':([30,35,63,64,90,111,],[61,72,98,99,120,139,]),'function_variables':([41,162,163,164,165,166,167,168,169,170,171,],[76,182,183,184,185,186,187,188,189,190,191,]),'and_or':([61,72,98,99,120,139,],[90,90,90,90,90,90,]),'vars':([70,135,136,137,138,],[106,154,155,156,157,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> start","S'",1,None,None,None),
  ('start -> PROGRAM ID SEMICOLON variable_block functions main_body','start',6,'p_start','pascal_to_c.py',108),
  ('functions -> FUNCTION ID LPAREN function_variables RPAREN COLON INTEGER SEMICOLON variable_block main_body functions','functions',11,'p_functions','pascal_to_c.py',126),
  ('functions -> FUNCTION ID LPAREN function_variables RPAREN COLON REAL SEMICOLON variable_block main_body functions','functions',11,'p_functions','pascal_to_c.py',127),
  ('functions -> FUNCTION ID LPAREN function_variables RPAREN COLON BOOLEAN SEMICOLON variable_block main_body functions','functions',11,'p_functions','pascal_to_c.py',128),
  ('functions -> FUNCTION ID LPAREN function_variables RPAREN COLON STRING SEMICOLON variable_block main_body functions','functions',11,'p_functions','pascal_to_c.py',129),
  ('functions -> FUNCTION ID LPAREN function_variables RPAREN COLON CHARACTER SEMICOLON variable_block main_body functions','functions',11,'p_functions','pascal_to_c.py',130),
  ('functions -> PROCEDURE ID SEMICOLON variable_block main_body functions','functions',6,'p_functions','pascal_to_c.py',131),
  ('functions -> empty','functions',1,'p_functions','pascal_to_c.py',132),
  ('function_variables -> ID COLON INTEGER','function_variables',3,'p_function_variables','pascal_to_c.py',141),
  ('function_variables -> ID COLON REAL','function_variables',3,'p_function_variables','pascal_to_c.py',142),
  ('function_variables -> ID COLON BOOLEAN','function_variables',3,'p_function_variables','pascal_to_c.py',143),
  ('function_variables -> ID COLON STRING','function_variables',3,'p_function_variables','pascal_to_c.py',144),
  ('function_variables -> ID COLON CHARACTER','function_variables',3,'p_function_variables','pascal_to_c.py',145),
  ('function_variables -> ID COLON INTEGER COMMA function_variables','function_variables',5,'p_function_variables','pascal_to_c.py',146),
  ('function_variables -> ID COLON REAL COMMA function_variables','function_variables',5,'p_function_variables','pascal_to_c.py',147),
  ('function_variables -> ID COLON BOOLEAN COMMA function_variables','function_variables',5,'p_function_variables','pascal_to_c.py',148),
  ('function_variables -> ID COLON STRING COMMA function_variables','function_variables',5,'p_function_variables','pascal_to_c.py',149),
  ('function_variables -> ID COLON CHARACTER COMMA function_variables','function_variables',5,'p_function_variables','pascal_to_c.py',150),
  ('function_variables -> ID COLON INTEGER SEMICOLON function_variables','function_variables',5,'p_function_variables','pascal_to_c.py',151),
  ('function_variables -> ID COLON REAL SEMICOLON function_variables','function_variables',5,'p_function_variables','pascal_to_c.py',152),
  ('function_variables -> ID COLON BOOLEAN SEMICOLON function_variables','function_variables',5,'p_function_variables','pascal_to_c.py',153),
  ('function_variables -> ID COLON STRING SEMICOLON function_variables','function_variables',5,'p_function_variables','pascal_to_c.py',154),
  ('function_variables -> ID COLON CHARACTER SEMICOLON function_variables','function_variables',5,'p_function_variables','pascal_to_c.py',155),
  ('function_variables -> empty','function_variables',1,'p_function_variables','pascal_to_c.py',156),
  ('main_body -> BEGIN body END','main_body',3,'p_main_body','pascal_to_c.py',164),
  ('write -> WRITE LPAREN STR RPAREN SEMICOLON','write',5,'p_write','pascal_to_c.py',169),
  ('write -> WRITE LPAREN expression RPAREN SEMICOLON','write',5,'p_write','pascal_to_c.py',170),
  ('body -> while_loop body','body',2,'p_body','pascal_to_c.py',175),
  ('body -> for_loop body','body',2,'p_body','pascal_to_c.py',176),
  ('body -> repeat_loop body','body',2,'p_body','pascal_to_c.py',177),
  ('body -> if_statement body','body',2,'p_body','pascal_to_c.py',178),
  ('body -> expression body','body',2,'p_body','pascal_to_c.py',179),
  ('body -> var_assign body','body',2,'p_body','pascal_to_c.py',180),
  ('body -> write body','body',2,'p_body','pascal_to_c.py',181),
  ('body -> empty','body',1,'p_body','pascal_to_c.py',182),
  ('while_loop -> WHILE boolean_expression DO main_body','while_loop',4,'p_while_loop','pascal_to_c.py',188),
  ('for_loop -> FOR ID ASSIGN TO INT DO main_body','for_loop',7,'p_for_loop','pascal_to_c.py',193),
  ('for_loop -> FOR ID ASSIGN DOWNTO INT DO main_body','for_loop',7,'p_for_loop','pascal_to_c.py',194),
  ('for_loop -> FOR ID ASSIGN INT DOWNTO INT DO main_body','for_loop',8,'p_for_loop','pascal_to_c.py',195),
  ('for_loop -> FOR ID ASSIGN INT TO INT DO main_body','for_loop',8,'p_for_loop','pascal_to_c.py',196),
  ('repeat_loop -> REPEAT body UNTIL boolean_expression SEMICOLON','repeat_loop',5,'p_repeat_loop','pascal_to_c.py',201),
  ('if_statement -> IF boolean_expression THEN main_body','if_statement',4,'p_if_statement','pascal_to_c.py',206),
  ('if_statement -> IF boolean_expression THEN main_body ELSE main_body','if_statement',6,'p_if_statement','pascal_to_c.py',207),
  ('numeric -> INT','numeric',1,'p_numeric','pascal_to_c.py',215),
  ('numeric -> RE','numeric',1,'p_numeric','pascal_to_c.py',216),
  ('numeric -> ID','numeric',1,'p_numeric','pascal_to_c.py',217),
  ('boolean_expression -> expression LESSER expression','boolean_expression',3,'p_boolean_expression','pascal_to_c.py',222),
  ('boolean_expression -> expression LESSER_EQUAL expression','boolean_expression',3,'p_boolean_expression','pascal_to_c.py',223),
  ('boolean_expression -> expression EQUAL expression','boolean_expression',3,'p_boolean_expression','pascal_to_c.py',224),
  ('boolean_expression -> expression GREATER_EQUAL expression','boolean_expression',3,'p_boolean_expression','pascal_to_c.py',225),
  ('boolean_expression -> expression GREATER expression','boolean_expression',3,'p_boolean_expression','pascal_to_c.py',226),
  ('boolean_expression -> NOT boolean_expression','boolean_expression',2,'p_boolean_expression','pascal_to_c.py',227),
  ('boolean_expression -> boolean_expression and_or boolean_expression','boolean_expression',3,'p_boolean_expression','pascal_to_c.py',228),
  ('boolean_expression -> LPAREN boolean_expression RPAREN','boolean_expression',3,'p_boolean_expression','pascal_to_c.py',229),
  ('boolean_expression -> BOOL','boolean_expression',1,'p_boolean_expression','pascal_to_c.py',230),
  ('and_or -> AND','and_or',1,'p_and_or','pascal_to_c.py',243),
  ('and_or -> OR','and_or',1,'p_and_or','pascal_to_c.py',244),
  ('var_assign -> ID ASSIGN STR SEMICOLON','var_assign',4,'p_var_assign','pascal_to_c.py',249),
  ('var_assign -> ID ASSIGN CHAR SEMICOLON','var_assign',4,'p_var_assign','pascal_to_c.py',250),
  ('var_assign -> ID ASSIGN BOOL SEMICOLON','var_assign',4,'p_var_assign','pascal_to_c.py',251),
  ('var_assign -> ID ASSIGN expression SEMICOLON','var_assign',4,'p_var_assign','pascal_to_c.py',252),
  ('var_assign -> ID SEMICOLON','var_assign',2,'p_var_assign','pascal_to_c.py',253),
  ('function_call -> ID LPAREN vars RPAREN','function_call',4,'p_function_call','pascal_to_c.py',261),
  ('vars -> STR COMMA vars','vars',3,'p_vars','pascal_to_c.py',266),
  ('vars -> BOOL COMMA vars','vars',3,'p_vars','pascal_to_c.py',267),
  ('vars -> CHAR COMMA vars','vars',3,'p_vars','pascal_to_c.py',268),
  ('vars -> expression COMMA vars','vars',3,'p_vars','pascal_to_c.py',269),
  ('vars -> expression','vars',1,'p_vars','pascal_to_c.py',270),
  ('vars -> STR','vars',1,'p_vars','pascal_to_c.py',271),
  ('vars -> BOOL','vars',1,'p_vars','pascal_to_c.py',272),
  ('vars -> CHAR','vars',1,'p_vars','pascal_to_c.py',273),
  ('expression -> expression PLUS expression','expression',3,'p_binary_operators','pascal_to_c.py',281),
  ('expression -> expression MINUS expression','expression',3,'p_binary_operators','pascal_to_c.py',282),
  ('expression -> expression TIMES expression','expression',3,'p_binary_operators','pascal_to_c.py',283),
  ('expression -> expression DIVIDE expression','expression',3,'p_binary_operators','pascal_to_c.py',284),
  ('expression -> expression MOD expression','expression',3,'p_binary_operators','pascal_to_c.py',285),
  ('expression -> LPAREN expression RPAREN','expression',3,'p_binary_operators','pascal_to_c.py',286),
  ('expression -> numeric','expression',1,'p_binary_operators','pascal_to_c.py',287),
  ('expression -> function_call','expression',1,'p_binary_operators','pascal_to_c.py',288),
  ('variable_block -> empty','variable_block',1,'p_variable_block','pascal_to_c.py',307),
  ('variable_block -> VAR variables','variable_block',2,'p_variable_block','pascal_to_c.py',308),
  ('variables -> variable_describe','variables',1,'p_variables','pascal_to_c.py',314),
  ('variables -> variable_describe variables','variables',2,'p_variables','pascal_to_c.py',315),
  ('variable_describe -> ID COLON STRING SEMICOLON','variable_describe',4,'p_variable_describe','pascal_to_c.py',323),
  ('variable_describe -> ID COLON INTEGER SEMICOLON','variable_describe',4,'p_variable_describe','pascal_to_c.py',324),
  ('variable_describe -> ID COLON CHARACTER SEMICOLON','variable_describe',4,'p_variable_describe','pascal_to_c.py',325),
  ('variable_describe -> ID COLON BOOLEAN SEMICOLON','variable_describe',4,'p_variable_describe','pascal_to_c.py',326),
  ('variable_describe -> ID COLON REAL SEMICOLON','variable_describe',4,'p_variable_describe','pascal_to_c.py',327),
  ('empty -> <empty>','empty',0,'p_empty','pascal_to_c.py',332),
]
