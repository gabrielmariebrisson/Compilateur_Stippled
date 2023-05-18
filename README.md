# Projet de Compilation - Création d'un compilateur Stippled

Ce projet est réalisé dans le cadre du cours de Compilation de la Licence Informatique à l'Université Bordeaux. L'objectif est de développer un parseur et un analyseur pour le langage Stippled, un langage de programmation de dessin. 

## Intructions 

1. Compilez le projet avec 
''' bash
 make 
'''
2. Pour lancer les programmes
    - Visualisateur (AST):
     ''' bash
      ./visualiser.out  programs/good_examples/arg_list.stippled 
    '''
    <img src="readme/visualiser" width="300px">

    - Interpreter et Analyseur:
     ''' bash
      ./interpreter.out  programs/good_examples/variables.stippled 
    '''
    <img src="readme/interpreter" width="300px">



### Partie principale

- Un lexeur (dans parser/Lexer.mll).
- Un parseur (dans parser/Parser.mly).
- Un analyseur sémantique (dans analyser) comprenant un analyseur de type et un simplificateur d'AST.

## Auteurs

- Lucy-jean Curtis
- Clément Delmas
- Gabriel Marie-brisson