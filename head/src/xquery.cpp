/***************************************************************************
 *   Copyright (C) 2005 by Benjamin Piwowarski                             *
 *   bpiwowar@piwo                                                         *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <cstdlib>
#include "xquerygrammar.h"

using namespace std;

int main(int argc, char *argv[])
{
   if (argc != 2) { std::cerr << argv[0] << " takes one argument: the query" << endl; exit(1);
   }
   string str = argv[1];
   
   XQueryGrammar grammar;
   XQuerySkip skipper;
   
   IteratorScanner<std::string::const_iterator> scanner(str.begin(),str.end());
   scanner.set_skipper(skipper.start());
   scanner.context.do_generate_AST(true);
   
   bool b = grammar.parse(scanner);
   if (b && scanner.at_end()) {
   cout << "-------------------------\n";
   cout << "Parsing succeeded for \n";
   cout << "-------------------------\n";
   cout << str << endl;
   cout << "-------------------------\n";
   
   scanner.context.print_parse_tree(cout, scanner);
   
   cout << "--------- Generating the query\n";
   ASTScanner ast_scanner(scanner.context.current);
   XQueryWalker walker;
   b = walker.parse(ast_scanner);
   if (b && ast_scanner.at_end()) cout << "OK\n"; else cout << "Error\n";
   
   
   } else {
      cout << "-------------------------\n";
      cout << " /!\\ Parsing failed for\n";
      cout << "-------------------------\n";
      cout << str << endl;
      cout << "-------------------------\n";
      cout << "around\n";
      cout << "-------------------------\n";
      scanner.print_context(cout,scanner.get_pos(),10);
      cout << "\n-------------------------\n";
   }
   return EXIT_SUCCESS;
}
