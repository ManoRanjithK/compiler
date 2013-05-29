#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <utility>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

struct class_tree_node_type;

typedef class_tree_node Type;

typedef List<class_tree_node_type> class_method_type;
typedef class_tree_node_type *class_tree_node;
typedef class_method_type *class_method;

typedef SymbolTable< Symbol, class_tree_node_type> symtable_type;
typedef SymbolTable< Symbol, class_method_type> method_table_type;

extern Type Object_type;
extern Type Int_type;
extern Type Bool_type;
extern Type Str_type;
extern Type No_type;
extern Type Self_type;
extern Type Current_type;

// Env vars.
extern symtable_type *class_table;
extern symtable_type *var_table;

Type lookup_install_type( Symbol name)
{
	   Type type = class_table->lookup( name);
	   if ( type == NULL)
	   {
		   type = new Type( NULL);
		   class_table->addid( name, type);
	   }

	   return type;
}

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};

struct class_tree_node_type {
	class_tree_node set_head;
	int set_rank;
	int set_size;

	class_tree_node father;
	class_tree_node son;
	class_tree_node sibling;
	Type contain;
	int depth;

	method_table_type method_table;

	class_tree_node find_set()
	{
		return set_head == this ? this : set_head = set_head->find_set();
	}

	friend class_tree_node union_set( class_tree_node, class_tree_node);

	class_tree_node_type( Type class_) :
		set_head( this), set_rank( 0), set_size( 1),
		contain( class_), depth( 0),
		father( NULL), son( NULL), sibling( NULL)
	{
		method_table.enter_scope();
	}

	~class_tree_node_type()
	{
		method_table.exit_scope();
	}

	bool set_father( class_tree_node father)
	{
		this->father = father;
		this->depth = father->depth + 1;
		this->sibling = father->son;
		father->son = this;

		return union_set( this, father);
	}

	void set_contain( class_tree_node contain)
	{
		this->contain = contain;
		return contain->collect_Features( &method_table, attr_table);
	}

	bool is_subtype_of( class_tree_node super) const
	{
		if ( !this->contain || !super->contain)
		{
			return false;
		}

		class_tree_node leg = this;
		while ( leg->depth > super->depth)
		{
			leg = leg->father;
		}

		return leg == super;
	}

	Type defined() const
	{
		return contain ? this : No_type;
	}

	bool is_defined() const
	{
		return defined() != No_type;
	}

	class_method find_method( Symbol name) const
	{
		class_method ret = method_table.lookup( name);
		return ret ? ret : ( father ? father->find_method( name) : NULL);
	}

	friend class_tree_node find_lca( class_tree_node, class_tree_node);

	bool walk_down();
};

static class_tree_node union_set( class_tree_node first, class_tree_node second);

#endif

