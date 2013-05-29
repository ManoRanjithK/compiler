

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

symtable_type symtable;
symtable_type vartable;

symtable_type *class_table = &symtable;
symtable_type *var_table = &vartable;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

class_tree_node find_lca( class_tree_node x, class_tree_node y)
{
	if ( !x || !y)
	{
		return NULL;
	}

	int depth = x->depth < y->depth ? x->depth : y->depth;
	while ( x && x->depth != depth)
	{
		x = x->father;
	}
	while ( y && y->depth != depth)
	{
		y = y->father;
	}

	while ( x && y && x != y)
	{
		x = x->father;
		y = y->father;
	}

	return x ? y : NULL;
}

static class_tree_node union_set( class_tree_node first, class_tree_node second)
{
	first = first->find_set();
	second = second->find_set();
	if ( first == second)
	{
		return NULL;
	}

	class_tree_node new_root = first->set_rank < second->set_rank ? second : first;

	new_root->set_size = first->set_size + second->set_size;
	new_root->set_rank += first->set_rank == second->set_rank;
	first->set_head = second->set_head = new_root;

	return new_root;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
	class_table->enterscope();

	install_basic_classes();

	for ( int i = classes->first(); classes->more( i); i = classes->next( i))
	{
		Type cur = classes->nth(i);
		class_tree_node ct_node = lookup_install_type( cur->name);

		if ( ct_node->contain == NULL)
		{
			ct_node->set_contain( cur);
		}
		else
		{
			// Find error: Redefinition of class
			semant_error( cur) << "Redefinition of Class " << cur->name;
			return;
		}

		class_tree_node father_node = lookup_install_type( cur->parent);

		if ( !ct_node->set_father( father_node))
		{
			// Find error: cur could not be a subclass of father node.
			semant_error( cur) << "Find inherit circle of Class " << cur->name
				<< " and Class " << cur->parent;
			return;
		}
	}

	class_tree_node root = class_table->probe( Object);
	if ( !root)
	{
		// Find bug: No Object Class!
		return;
	}

	root->walk_down();

	class_table->exitscope();
}

bool class_tree_node_type::walk_down()
{
	var_table->enter_scope();
	var_table->addid( self, this);

	bool ret = is_defined() && this->contain->check_Class_Types();

	class_tree_node leg = this->son;
	while ( leg && ret)
	{
		ret = leg->walk_down();
		leg = leg->sibling;
	}

	var_table->exit_scope();
	return ret;
}

Type Object_type = NULL;
Type IO_type = NULL;
Type Int_type = NULL;
Type Bool_type = NULL;
Type Str_type = NULL;
Type No_type = NULL;

Type Self_type = NULL;
Type Current_type = NULL;

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object,
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
	class_(IO,
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
	class_(Str,
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat,
								      single_Formals(formal(arg, Str)),
								      Str,
								      no_expr()))),
			       single_Features(method(substr,
						      append_Formals(single_Formals(formal(arg, Int)),
								     single_Formals(formal(arg2, Int))),
						      Str,
						      no_expr()))),
	       filename);
    Class_ Self_class = class_( SELF_TYPE, Object, nil_Features(), filename);

    ::Object_type = new class_tree_node( Object_class);

    ::IO_type = new class_tree_node( IO_class);
    ::IO_type->set_father( Object_type);

    ::Int_type = new class_tree_node( Int_class);
    ::Int_type->set_father( Object_type);

    ::Bool_type = new class_tree_node( Bool_class);
    ::Bool_type->set_father( Object_type);

    ::Str_type = new class_tree_node( Str_class);
    ::Str_type->set_father( Object_type);

    ::Self_type = new class_tree_node( Self_class);

    symtable.addid( Object, ::Object_type);
    symtable.addid( IO, ::IO_type);
    symtable.addid( Int, ::Int_type);
    symtable.addid( Bool, ::Bool_type);
    symtable.addid( Str, ::Str_type);
    symtable.addid( SELF_TYPE, ::Self_type);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}

void class__class::collect_Methods()
{
	for ( int i = features->first(); features->more( i); i = features->next( i))
	{
		features->nth( i)->collect_Feature_Types();
	}
}

bool class__class::check_Class_Types()
{
	for ( int i = features->first(); features->more( i); i = features->next( i))
	{
		Feature ft = features->nth( i);
		ft->install_Feature_Types();
	}

	for ( int i = features->first(); features->more( i); i = features->next( i))
	{
		Feature ft = features->nth( i);
		if ( !ft->check_Feature_Types())
		{
			return false;
		}
	}

	return true;
}

void method_class::collect_Feature_Types()
{
	feature_type = class_table->lookup( return_type);
	List<class_tree_node_type> *syms = new List<class_tree_node_type>( feature_type, NULL);
	for ( int i = formals.first(); formals->more( i); i = formals->next( i))
	{
		Type type = formals->nth( i)->collect_Formal_Type();
		syms = new List<class_tree_node_type>( type, syms);
	}

	method_table->addid( name, syms);
}

void method_class::install_Feature_Types()
{
}

bool method_class::check_Feature_Types()
{
	var_table->enter_scope();
	for ( int i = formals.first(); formals->more( i); i = formals->next( i))
	{
		Formal fm = formals->nth( i);
		if ( !fm->check_Formal_Type())
		{
			return false;
		}

		fm->install_Formal_Type();
	}

	Type type = feature_type;
	Type body_type = expr->get_Expr_Type();

	var_table->exit_scope();

	return type && type->is_defined() && body_type && body_type->is_subtype_of( type);
}

void attr_class::collect_Feature_Types()
{
}

bool attr_class::check_Feature_Types()
{
	Type type = feature_type;
	Type t2 = init->is_no_expr() ? init->get_Expr_Type() : type;

	return type && type->is_defined() && t2 && t2->is_subtype_of( type);
}

void attr_class::install_Feature_Types()
{
	feature_type = lookup_install_type( type_decl);
	var_table->addid( name, feature_type);
}

Type formal_class::collect_Formal_Type()
{
	type = val_table->probe( name);
	if ( type == NULL)
	{
		type = lookup_install_type( type_decl);
	}
	else
	{
		type = No_type;
	}

	return type;
}

bool formal_class::check_Formal_Type()
{
	return type && type->is_defined();
}

void formal_class::install_Formal_Type()
{
	var_table->addid( name, type);
}

Type branch_class::check_Case_Type( Type path_type)
{
	Type id_type = class_table->lookup( type_decl);
	Type ret = No_type;

	if ( id_type && id_type->is_defined() &&
			( id_type->is_subtype_of( path_type)
			  || path_type->is_subtype_of( id_type)
			)
	   )
	{
		var_table->enter_scope();
		var_table->addid( name, id_type);

		ret = expr->get_Expr_Type();

		var_table->exit_scope();
	}

	return ret;
}

