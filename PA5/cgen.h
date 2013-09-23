#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   void code_prototypes();
   void code_classnametab();
   void code_disptabs();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


SymbolTable< Symbol, Entry> method_table;

struct class_method_list;

class CgenNode : public class__class {
private:
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int object_size;
   class_method_list *method_list;
   int dispatch_table_size;

   static int class_count;

   StringEntry *class_name_entry;

   void count_Features();

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   const int class_tag;

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   void code_prototype( ostream &str);
   void code_classnameentry( ostream &str);
   void walk_down_code_disptab( ostream &str);

   static void set_class_count( int count) { CgenNode::class_count = count;}
};

class BoolConst
{
 private:
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

struct class_method_list
{
	private:
	Symbol name;
	class_method_list *next;

	public:
	class_method_list( Symbol nt, class_method_list *nn = NULL) : name( nt), next( nn) {}

	Symbol hd() const { return name;}
	class_method_list *tl() const { return next;}

	void set_hd( Symbol nt) { name = nt;}
	void set_tl( class_method_list *nn) { next = nn;}
};

