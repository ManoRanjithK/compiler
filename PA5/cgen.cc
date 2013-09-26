
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

CgenClassTableP global_table = NULL;
CgenNodeP global_node = NULL;

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_pop( char *reg, ostream& str)
{
	emit_addiu( SP, SP, 4, str);
	emit_load( reg, 0, SP, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_func_call_before( int temp_size, ostream &s)
{
	emit_store( FP, 0, SP, s);
	emit_store( SELF, 4, SP, s);
	emit_store( RA, 8, SP, s);
	emit_addiu( SP, SP, -12 - temp_size, s);
	emit_addiu( FP, SP, 4, s);
	emit_move( SELF, ACC, s);
}

static void emit_func_call_after( int temp_size, ostream &s)
{
	emit_move( ACC, SELF, s);
	emit_addiu( SP, SP, 12 + temp_size, s);
	emit_load( RA, 0, SP, s);
	emit_load( SELF, 4, SP, s);
	emit_load( FP, 8, SP, s);

	emit_return( s);
}

static void emit_new( Symbol name, ostream &s)
{
	emit_partial_load_address( ACC, s); emit_protobj_ref( name, s);
	s << JAL; emit_method_ref( Object, copy, s);
}

int expr_is_const = 0;
int object_offset = 0;
char *object_base_reg = NULL;

static void lookup_var( Symbol name)
{
	object_offset = ( int) ( ::method_var_table->lookup( name));
	if ( !object_offset)
	{
		object_offset = ( int) ( ::var_table->lookup( name));
		if ( object_offset < 0)
		{
			object_base_reg = reg_S[-object_offset];
			object_offset = 0;
		}
		else
		{
			object_base_reg = SELF;
		}
	}
	else
	{
		object_offset -= DEFAULT_OBJFIELDS;
		object_base_reg = FP;
	}
}

static void emit_func_call( Symbol class_name, Symbol method_name, ostream &s)
{
	CgenNodeP node = global_table->lookup( class_name);
	int offset = ( ( int)( node->method_offset_table.lookup( method_name))) - DEFAULT_OBJFIELDS;

	emit_push( ACC, s);
	emit_partial_load_address( ACC, s); emit_disptable_ref( class_name, s);
	emit_load( ACC, offset << 2, ACC, s);
	emit_jalr( ACC, s);
	emit_pop( ACC, s);
}

static void emit_not( char *dest_reg, char *soruce_reg, ostream &s)
{
	s << NOT << dest_reg << " " << source_reg << endl;
}

static void emit_slt( char *dest_ret, char *src0_reg, char *src1_reg, ostream &s)
{
	s << SLT << dest_reg << " " << src0_reg << " " << src1_reg << endl;
}

static void emit_nor( char *dest_reg, char *soruce_reg, ostream &s)
{
	s << NOR << dest_reg << " " << source_reg << endl;
}

static void emit_xor( char *dest_reg, char *soruce_reg, ostream &s)
{
	s << XOR << dest_reg << " " << source_reg << endl;
}

static void emit_sll( char *dest_reg, char *soruce_reg, int bits, ostream &s)
{
	s << SLL << dest_reg << " " << source_reg << << " " << bits << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD; emit_disptable_ref(Str, s); s << endl          // dispatch table
      << WORD;  lensym->code_ref(s);  s << endl;              // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; emit_disptable_ref(Int, s); s << endl      // dispatch table
      << WORD << str << endl;                             // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD; emit_disptable_ref(Bool, s); s << endl       // dispatch table
      << WORD << val << endl;                               // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL
      << WORD << stringclasstag << endl;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_prototypes()
{
	for ( List<CgenNode> *leg = ordered_nds; leg; leg = leg->tl())
	{
		leg->hd()->code_prototype( str);
	}
}

void CgenClassTable::code_classnametab()
{
	str << CLASSNAMETAB << LABEL;
	for ( List<CgenNode> *leg = ordered_nds; leg; leg = leg->tl())
	{
		leg->hd()->code_classnameentry( str);
	}
}

void CgenClassTable::code_classobjtab()
{
	str << CLASSOBJTAB << LABEL;
	for ( List<CgenNode> *leg = ordered_nds; leg; leg = leg->tl())
	{
		leg->hd()->code_classobjentry( str);
	}
}

void CgenClassTable::code_disptabs()
{
	for ( List<CgenNode> *leg = ordered_nds; leg; leg = leg->tl())
	{
		leg->hd()->code_disptab( str);
	}
}

void CgenClassTable::code_initializers()
{
	for ( List<CgenNode> *leg = ordered_nds; leg; leg = leg->tl())
	{
		leg->hd()->code_initializer( str);
	}
}

void CgenClassTable::code_class_methods()
{
	for ( List<CgenNode> *leg = ordered_nds; leg; leg = leg->tl())
	{
		leg->hd()->code_class_methods( str);
	}
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s), ordered_nds(NULL)
{
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();

   stringclasstag = lookup( Str)->class_tag;
   intclasstag = lookup( Int)->class_tag;
   boolclasstag = lookup( Bool)->class_tag;

   install_classes(classes);

   build_inheritance_tree();

   CgenNode::set_class_count( 0);
   root()->walk_down();

   code();

   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  global_table = this;
  CgenNode::set_class_count( -3);

  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

//
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object,
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

//
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer.
//
   install_class(
    new CgenNode(
     class_(Int,
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
  {
      set_relations(l->hd());
      ordered_nds = new List<CgenNode>( l->hd(), ordered_nds);
  }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

int CgenNode::class_count = 0;

SymbolTable< Symbol, void> global_method_var_table;
SymbolTable< Symbol, void> *var_table;
SymbolTable< Symbol, void> *method_var_table = &global_method_var_table;

void CgenNode::count_Features()
{
	for ( int i = features->first(); features->more( i); i = features->next( i))
	{
		Symbol name = features->nth( i)->get_name();

		if ( features->nth( i)->is_method())
		{
			if ( !this->method_table.lookup( name))
			{
				this->method_offset_table.addid( name,
						( void *)( dispatch_table_size++ + DEFAULT_OBJFIELDS));
				method_list->set_tl( new class_method_list( name));
				method_list = method_list->tl();

			}
			this->method_table.addid( name, get_name());
		}
		else
		{
			this->member_offset_table.addid( name,
					( void *)( object_size++ + DEFAULT_OBJFIELDS));
		}
	}
}

void CgenNode::code_disptab( ostream &str)
{
	emit_disptable_ref( get_name(), str); str << LABEL;

	class_method_list *methods = method_list;
	while ( methods)
	{
		Symbol method_id = methods->hd();
		Symbol class_id = method_table.lookup( method_id);

		str << WORD; emit_method_ref( class_id, method_id, str);

		methods = methods->tl();
	}
}

void CgenNode::walk_down()
{
	class_tag = class_count++;

	class_method_list *method_list_head = new class_method_list( NULL, NULL);
	method_list = method_list_head;

	if ( get_name() != Object)
	{
		dispatch_table_size = parentnd->dispatch_table_size;
		object_size = parentnd->object_size;

		this->member_offset_table = p->member_offset_table;
		this->method_offset_table = p->method_offset_table;
		this->method_table = p->method_table;

		class_method_list *inhe_methods = parentnd->method_list;
		while ( inhe_methods)
		{
			method_list->set_tl( new class_method_list( inhe_methods->hd()));
			method_list = method_list->tl();
			inhe_methods = inhe_methods->tl();
		}
	}
	this->member_offset_table.enterscope();
	this->method_offset_table.enterscope();
	this->method_table.enterscope();

	count_Features();

	method_list = method_list_head->tl();
	// delete method_list_head;

	max_class_tag = class_tag;
	for ( List<CgenNode> *leg = children; leg; leg = leg->tl())
	{
		leg->hd()->walk_down();
		max_class_tag = max( max_class_tag, leg->hd()->max_class_tag);
	}
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenNode::code_classnameentry( ostream &str)
{
	str << WORD; class_name_entry->code_ref( str);
}

void CgenNode::code_classobjentry( ostream &str)
{
	str << WORD; emit_protobj_ref( get_name(), str);
	str << WORD; emit_init_ref( get_name(), str);
}

void CgenNode::code_prototype( ostream &str)
{
	str << WORD << "-1" << endl;
	emit_protobj_ref( get_name(), str); str << LABEL
		<< WORD << class_tag << endl
		<< WORD << ( DEFAULT_OBJFIELDS + object_size) << endl;
	emit_disptable_ref( get_name(), str);

	for ( List< CgenNode> *leg = children; leg; leg = leg->tl())
	{
		char *prefix = "";
		if ( leg->hd()->basic() == Basic)
		{
			switch ( leg->hd()->class_tag)
			{
				case 0:
					// Object member.
					break;
				case 1:
					// IO member.
					break;
				case 2:
					// Int member.
					prefix = INTCONST_PREFIX;
					break;
				case 3:
					// Bool member.
					prefix = BOOLCONST_PREFIX;
					break;
				case 4:
					// String member.
					prefix = STRCONST_PREFIX;
					break;
				default:
					if ( cgen_debug)
					cout << "BUG: a Basic class is not int, bool or string." << endl;
			}
		}
		str << WORD << prefix << "0" << endl;
	}
}

void CgenNode::code_initializer( ostream &str)
{
	global_node = this;
	int cnt = 0;
	for ( int i = features->first(); features->more( i); i = features->next( i))
	{
		if ( !features->nth( i)->is_method())
		{
			cnt = max( features->nth( i)->get_temp_size(), cnt)
		}
	}

	emit_init_ref( get_name(), str); str << LABEL;
	emit_func_call_before( cnt, str);

	if ( get_name() != Object)
	{
		str << JAL; emit_init_ref( parentnd->get_name(), str);
	}

	::var_table = &( this->member_offset_table);
	for ( int i = features->first(); features->more( i); i = features->next( i))
	{
		if ( !features->nth( i)->is_method())
		{
			features->nth( i)->code( str);
		}
	}
	::var_table = NULL;

	emit_func_call_after( cnt, str);
}

void CgenNode::code_class_methods( ostream &str)
{
	global_node = this;
	::var_table = &( this->member_offset_table);
	for ( int i = features->first(); features->more( i); i = features->next( i))
	{
		if ( features->nth( i)->is_method())
		{
			emit_method_ref( get_name(), "", s);
			features->nth( i)->code( str);
		}
	}
	::var_table = NULL;
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables

  if (cgen_debug) cout << "coding prototypes" << endl;
  code_prototypes();

  if (cgen_debug) cout << "coding class name tables" << endl;
  code_classnametab();

  if (cgen_debug) cout << "coding class object tables" << endl;
  code_classobjtab();

  if (cgen_debug) cout << "coding dispatch tables" << endl;
  code_disptabs();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

  if (cgen_debug) cout << "coding initializers" << endl;
  code_initializers();

  if (cgen_debug) cout << "coding class methods" << endl;
  code_class_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus),
   class_tag( class_count++),
   object_size( 0),
   dispatch_table_size( 0)
{
   class_name_entry = stringtable.lookup(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void attr_class::code( ostream &s) {
	init->code( s);
	int offset = ( int)( ::var_table->lookup( get_name()));
	emit_store( ACC, offset << 2, SELF, str);
}

int attr_class::get_temp_size() {
	return init->get_temp_size();
}

void method_class::code( ostream &s) {
	init_alloc_temp();
	int temps = get_temp_size();
	method_var_table->enterscope();

	s << name << LABEL << endl;

	emit_func_call_before( temps, s);

	int cnt = DEFAULT_OBJFIELDS;
	for ( int i = formals->first(); formals->more( i); i = formals->next( i))
	{
		// TODO: should use a get_name method here.
		method_var_table->addid( formals->nth( i)->name, ( void *)( cnt++));
	}
	body->code( s);

	emit_func_call_after( temps, s);

	method_var_table->exitscope();
}

int method_class::get_temp_size() {
	return formals->len() + body->get_temp_size();
}

void assign_class::code(ostream &s) {
	expr->code( s);
	lookup_var( name);
	emit_store( source_reg, object_offset << 2, object_base_reg, s);
}

int assign_class::get_temp_size() {
	return expr->get_temp_size();
}

void static_dispatch_class::code(ostream &s) {
	for ( int i = actual->first(); actual->more( i); i = actual->next( i))
	{
		actual->nth( i)->code( s);
		emit_push( ACC, s);
	}
	expr->code( s);
	emit_func_call( type_name, name, s);
}

int static_dispatch_class::get_temp_size() {
	int ret = expr->get_temp_size();
	for ( int i = actual->first(); actual->more( i); i = actual->next( i))
	{
		ret = max( ret, actual->nth( i)->get_temp_size());
	}
	return ret;
}

void dispatch_class::code(ostream &s) {
	Symbol type_name = expr->get_type();

	for ( int i = actual->first(); actual->more( i); i = actual->next( i))
	{
		actual->nth( i)->code( s);
		emit_push( ACC, s);
	}
	expr->code( s);
	emit_func_call( type_name, name, s);
}

int dispatch_class::get_temp_size() {
	int ret = expr->get_temp_size();
	for ( int i = actual->first(); actual->more( i); i = actual->next( i))
	{
		ret = max( ret, actual->nth( i)->get_temp_size());
	}
	return ret;
}

void cond_class::code(ostream &s) {
	int else_label = new_label();
	int end_lable = new_label();

	pred->code( s);
	emit_beqz( ACC, else_label, s);
	then_expr->code( s);
	emit_branch( end_label, s);
	emit_label_def( else_label, s);
	else_expr->code( s);
	emit_label_def( end_label, s);
}

int cond_class::get_temp_size() {
	return max( pred->get_temp_size(),
			max( then_expr->get_temp_size(), else_expr->get_temp_size()));
}

void loop_class::code(ostream &s) {
	int cond_label = new_label();
	int end_label = new_label();

	emit_label_def( cond_label, s);
	pred->code( s);
	emit_beqz( ACC, end_label, s);
	body->code( s);
	emit_branch( cond_label, s);
	emit_label_def( end_label, s);

	// Return void dear.
	emit_move( ACC, ZERO, s);
}

int loop_class::get_temp_size() {
	return max( pred->get_temp_size(), body->get_temp_size());
}

void typcase_class::code(ostream &s) {
	expr->code( s);
	emit_load( ACC, TAG_OFFSET, ACC, s);

	int last_label = new_label();
	emit_bne( ACC, ZERO, last_label, s);
	emit_load_string( ACC, stringtable.lookup_string( global_node->filename->get_string()), s);
	emit_jal( CASEABORT2, s);

	clear_vec();
	for ( int i( cases->first()); cases->more( i); i = cases->next( i))
	{
		// TODO: should use a get_type here.
		Symbol type = cases->nth( i)->type_decl;

		CgenNodeP class_node = global_table->lookup( type);
		push_vec( class_node->get_class_tag(),class_node->get_max_class_tag(), i);
	}

	sort_vec();

	int temp = alloc_temp();

	int x, y, c, cur_label, next_label = new_label();
	for ( init_vec(); next_vec(); fetch_vec( x, y, c),
			cur_label = next_label, next_label = new_label())
	{
		emit_label_def( cur_lable, s);
		emit_blti( ACC, x, next_label, s);
		emit_bgti( ACC, y, next_label, s);

		Cases_class cases = cases->nth( c);
		// TODO: lots of getters.
		method_var_table->enterscope();
		method_var_table->addid( cases->name, ( void *)( temp + DEFAULT_OBJFIELDS)));
		cases->expr->code( s);
		method_var_table->exitscope();
		emit_branch( last_label, s);
	}

	emit_label_def( next_label, s);
	emit_jal( CASEABORT, s);

	emit_label_def( last_label, s);
}

int typcase_class::get_temp_size() {
	int cnt = expr->get_temp_size();
	for ( int i( cases->first()); cases->more( i); i = cases->next( i))
	{
		cnt = max( cnt, cases->nth( i)->expr->get_temp_size() + 1);
	}

	return cnt;
}

void block_class::code(ostream &s) {
	body->code( s);
}

int block_class::get_temp_size() {
	return body->get_temp_size();
}

void let_class::code(ostream &s) {
	init->code( s);
	int offset = alloc_temp();
	emit_store( ACC, offset << 2, FP, s);
	method_var_table->enterscope();
	method_var_table->addid( identifier, ( void *)( offset + DEFAULT_OBJFIELDS));
	body->code( s);
	method_var_table->exitscope();
}

int let_class::get_temp_size() {
	return max( init->get_temp_size(), body->get_temp_size() + 1);
}

#define ARITH_CODE( cmd, s)\
{\
	e1->code( s);\
	int e1_is_const = expr_is_const;\
	emit_push( S1, s);\
	emit_move( S1, ACC, s);\
	e2->code( s);\
	int e2_is_const = expr_is_const;\
	emit_move( T0, ACC, s);\
	if ( e2_is_const)\
	{\
		if ( !e1_is_const)\
		{\
			emit_move( ACC, S1, s);\
		}\
		else\
		{\
			emit_push( T0, s);\
			emit_new( Int, s);\
			emit_pop( T0, s);\
		}\
	}\
	emit_fetch_int( S1, S1, s);\
	emit_fetch_int( T0, T0, s);\
	emit_##cmd( T0, S1, T0, s);\
	emit_store_int( T0, ACC, s);\
	emit_pop( S1);\
	expr_is_const = 0;\
}

void plus_class::code(ostream &s) {
	ARITH_CODE( add, s);
}

int plus_class::get_temp_size() {
	return max( e1->get_temp_size(), e2->get_temp_size() + 1);
}

void sub_class::code(ostream &s) {
	ARITH_CODE( sub, s);
}

int sub_class::get_temp_size() {
	return max( e1->get_temp_size(), e2->get_temp_size() + 1);
}

void mul_class::code(ostream &s) {
	ARITH_CODE( mul, s);
}

int mul_class::get_temp_size() {
	return max( e1->get_temp_size(), e2->get_temp_size() + 1);
}

void divide_class::code(ostream &s) {
	ARITH_CODE( div, s);
}

int divide_class::get_temp_size() {
	return max( e1->get_temp_size(), e2->get_temp_size() + 1);
}

void neg_class::code(ostream &s) {
	e1->code( s);
	emit_fetch_int( T0, ACC, s);
	emit_neg( T0, T0, s);
	if ( expr_is_const)
	{
		emit_push( T0, s);
		emit_new( Int, s);
		emit_pop( T0, s);
	}
	emit_sotre_int( T0, ACC, s);
	expr_is_const = 0;
}

int neg_class::get_temp_size() {
	return e1->get_temp_size();
}

void lt_class::code(ostream &s) {
	e1->code( s);
	emit_push( S1, s);
	emit_fetch_int( S1, ACC, s);
	e2->code( s);
	emit_fetch_int( ACC, ACC, s);
	emit_slt( ACC, S1, ACC, s);
	emit_pop( S1, s);
}

int lt_class::get_temp_size() {
	return max( e1->get_temp_size(), e2->get_temp_size() + 1);
}

void eq_class::code(ostream &s) {
	e1->code( s);
	emit_push( ACC, s);
	e2->code( s);
	emit_pop( T0, s);

	// This is a bool comparision.
	if ( this->e1->get_type() == Bool)
	{
		emit_xor( ACC, ACC, T0, s);
		emit_not( ACC, ACC, s);
	}
	else
	{
		emit_move( T1, ACC, s);
		emit_move( A1, ZERO, s);
		emit_load_imm( ACC, 1, s);
		emit_jal( EQUALITY_TEST, s);
	}

	/*
	int true_branch = new_label();
	int false_branch = new_label();
	int int_test_branch = new_label();
	int str_test_branch = new_label();
	int end_branch = new_label();

	emit_beq( ACC, T0, true_branch, s);

	emit_load( T1, TAG_OFFSET, T0, s);
	emit_load( T2, TAG_OFFSET, ACC, s);

	emit_bne( T1, T2, false_branch, s);

	emit_load_imm( T2, global_table->lookup( Int)->get_class_tag(), s);
	emit_beq( T1, T2, int_test_branch, s);
	emit_load_imm( T2, global_table->lookup( Str)->get_class_tag(), s);
	emit_beq( T1, T2, str_test_branch, s);
	emit_load_imm( T2, global_table->lookup( Bool)->get_class_tag(), s);
	emit_bne( T1, T2, false_branch, s);
	emit_bne( ACC, T0, false_branch, s);

	emit_label_def( true_branch, s);
	emit_load_imm( ACC, 1, s);
	emit_branch( end_branch, s);

	emit_label_def( int_test_branch, s);
	emit_label_def( str_test_branch, s);

	emit_label_def( false_branch, s);
	emit_move( ACC, ZERO, s);

	emit_label_def( end_branch, s);
	*/
}

int eq_class::get_temp_size() {
	return max( e1->get_temp_size(), e2->get_temp_size() + 1);
}

void leq_class::code(ostream &s) {
	e1->code( s);
	emit_push( S1, s);
	emit_fetch_int( S1, ACC, s);
	e2->code( s);
	emit_fetch_int( ACC, ACC, s);
	emit_slt( ACC, ACC, S1, s);
	emit_not( ACC, ACC, s);
	emit_pop( S1, s);
}

int leq_class::get_temp_size() {
	return max( e1->get_temp_size(), e2->get_temp_size() + 1);
}

void comp_class::code(ostream &s) {
	e1->code( s);
	emit_not( ACC, ACC, s);
}

int comp_class::get_temp_size() {
	return e1->get_temp_size();
}

void int_const_class::code(ostream& s)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
  expr_is_const = 1;
}

int int_const_class::get_temp_size() {
	return 0;
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

int string_const_class::get_temp_size() {
	return 0;
}

void bool_const_class::code(ostream& s)
{
	emit_load_imm( ACC, val, s);
}

int bool_const_class::get_temp_size() {
	return 0;
}

void new__class::code(ostream &s) {
	if ( type_name = SELF_TYPE)
	{
		// Calcu address
		emit_load( T0, TAG_OFFSET, SELF, s);
		emit_sll( T0, T0, 3, s);

		emit_push( S1, s);
		emit_load_address( S1, CLASSOBJTAB, s);
		emit_addu( S1, T0, S1, s);

		// Call copy.
		emit_load( ACC, 0, S1, s);
		s << JAL; emit_method_ref( Object, copy, s);

		// Run init.
		emit_load( ACC, 4, S1, s);
		emit_jalr( ACC, s);
		emit_pop( S1, s);
	}
	else
	{
		emit_new( type_name);
		s << JAL; emit_init_ref( type_name, s);
	}
}

int new__class::get_temp_size() {
	return 1;
}

void isvoid_class::code(ostream &s) {
	e1->code( s);

	emit_not( ACC, ACC, s);
	/*
	int else_label = new_label();
	int end_label = new_label();

	emit_load_imm( T1, 0, s);
	emit_bne( ACC, T1, else_label);
	emit_load_bool( ACC, BoolConst(1), s);
	emit_branch( end_label, s);
	emit_label_def( else_label);
	emit_load_bool( ACC, BoolConst(0), s);
	emit_label_def( end_label, s);
	*/
}

int isvoid_class::get_temp_size() {
	return e1->get_temp_size();
}

void no_expr_class::code(ostream &s) {
	emit_load_imm( ACC, 0, s);
}

int no_expr_class::get_temp_size() {
	return 0;
}

void object_class::code(ostream &s) {
	lookup_var( name);
	emit_addiu( ACC, object_base_reg, object_offset << 2, s);
}

int object_class::get_temp_size() {
	return 0;
}

