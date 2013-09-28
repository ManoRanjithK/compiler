#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "stringtab.h"

static int ascii = 0;

void ascii_mode(ostream& str)
{
  if (!ascii)
    {
      str << "\t.ascii\t\"";
      ascii = 1;
    }
}

void byte_mode(ostream& str)
{
  if (ascii)
    {
      str << "\"\n";
      ascii = 0;
    }
}

void emit_string_constant(ostream& str, char* s)
{
  ascii = 0;

  while (*s) {
    switch (*s) {
    case '\n':
      ascii_mode(str);
      str << "\\n";
      break;
    case '\t':
      ascii_mode(str);
      str << "\\t";
      break;
    case '\\':
      byte_mode(str);
      str << "\t.byte\t" << (int) ((unsigned char) '\\') << endl;
      break;
    case '"' :
      ascii_mode(str);
      str << "\\\"";
      break;
    default:
      if (*s >= ' ' && ((unsigned char) *s) < 128)
	{
	  ascii_mode(str);
	  str << *s;
	}
      else
	{
	  byte_mode(str);
	  str << "\t.byte\t" << (int) ((unsigned char) *s) << endl;
	}
      break;
    }
    s++;
  }
  byte_mode(str);
  str << "\t.byte\t0\t" << endl;
}

int new_label()
{
	static int label_count = 0;
	return label_count++;
}

static int temp_offset = 0;
void init_alloc_temp()
{
	temp_offset = 0;
}

int alloc_temp()
{
	return temp_offset++;
}

#include <vector>
#include <utility>
#include <algorithm>

static std::vector< std::pair< std::pair< int, int>, int> > vec;
static std::vector< std::pair< std::pair< int, int>, int> >::iterator vec_leg;
void clear_vec()
{
	vec.clear();
}

void push_vec( int x, int y, int c)
{
	vec.push_back( std::make_pair( std::make_pair( x, y), c));
}

bool sort_cmp( const std::pair< std::pair< int, int>, int> &a,
		const std::pair< std::pair< int, int>, int> &b)
{
	return a.first.first >= b.first.first && a.first.second <= b.first.second;
}

void sort_vec()
{
	sort( vec.begin(), vec.end(), sort_cmp);
}

void init_vec()
{
	vec_leg = vec.begin();
}

bool next_vec()
{
	return vec_leg != vec.end();
}

void fetch_vec( int &x, int &y, int &c)
{
	x = vec_leg->first.first;
	y = vec_leg->first.second;
	c = vec_leg->second;
	vec_leg++;
}
