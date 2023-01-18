#include <stdio.h>

#define debugf printf
#define errf printf

typedef unsigned char index;
typedef unsigned char val;
typedef unsigned int cmask; //candidate mask
typedef int error;
const cmask cmask_all = (cmask)0x1FF; //9 1s
const val val_invalid = 0;

cmask val_cmask(val v) { return 1 << (v-1); }
cmask clear_cmask(cmask c, val v) { return c & ~val_cmask(v); }
cmask set_cmask(cmask c, val v) { return c | val_cmask(v); }
cmask has_cmask(cmask c, val v) { return c & val_cmask(v); }
index xy_ind(index x, index y) { return y*9+x; }
int ind_col(index i) { return i%9; }
int ind_row(index i) { return i/9; }
int ind_box(index i)
{
  int ix = ind_col(i);
  int iy = ind_row(i);
  return (ix/3)+(iy/3)*3;
}
val char_val(char c)
{
  if(c >= '1' && c <= '9') return c-'0';
  return val_invalid;
}
val cmask_val(cmask c)
{
  for(int i = 0; i < 9; i++)
    if(c == (1 << i)) return i+1;
  return val_invalid;
}
int cmask_cands(cmask c)
{
  int n = 0;
  for(int i = 0; i < 9; i++)
    if(c & (1 << i)) n++;
  return n;
}

typedef struct
{
  val v;
  cmask cs;
} tile;

typedef struct
{
  int i;
  char ccounts[9]; //per value
  index inds[9]; //per tile
  cmask complete;
} group;

typedef struct
{
  tile tiles[9*9];
  group rows[9];
  group cols[9];
  group boxs[9];
  int n_vals;
  int n_cands;
} board;
void print_state(board *b);

void zero_board(board *b)
{
  for(int i = 0; i < 9*9; i++)
  {
    b->tiles[i].v = val_invalid;
    b->tiles[i].cs = cmask_all;
  }
  for(int i = 0; i < 9; i++)
  {
    for(int j = 0; j < 9; j++)
    {
      b->rows[i].i = i;
      b->rows[i].ccounts[j] = 9;
      b->rows[i].inds[j] = xy_ind(j,i);
      b->rows[i].complete = 0;

      b->cols[i].i = i;
      b->cols[i].ccounts[j] = 9;
      b->cols[i].inds[j] = xy_ind(i,j);
      b->cols[i].complete = 0;

      b->boxs[i].i = i;
      b->boxs[i].ccounts[j] = 9;
      b->boxs[i].inds[j] = xy_ind(i%3*3+j%3,i/3*3+j/3);
      b->boxs[i].complete = 0;
    }
  }
  b->n_vals = 0;
  b->n_cands = 9*9*9;
}

int clear_tile_group_candidate_counts(cmask cs, val v, group *g, error *err)
{
  int clears = 0;
  for(val i = 0; i < 9; i++)
  {
    if(cs & (1 << i))
    {
      if(i != (v-1) && g->ccounts[i] < 2) { errf("removing last candidate for group\n"); *err = -1; return 0; }
      g->ccounts[i]--;
      clears++;
    }
  }
  return clears;
}

int clear_group_tiles_candidates(val v, group *g, board *b, error *err)
{
  int clears = 0;
  for(int i = 0; i < 9; i++)
  {
    int ind = g->inds[i];
    if(has_cmask(b->tiles[ind].cs, v))
    {
      b->n_cands--;
      b->tiles[ind].cs = clear_cmask(b->tiles[ind].cs, v);
      if(b->tiles[ind].cs == 0) { errf("clearing last candidate\n"); *err = -1; return 0; }

      group *row = &b->rows[ind_row(ind)];
      group *col = &b->cols[ind_col(ind)];
      group *box = &b->boxs[ind_box(ind)];

      char *c;
      //debugf("%d %d %d %d\n",ind,ind_row(ind),ind_col(ind),ind_box(ind));
      c = &row->ccounts[v-1]; if(has_cmask(row->complete, v) || *c >= 2) *c = *c-1; else { errf("removing last candidate (%d) for row %d\n",v,row->i); *err = -1; return 0; }
      c = &col->ccounts[v-1]; if(has_cmask(col->complete, v) || *c >= 2) *c = *c-1; else { errf("removing last candidate (%d) for col %d\n",v,col->i); *err = -1; return 0; }
      c = &box->ccounts[v-1]; if(has_cmask(box->complete, v) || *c >= 2) *c = *c-1; else { errf("removing last candidate (%d) for box %d\n",v,box->i); *err = -1; return 0; }

      clears++;
    }
  }
  return clears;
}

int stamp_val(index ind, val v, board *b, error *err)
{
  group *row = &b->rows[ind_row(ind)];
  group *col = &b->cols[ind_col(ind)];
  group *box = &b->boxs[ind_box(ind)];

  //stamp
  if(b->tiles[ind].v != val_invalid) { errf("already set\n"); *err = -1; return 0; }
  b->tiles[ind].v = v;
  b->n_vals++;

  //clear tile candidates
  cmask tcs = b->tiles[ind].cs;
  if(!has_cmask(tcs, v)) { errf("not a candidate\n"); *err = -1; return 0; }
  clear_tile_group_candidate_counts(tcs, v, row, err); if(*err) return 0;
  clear_tile_group_candidate_counts(tcs, v, col, err); if(*err) return 0;
  clear_tile_group_candidate_counts(tcs, v, box, err); if(*err) return 0;
  b->tiles[ind].cs = 0;
  b->n_cands -= cmask_cands(tcs);

  //mark group completeness
  if(!has_cmask(row->complete, v)) row->complete = set_cmask(row->complete, v); else { errf("row %d already has val %d\n",row->i,v); *err = -1; return 0; }
  if(!has_cmask(col->complete, v)) col->complete = set_cmask(col->complete, v); else { errf("col %d already has val %d\n",col->i,v); *err = -1; return 0; }
  if(!has_cmask(box->complete, v)) box->complete = set_cmask(box->complete, v); else { errf("box %d already has val %d\n",box->i,v); *err = -1; return 0; }

  //clear group candidates
  clear_group_tiles_candidates(v, row, b, err); if(*err) return 0;
  clear_group_tiles_candidates(v, col, b, err); if(*err) return 0;
  clear_group_tiles_candidates(v, box, b, err); if(*err) return 0;

  return 1;
}

int find_tile_stamps(board *b, error *err)
{
  int stamps = 0;
  for(index i = 0; i < 9*9; i++)
  {
    if(b->tiles[i].v == val_invalid)
    {
      val v = cmask_val(b->tiles[i].cs);
      if(v != val_invalid)
      {
        stamps += stamp_val(i, v, b, err);
        if(*err) return 0;
      }
    }
  }
  return stamps;
}

int find_group_stamps(group *g, board *b, error *err)
{
  int stamps = 0;
  for(int i = 0; i < 9; i++)
  {
    if(g->ccounts[i] == 1)
    {
      for(int j = 0; j < 9; j++)
      {
        index ind = g->inds[j];
        if(b->tiles[ind].cs & (1 << i))
        {
          stamps += stamp_val(ind, i+1, b, err);
          if(*err) return 0;
        }
      }
    }
  }
  return stamps;
}

int find_stamps(board *b, error *err)
{
  int stamps = 0;
  if(find_tile_stamps(b, err)) if(*err) return 0;

  for(int i = 0; i < 9; i++) { stamps += find_group_stamps(&b->rows[i], b, err); if(*err) return 0; }
  for(int i = 0; i < 9; i++) { stamps += find_group_stamps(&b->cols[i], b, err); if(*err) return 0; }
  for(int i = 0; i < 9; i++) { stamps += find_group_stamps(&b->boxs[i], b, err); if(*err) return 0; }

  return stamps;
}

int solve(board *b, error *err)
{
  while(find_stamps(b, err))
  {
    if(*err) return 0;
    print_state(b);
  }
  return 1;
}

int consume_rows(char *rows, board *b, error *err)
{
  int i = 0;
  index ind = 0;
  while(rows[i])
  {
    if(rows[i] == ' ' || rows[i] == '\n')
      ; //ignore
    else
    {
      val v = char_val(rows[i]);
      if(v != val_invalid)
      {
        if(stamp_val(ind, char_val(rows[i]), b, err))
        {
          //print_state(b);
          if(*err) return 0;
        }
      }
      ind++;
    }
    i++;
  }
  return 1;
}

int consume_tight_rows(char *rows, board *b, error *err)
{
  for(int i = 0; i < 9; i++)
  {
    for(int j = 0; j < 9; j++)
    {
      index ti = xy_ind(j,i);
      char rc = rows[i*9+j];
      val v = char_val(rc);
      if(v != val_invalid)
        if(stamp_val(ti, char_val(rc), b, err)) if(*err) return 0;
    }
  }
  return 1;
}

void print_line()
{
  printf("------------------------------\n");
}

void print_board(board *b)
{
  for(int i = 0; i < 9; i++)
  {
    for(int j = 0; j < 9; j++)
    {
      index ti = xy_ind(j,i);
      //debugf("%d",ind_box(ti)); //verifies ind->box conversion
      if(b->tiles[ti].v == val_invalid)
        printf(".");
      else
        printf("%d",b->tiles[ti].v);
      if((j+1)%3 == 0) printf(" ");
    }
      printf("\n");
    if(i != 8)
    {
      if((i+1)%3 == 0) printf("\n");
    }
  }
}

void print_board_cmasks(board *b)
{
  for(int i = 0; i < 9; i++)
  {
    for(int ci = 0; ci < 3; ci++)
    {
      for(int j = 0; j < 9; j++)
      {
        index ti = xy_ind(j,i);
        for(int cj = 0; cj < 3; cj++)
        {
          int c = (ci*3+cj)+1;
          if(b->tiles[ti].v == c || has_cmask(b->tiles[ti].cs,c))
            printf("%d",c);
          else
            printf(".");
        }
        printf(" ");
        if((j+1)%3 == 0) printf(" ");
      }
      if(i != 8 || ci != 2)
      {
        printf("\n");
        if(ci == 2 && (i+1)%3 == 0) printf("\n");
      }
    }
    if(i != 8) printf("\n");
  }
}

void print_state(board *b)
{
  print_line();
  print_board(b);
  printf("\n");
  print_board_cmasks(b);
  printf("\n");
  print_line();
}

int main(int argc, char **argv)
{
  error err = 0;
  board b;

  zero_board(&b);

/*
  char rows[9*9] =
  "001000000"
  "000000000"
  "000000000"
  "000000000"
  "000000000"
  "000000000"
  "000000000"
  "000000000"
  "000000000";
*/

  char *rows =
"... ..7 .3. "
"... .5. 2.. "
".36 ... .7. "

"5.. 7.. ... "
".47 .6. ..8 "
"2.. ... ..9 "

"9.1 ..6 .87 "
"... 8.. ..4 "
"... 5.. ...";

  consume_rows(rows, &b, &err);
  if(err) { printf("error consuming\n"); return 1; }

  printf("before: (%d vals, %d candidates)\n", b.n_vals, b.n_cands);
  print_board(&b);
  printf("\n");

  solve(&b, &err);
  if(err) { printf("error solving\n"); return 1; }

  printf("after: (%d vals, %d candidates)\n", b.n_vals, b.n_cands);
  print_board(&b);
  printf("\n");
  print_board_cmasks(&b);
  printf("\n");

  return 0;
}

