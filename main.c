#include <stdio.h>
#include <string.h>

void noop(char *x, ...) { }

#define logf printf
#define debugf noop //printf
#define errf noop //printf

typedef unsigned char inde; //index used by string
typedef unsigned char val;
typedef unsigned int cmask; //candidate mask
typedef int error;
const cmask cmask_all = (cmask)0x1FF; //9 1s
const val val_invalid = 0;

cmask val_cmask(val v) { return 1 << (v-1); }
cmask clear_cmask(cmask c, val v) { return c & ~val_cmask(v); }
cmask set_cmask(cmask c, val v) { return c | val_cmask(v); }
cmask has_cmask(cmask c, val v) { return c & val_cmask(v); }
int n_cmask(cmask c)
{
  int n = 0;
  for(val i = 0; i < 9; i++)
    if(c & (1 << i)) n++;
  return n;
}
int next_cmask(cmask c, val v)
{
  int n = 0;
  for(val i = v-1; i < 9; i++)
    if(c & (1 << i)) return i+1;
  return 0;
}
inde xy_ind(inde x, inde y) { return y*9+x; }
int ind_col(inde i) { return i%9; }
int ind_row(inde i) { return i/9; }
int ind_box(inde i)
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
  for(val i = 0; i < 9; i++)
    if(c == (1 << i)) return i+1;
  return val_invalid;
}
int cmask_cands(cmask c)
{
  int n = 0;
  for(val i = 0; i < 9; i++)
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
  inde inds[9]; //per tile
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
void print_board(board *b);
void print_state(board *b);
int solve(board *b, error *err);
void print_board_depth(int depth, board *b);
void print_state_depth(int depth, board *b);
int solve_depth(int depth, int maxdepth, board *b, error *err);

void zero_board(board *b)
{
  for(inde i = 0; i < 9*9; i++)
  {
    b->tiles[i].v = val_invalid;
    b->tiles[i].cs = cmask_all;
  }
  for(int i = 0; i < 9; i++) //per group
  {
    for(int j = 0; j < 9; j++) //per tile/val interchangeably
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

int clear_tile_group_candidate_counts(int depth, cmask cs, val v, group *g, error *err)
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

int clear_group_tiles_candidates(int depth, val v, group *g, board *b, error *err)
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

int clear_group_tiles_candidates_exclude_box(int depth, val v, int ebox, group *g, board *b, error *err)
{
  int clears = 0;
  for(int i = 0; i < 9; i++)
  {
    int ind = g->inds[i];
    if(ind_box(ind) != ebox && has_cmask(b->tiles[ind].cs, v))
    {
      b->n_cands--;
      b->tiles[ind].cs = clear_cmask(b->tiles[ind].cs, v);
      if(b->tiles[ind].cs == 0) { errf("clearing last candidate (%d)\n", v); *err = -1; return 0; }

      group *row = &b->rows[ind_row(ind)];
      group *col = &b->cols[ind_col(ind)];
      group *box = &b->boxs[ind_box(ind)];

      char *c;
      //debugf("%d %d %d %d\n",ind,ind_row(ind),ind_col(ind),ind_box(ind));
      c = &row->ccounts[v-1]; if(*c >= 2) *c = *c-1; else { errf("removing last candidate (%d) for row %d\n",v,row->i); *err = -1; return 0; }
      c = &col->ccounts[v-1]; if(*c >= 2) *c = *c-1; else { errf("removing last candidate (%d) for col %d\n",v,col->i); *err = -1; return 0; }
      c = &box->ccounts[v-1]; if(*c >= 2) *c = *c-1; else { errf("removing last candidate (%d) for box %d\n",v,box->i); *err = -1; return 0; }

      clears++;
    }
  }
  return clears;
}

int stamp_val(int depth, inde ind, val v, board *b, error *err)
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
  clear_tile_group_candidate_counts(depth, tcs, v, row, err); if(*err) return 0;
  clear_tile_group_candidate_counts(depth, tcs, v, col, err); if(*err) return 0;
  clear_tile_group_candidate_counts(depth, tcs, v, box, err); if(*err) return 0;
  b->tiles[ind].cs = 0;
  b->n_cands -= cmask_cands(tcs);

  //mark group completeness
  if(!has_cmask(row->complete, v)) row->complete = set_cmask(row->complete, v); else { errf("row %d already has val %d\n",row->i,v); *err = -1; return 0; }
  if(!has_cmask(col->complete, v)) col->complete = set_cmask(col->complete, v); else { errf("col %d already has val %d\n",col->i,v); *err = -1; return 0; }
  if(!has_cmask(box->complete, v)) box->complete = set_cmask(box->complete, v); else { errf("box %d already has val %d\n",box->i,v); *err = -1; return 0; }

  //clear group candidates
  clear_group_tiles_candidates(depth, v, row, b, err); if(*err) return 0;
  clear_group_tiles_candidates(depth, v, col, b, err); if(*err) return 0;
  clear_group_tiles_candidates(depth, v, box, b, err); if(*err) return 0;

  return 1;
}

int find_tile_stamps(int depth, board *b, error *err)
{
  int stamps = 0;
  for(inde i = 0; i < 9*9; i++)
  {
    if(b->tiles[i].v == val_invalid)
    {
      val v = cmask_val(b->tiles[i].cs);
      if(v != val_invalid)
      {
        stamps += stamp_val(depth, i, v, b, err);
        if(*err) return 0;
      }
    }
  }
  return stamps;
}

int find_group_stamps(int depth, group *g, board *b, error *err)
{
  int stamps = 0;
  for(val i = 0; i < 9; i++)
  {
    if(g->ccounts[i] == 1)
    {
      for(int j = 0; j < 9; j++)
      {
        inde ind = g->inds[j];
        if(b->tiles[ind].cs & (1 << i))
        {
          stamps += stamp_val(depth, ind, i+1, b, err);
          if(*err) return 0;
        }
      }
    }
  }
  return stamps;
}

int find_box_clears(int depth, group *g, board *b, error *err)
{
  int clears = 0;
  for(val i = 0; i < 9; i++)
  {
    if(g->ccounts[i] == 2 || g->ccounts[i] == 3)
    {
      int n;
      val v = i+1;

      //rows
      n = 0;
      if(has_cmask(b->tiles[g->inds[0]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[1]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[2]].cs,v)) n++;
      if(n == g->ccounts[i])
      {
        //debugf("clearing %d from row %d because of box %d\n",v,ind_row(g->inds[0]),g->i);
        clears += clear_group_tiles_candidates_exclude_box(depth, v, g->i, &b->rows[ind_row(g->inds[0])], b, err); if(*err) return 0;
      }

      n = 0;
      if(has_cmask(b->tiles[g->inds[3]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[4]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[5]].cs,v)) n++;
      if(n == g->ccounts[i])
      {
        //debugf("clearing %d from row %d because of box %d\n",i,ind_row(g->inds[3]),g->i);
        clears += clear_group_tiles_candidates_exclude_box(depth, v, g->i, &b->rows[ind_row(g->inds[3])], b, err); if(*err) return 0;
      }

      n = 0;
      if(has_cmask(b->tiles[g->inds[6]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[7]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[8]].cs,v)) n++;
      if(n == g->ccounts[i])
      {
        //debugf("clearing %d from row %d because of box %d\n",i,ind_row(g->inds[6]),g->i);
        clears += clear_group_tiles_candidates_exclude_box(depth, v, g->i, &b->rows[ind_row(g->inds[6])], b, err); if(*err) return 0;
      }

      //cols
      n = 0;
      if(has_cmask(b->tiles[g->inds[0]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[3]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[6]].cs,v)) n++;
      if(n == g->ccounts[i])
      {
        //debugf("clearing %d from col %d because of box %d\n",i,ind_col(g->inds[0]),g->i);
        clears += clear_group_tiles_candidates_exclude_box(depth, v, g->i, &b->cols[ind_col(g->inds[0])], b, err); if(*err) return 0;
      }

      n = 0;
      if(has_cmask(b->tiles[g->inds[1]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[4]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[7]].cs,v)) n++;
      if(n == g->ccounts[i])
      {
        //debugf("clearing %d from col %d because of box %d\n",i,ind_col(g->inds[1]),g->i);
        clears += clear_group_tiles_candidates_exclude_box(depth, v, g->i, &b->cols[ind_col(g->inds[1])], b, err); if(*err) return 0;
      }

      n = 0;
      if(has_cmask(b->tiles[g->inds[2]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[5]].cs,v)) n++;
      if(has_cmask(b->tiles[g->inds[8]].cs,v)) n++;
      if(n == g->ccounts[i])
      {
        //debugf("clearing %d from col %d because of box %d\n",i,ind_col(g->inds[2]),g->i);
        clears += clear_group_tiles_candidates_exclude_box(depth, v, g->i, &b->cols[ind_col(g->inds[2])], b, err); if(*err) return 0;
      }
    }
  }
  return clears;
}

int find_stamps(int depth, board *b, error *err)
{
  int changes = 0;
  changes += find_tile_stamps(depth, b, err); if(*err) return 0;

  for(int i = 0; i < 9; i++) { changes += find_group_stamps(depth, &b->rows[i], b, err); if(*err) return 0; }
  for(int i = 0; i < 9; i++) { changes += find_group_stamps(depth, &b->cols[i], b, err); if(*err) return 0; }
  for(int i = 0; i < 9; i++) { changes += find_group_stamps(depth, &b->boxs[i], b, err); if(*err) return 0; }

  int oldchanges = changes;
  for(int i = 0; i < 9; i++) { changes += find_box_clears(depth, &b->boxs[i], b, err); if(*err) return 0; }
  //if(oldchanges != changes) debugf("eliminated %d candidates by box\n", changes-oldchanges);

  return changes;
}

int find_brute_tile_stamps(int depth, int maxdepth, int breadth, board *b, error *err)
{
  int stamps = 0;
  for(inde i = 0; i < 9*9; i++)
  {
    if(b->tiles[i].v == val_invalid && n_cmask(b->tiles[i].cs) == breadth)
    {
      //debugf("trying (%d,%d) with %d candidates\n",ind_col(i),ind_row(i),n_cmask(b->tiles[i].cs));
      val v = 0;
      while((v = next_cmask(b->tiles[i].cs, v+1)))
      {
        /*
        logf("giving %d @ (%d,%d) a go (depth %d)\n",v,ind_col(i),ind_row(i),depth);
        print_board_depth(depth, b);
        logf("\n");
        */

        error eerr = 0;
        board eb;
        memcpy(&eb,b,sizeof(board));

        stamps += stamp_val(depth+1, i, v, &eb, &eerr);
        if(eerr) continue;

        stamps += solve_depth(depth+1, maxdepth, &eb, &eerr);
        if(eerr) continue;

        memcpy(b,&eb,sizeof(board));
        return stamps;
      }

    }
  }
  return stamps;
}

int find_brute_group_stamps(int depth, int maxdepth, int breadth, group *g, board *b, error *err)
{
  int stamps = 0;
  for(val i = 0; i < 9; i++)
  {
    if(g->ccounts[i] == breadth)
    {
      for(int j = 0; j < 9; j++)
      {
        inde ind = g->inds[j];
        if(b->tiles[ind].cs & (1 << i))
        {
          val v = i+1;
          /*
          logf("giving %d @ (%d,%d) a go (depth %d)\n",v,ind_col(i),ind_row(i),depth);
          print_board_depth(depth, b);
          logf("\n");
          */

          error eerr = 0;
          board eb;
          memcpy(&eb,b,sizeof(board));

          stamps += stamp_val(depth+1, ind, v, &eb, &eerr);
          if(eerr) continue;

          stamps += solve_depth(depth+1, maxdepth, &eb, &eerr);
          if(eerr) continue;

          memcpy(b,&eb,sizeof(board));
          return stamps;
        }
      }
    }
  }
  return stamps;
}

int brute_stamps(int depth, int maxdepth, int breadth, board *b, error *err)
{
  if(depth >= maxdepth) { errf("depth greater than %d\n", maxdepth); *err = -1; return 0; }

  int changes = 0;
  changes += find_brute_tile_stamps(depth, maxdepth, breadth, b, err); if(*err) return 0;
  if(changes) return changes; //early exit to see if lower hanging derivations

  for(int i = 0; i < 9; i++) { changes += find_brute_group_stamps(depth, maxdepth, breadth, &b->rows[i], b, err); if(*err) return 0; }
  if(changes) return changes; //early exit to see if lower hanging derivations
  for(int i = 0; i < 9; i++) { changes += find_brute_group_stamps(depth, maxdepth, breadth, &b->cols[i], b, err); if(*err) return 0; }
  if(changes) return changes; //early exit to see if lower hanging derivations
  for(int i = 0; i < 9; i++) { changes += find_brute_group_stamps(depth, maxdepth, breadth, &b->boxs[i], b, err); if(*err) return 0; }
  if(changes) return changes; //early exit to see if lower hanging derivations

  return changes;
}

int solve_derive(int depth, board *b, error *err)
{
  int changes = 0;
  int found_changes = 0;
  while((found_changes = find_stamps(depth, b, err)))
  {
    changes += found_changes;
    //print_state(b);
  }
  if(*err) return 0;

  return changes;
}

int solve_depth(int depth, int maxdepth, board *b, error *err)
{
  int changes = solve_derive(depth, b, err);
  if(*err) return 0;

  if(b->n_vals < 9*9)
  {
    changes += brute_stamps(depth, maxdepth, 2, b, err);
    if(*err) return 0;
  }

  return changes;
}
int solve(board *b, error *err)
{
  int depth = 0;
  int maxdepth = 5;
  int maxmaxdepth = 10;

  int changes = solve_derive(depth, b, err);
  if(*err) return 0;

  while(b->n_vals < 9*9 &&  maxdepth < maxmaxdepth)
  {
    changes += brute_stamps(depth, maxdepth, 2, b, err);
    if(*err) return 0;
  }

  return changes;
}

int consume_tiles(char *tiles, board *b, error *err)
{
  int i = 0;
  inde ind = 0;
  while(tiles[i])
  {
    if(tiles[i] == ' ' || tiles[i] == '\n')
      ; //ignore
    else
    {
      val v = char_val(tiles[i]);
      if(v != val_invalid)
      {
        if(stamp_val(0, ind, char_val(tiles[i]), b, err))
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

int consume_tight_tiles(char *tiles, board *b, error *err)
{
  for(int i = 0; i < 9; i++)
  {
    for(int j = 0; j < 9; j++)
    {
      inde ti = xy_ind(j,i);
      char rc = tiles[i*9+j];
      val v = char_val(rc);
      if(v != val_invalid)
        if(stamp_val(0, ti, char_val(rc), b, err)) if(*err) return 0;
    }
  }
  return 1;
}

void print_line()
{
  logf("------------------------------\n");
}

void print_board_depth(int depth, board *b)
{
  //logf("(%d vals, %d candidates)\n", b->n_vals, b->n_cands);
  for(int i = 0; i < 9; i++)
  {
    for(int j = 0; j < 9; j++)
    {
      inde ti = xy_ind(j,i);
      //debugf("%d",ind_box(ti)); //verifies ind->box conversion
      if(b->tiles[ti].v == val_invalid)
        logf(".");
      else
        logf("%d",b->tiles[ti].v);
      if((j+1)%3 == 0) logf(" ");
    }
    for(int j = 0; j < depth; j++)
      printf("-");
    logf("\n");
    if(i != 8)
    {
      if((i+1)%3 == 0) logf("\n");
    }
  }
}
void print_board(board *b) { print_board_depth(0, b); }

void print_board_cmasks_depth(int depth, board *b)
{
  for(int i = 0; i < 9; i++)
  {
    for(int ci = 0; ci < 3; ci++)
    {
      for(int j = 0; j < 9; j++)
      {
        inde ti = xy_ind(j,i);
        for(int cj = 0; cj < 3; cj++)
        {
          int c = (ci*3+cj)+1;
          if(b->tiles[ti].v == c || has_cmask(b->tiles[ti].cs,c))
            logf("%d",c);
          else
            logf(".");
        }
        logf(" ");
        if((j+1)%3 == 0) logf(" ");
      }
      if(i != 8 || ci != 2)
      {
        logf("\n");
        if(ci == 2 && (i+1)%3 == 0) logf("\n");
      }
    }
    if(i != 8) logf("\n");
  }
}
void print_board_cmasks(board *b) { print_board_cmasks_depth(0, b); }

void print_state_depth(int depth, board *b)
{
  print_line();
  print_board_depth(depth, b);
  logf("\n");
  print_board_cmasks_depth(depth, b);
  logf("\n");
  print_line();
}
void print_state(board *b) { print_state_depth(0, b); }

int main(int argc, char **argv)
{
  error err = 0;
  board b;

  zero_board(&b);

  char *tiles =

/*//blank
"... ... ..."
"... ... ..."
"... ... ..."

"... ... ..."
"... ... ..."
"... ... ..."

"... ... ..."
"... ... ..."
"... ... ...";
//*/

/*//nytimes 1/18/23 easy
"..9 .3. 658"
".83 27. ..."
".45 ..9 ..3"

"5.6 3.8 ..."
"2.. 7.. .39"
"... 1.. 5.4"

".78 .64 31."
"4.. .2. ..5"
"35. .1. 48.";
//*/

/*//nytimes 1/18/23 med
".42 ... 3.."
"... .46 7.."
"..1 9.. .2."

".1. .6. .9."
".3. .9. 2.."
"..7 3.. ..5"

"..6 .8. .5."
"... ... ..."
"72. ... ...";
//*/

//*//nytimes 1/18/23 hard
"... ..7 .3."
"... .5. 2.."
".36 ... .7."

"5.. 7.. ..."
".47 .6. ..8"
"2.. ... ..9"

"9.1 ..6 .87"
"... 8.. ..4"
"... 5.. ...";
//*/

  consume_tiles(tiles, &b, &err);
  if(err) { logf("error consuming\n"); return 1; }

  logf("before: (%d vals, %d candidates)\n", b.n_vals, b.n_cands);
  print_board(&b);
  logf("\n");

  solve(&b, &err);
  if(err) { logf("error solving\n"); return 1; }

  logf("after: (%d vals, %d candidates)\n", b.n_vals, b.n_cands);
  print_board(&b);
  logf("\n");

  return 0;
}

